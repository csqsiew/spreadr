#' Spread of activation process to determine activation of network nodes.
#'
#' This function takes in a dataframe with 'node' and 'activation' columns, and
#' simulates the spread of activation among nodes in a specified network
#' structure.
#'
#' @param network Network where the spreading occurs. Must be specified. Must be
#'   an igraph object with a "name" property or an adjacency matrix.
#' @param start_run A non-empty data.frame. The data.frame must have 'node' and
#'   'activation' columns. Optionally, it may have a 'time' column. For each
#'   iteration of the simulation, if the simulation's time point matches the
#'   'time' column of any row, then the node specified by the 'node' column will
#'   receive additional activation given by the value in the 'activation'
#'   column. If the 'time' column is not specified, then 'time' is assumed to
#'   be 0 (starting state of the simulation).
#' @param decay Proportion of activation that is lost at each time step. Ranges
#'   from 0 to 1. Default is 0.
#' @param retention Proportion of activation that remains in the node from which
#'   activation is being spread from. Ranges from 0 to 1. Default is 0.5.
#' @param suppress Suppress nodes with a final activation of < x units at each
#'   time step to an activation value of 0. Default is 0.
#' @param time If not \code{NULL}, the number of time steps to simulate.
#' @param threshold_to_stop If not \code{NULL}, stop the simulation only when
#'   there is at least one node with activation value less than
#'   \code{threshold_to_stop}.
#' @param create_names Name nodes 1:number_of_nodes in case network is missing
#'   node names.
#'
#' @return A compiled dataframe with 'node', 'activation' and 'time' columns
#'   showing the spread of activation in the network over time.
#'
#' @details At least one of \code{time} or \code{threshold_to_stop} must be
#'   non-\code{NULL}. If both are non-\code{NULL}, the simulation stops at the
#'   earliest time possible.
#'
#'   The simulation iterates like so:
#'   \itemize{
#'     \item{t = 1, add}{activations in start_run with t = 1 are added to the
#'       network.}
#'     \item{t = 1, spread}{spreading activation from node to node.}
#'     \item{t = 1, decay}{activation in each node decays.}
#'     \item{t = 1, suppress}{nodes with activations less than the suppress
#'       value are given activation = 0.}
#'     \item{t = 1, record}{activations in the nodes are recorded for output.}
#'     \item{t = 1, check}{the termination conditions, time and
#'       threshold_to_stop, are checked; the simulation stops immediately if
#'       either is satisfied.}
#'   }
#'   with t = 2 being the same as t = 1, but substitute 1 for 2.
#'
#' @examples
#' g_d_mat <- matrix(sample(c(0,1), 100, replace = TRUE), 10, 10)
#' # make an adjacency matrix and randomly fill some cells with 1s
#' diag(g_d_mat) <- 0 # remove self-loops
#' library(spreadr)
#' initial_df <- data.frame(node = 1, activation = 20, stringsAsFactors = FALSE)
#' result_d <- spreadr(start_run = initial_df, decay = 0, retention = 0.5,
#' suppress = 0, network = g_d_mat, time = 10)
#' head(result_d, 10)
#' tail(result_d, 10)
#'
#' @importFrom assertthat assert_that is.count has_name
#' @importFrom igraph as_adjacency_matrix is.igraph V V<-
#' @importFrom Rcpp sourceCpp
#' @useDynLib spreadr
#'
#' @export
spreadr <- function(
    network, start_run, retention=0.5, time=10, threshold_to_stop=NULL,
    decay=0, suppress=0, create_names=TRUE
  ){

  # is network an igraph or square matrix-like object?
  assert_that(is.igraph(network) ||
    length(dim(network)) == 2 && dim(network)[1] == dim(network)[2])
  # is start_run in the correct format?
  assert_that(is.data.frame(start_run))
  assert_that(has_name(start_run, "node"), has_name(start_run, "activation"))
  assert_that(  # time is non-negative integer, if it is provided
    !has_name(start_run, "time") ||
      all(sapply(start_run$time, function(x) x == 0 || is.count(x))))
  # is decay is a number between 0 and 1 inclusive?
  assert_that(decay >= 0 && decay <= 1)
  # is retention is a number between 0 and 1 inclusive?
  assert_that(retention >= 0 && retention <= 1)
  # are terminating conditions ok? (time and threshold_to_stop)
  assert_that(
    !(is.null(time) && is.null(threshold_to_stop)),
    msg="time and threshold_to_stop cannot both be NULL")
  assert_that(is.null(time) || is.count(time))
  assert_that(is.null(threshold_to_stop) || threshold_to_stop > 0)
  # are node names ok?
  does_network_have_names <- if (is.igraph(network))
    !is.null(V(network)$name) else
    !is.null(colnames(network)) || !is.null(rownames(network))
  assert_that(create_names || does_network_have_names)

  # honour create_names: if TRUE, add node names
  if (create_names && !does_network_have_names && is.igraph(network)) {
    V(network)$name <- 1:length(V(network))
  } else if (create_names && !does_network_have_names && !is.igraph(network)) {
    colnames(network) <- 1:ncol(network)
    rownames(network) <- 1:nrow(network)
  }

  # we work with adjacency matrices, so if network is.igraph, convert to an
  # adjacency matrix
  if (is.igraph(network)) network <- as_adjacency_matrix(network)
  assert_that(all(colnames(network) == rownames(network)))

  # variables in the loop:
  # -   d :: numeric integer vector, the degree of each node
  # - a_t :: numeric vector, the activation at time t
  # - mat :: base::matrix version of network
  d <- Matrix::rowSums(network)
  n_nodes <- length(d)
  a_t <- rep(0, n_nodes) # empty vector
  mat <- as.matrix(network)

  # pre-loop set-up: if !has_name(start_run, "time"), start_run specifies the
  # activation value of certain nodes at t=0. Otherwise, start_run specifies
  # when to add how much activation to certain nodes, so check for activations
  # to be added at t=0.
  if (!has_name(start_run, "time")) start_run$time <- 1

  current_time <- 1
  activations <- numeric(0)
  while (TRUE) {
    # apply start_run instructions
    start_run_t <- subset(start_run, time == current_time)
    for (i in 1:nrow(start_run_t)) {
      j <- which(colnames(network) == start_run_t[i, "node"])
      a_t[j] <- a_t[j] + start_run_t[i, "activation"]
    }

    # spreading activation
    a_tm1 <- a_t
    mat_t <- create_mat_t(mat, a_tm1, d, retention)
    # create_mat_t creates an "activation matrix", where each (i, j) entry is
    # the activation at time t, at node j, due to node i.
    a_t <- colSums(mat_t)

    # decay and suppress
    a_t <- (1-decay) * a_t
    a_t[a_t < suppress] <- 0

    # record
    activations <- c(activations, a_t)

    # check termination
    if (!is.null(time) && current_time >= time) break
    if (!is.null(threshold_to_stop) && any(a_t < threshold_to_stop)) break

    current_time <- current_time + 1
  }

  nodes <- rep(colnames(network), current_time)
  is <- rep(1:current_time, rep(n_nodes, current_time))

  data.frame('node' = nodes, # spread_fast does not include t=0
             'activation' = activations,
             'time' = is)
}
