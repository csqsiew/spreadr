#' Simulate spreading activation in a network
#'
#' @param network Adjacency matrix or
#'   \code{\link[igraph:igraph-package]{igraph}} object representing the network
#'   in which to simulate spreading activation.
#' @param start_run Non-empty \code{\link[base]{data.frame}} with mandatory
#'   columns \emph{node}, \emph{activation}; and optional columns \emph{time}.
#'   If the \emph{time} column is present, \emph{activation} is added to
#'   \emph{node} at each \emph{time}. Otherwise, the \emph{activation}s are
#'   added to their corresponding nodes at \code{t = 0}.
#' @param decay Number from 0 to 1 (inclusive) representing the proportion of
#'   activation that is lost at each time step.
#' @param retention Number from 0 to 1 (inclusive) or a numeric vector of such
#'   numbers of length equals number of nodes in the network. This represents
#'   the proportion of activation that remains in the node (not spread) at each
#'   time step. Then, \code{1 - retention} of the activation at each node is
#'   spread to neighbouring nodes. If a numeric vector, retentions are assigned
#'   to nodes according to the order given by \code{V(network)} if
#'   \code{network} is an \code{\link[igraph:igraph-package]{igraph}} object or
#'   \code{nrow(network)} if \code{network} is an adjacency matrix.
#' @param suppress Number representing the maximum amount of activation in a
#'   node for it to be set to 0, at each time step.
#' @param time Positive non-zero integer, or \code{NULL}. If not \code{NULL},
#'   the number of time steps to simulate before stopping. Otherwise,
#'   stop with the \code{threshold_to_stop} parameter.
#' @param threshold_to_stop Number or \code{NULL}. If not \code{NULL}, stop the
#'   simulation only when all nodes have activation value less than
#'   \code{threshold_to_stop}. Otherwise, stop with the \code{time} parameter.
#' @param include_t0 Boolean flag indicating if activation at \code{t = 0}
#'   should be prepended to the output \code{data.frame}. This is \code{FALSE}
#'   by default for back-compatibility.
#' @param create_names Boolean flag indicating if nodes should be automatically
#'   named (\code{1:n}, where \code{n} is the number of nodes) in case they are
#'   missing.
#' @param never_stop Boolean flag indicating if the simulation should be stopped
#'   if there have been too many iterations (so that there might be an infinite
#'   loop).
#'
#' @return A \code{\link[base]{data.frame}} with \emph{node}, \emph{activation}
#'   and \emph{time} columns representing the spread of activation in the network
#'   over time.
#'
#' @details At least one of parameters \code{time} or \code{threshold_to_stop}
#'   must be non-\code{NULL}. If both are non-\code{NULL}, the simulation stops
#'   at the earliest time possible.
#'
#'   The simulation iterates like so: for every \code{i} in \code{[0, time]},
#'   \itemize{
#'     \item{Spread activation from node to node}
#'     \item{Decay the activation at each node by the proportion specified by
#'       \code{decay}}
#'     \item{Set the activation at nodes with activation less than
#'       \code{suppress} to \code{0}}
#'     \item{Add the activations in \code{start_run} with \code{time = i} to
#'       their corresponding nodes}
#'     \item{Save the activations at each node for output}
#'     \item{Check the terminating conditions \code{time} and
#'       \code{threshold_to_stop}. If any are satisfied, terminate the
#'       simulation.}
#'   }
#'
#' @examples
#' # make an adjacency matrix and randomly fill some cells with 1s
#' mat <- matrix(sample(c(0,1), 100, replace=TRUE), 10, 10)
#' diag(mat) <- 0 # remove self-loops
#' initial_df <- data.frame(node=1, activation=20, stringsAsFactors=FALSE)
#' results <- spreadr(mat, initial_df)
#'
#' head(results, 10)
#' tail(results, 10)
#'
#' @importFrom assertthat assert_that is.count is.flag has_name
#' @importFrom igraph as_adjacency_matrix graph_from_adjacency_matrix is.igraph
#'   V V<-
#' @importFrom Rcpp sourceCpp
#' @useDynLib spreadr
#'
#' @export
spreadr <- function(
    network, start_run, retention=0.5, time=10, threshold_to_stop=NULL,
    decay=0, suppress=0, include_t0=FALSE, create_names=TRUE, never_stop=FALSE
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
  # is retention an appropriate number or numeric vector?
  assert_that(is.numeric(retention))
  n_nodes <- if (is.igraph(network)) length(V(network)) else nrow(network)
  assert_that(length(retention) == 1 || length(retention) == n_nodes)
  assert_that(all(retention >= 0 & retention <= 1))
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
  # is return_type ok?
  assert_that(is.flag(include_t0))

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

  # is there any node in start_run which does not exist in network?
  not_exist_idx <- which(!start_run$node %in% colnames(network))
  not_exist_node <- start_run$node[not_exist_idx]
  not_exist_node <- unique(not_exist_node)
  if (length(not_exist_node) > 0) warning(
    "These nodes specified in start_run don't exist in network: ",
    paste0(not_exist_node, collapse=", "))

  # it is easier if we assume retention is always a vector
  if (length(retention) == 1) retention <- rep(retention, n_nodes)

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
  if (!has_name(start_run, "time")) start_run$time <- 0

  current_time <- 0
  activations <- numeric(0)
  while (TRUE) {
    # spreading activation
    a_tm1 <- a_t
    mat_t <- create_mat_t(mat, a_tm1, d, retention)
    # create_mat_t creates an "activation matrix", where each (i, j) entry is
    # the activation at time t, at node j, due to node i.
    a_t <- colSums(mat_t)

    # decay and suppress
    a_t <- (1-decay) * a_t
    a_t[a_t < suppress] <- 0

    # apply start_run instructions
    start_run_t <- subset(start_run, time == current_time)
    for (i in 1:nrow(start_run_t)) {
      j <- which(colnames(network) == start_run_t[i, "node"])
      a_t[j] <- a_t[j] + start_run_t[i, "activation"]
    }

    # record
    activations <- c(activations, a_t)

    # check termination
    if (!is.null(time) && current_time >= time) break
    if (!is.null(threshold_to_stop) && all(a_t < threshold_to_stop)) break
    if (is.null(time) && decay == 0 && !never_stop && current_time > 10000) {
      warning(
        "Stopping because there might potentially be an infinite loop. ",
        "Set NEVER_STOP=TRUE to override.")
      break
    }

    current_time <- current_time + 1
  }

  nodes <- rep(colnames(network), current_time+1)
  is <- rep(0:(current_time), rep(n_nodes, current_time+1))
  return_df <- data.frame(
    node=nodes, activation=activations, time=is, stringsAsFactors=TRUE)

  if (!include_t0) {
    return_df <- subset(return_df, time != 0)
    if (nrow(return_df) != 0) rownames(return_df) <- 1:nrow(return_df)
  }

  return_df
}
