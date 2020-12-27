#' Spread of activation process to determine activation of network nodes.
#'
#' This function takes in a dataframe with 'node' and 'activation' columns, and
#' simulates the spread of activation among nodes in a specified network
#' structure.
#'
#' @param network Network where the spreading occurs. Must be specified. Must be
#'   an igraph object with a "name" property or an adjacency matrix.
#' @param start_run A non-empty dataframe with 'node' and 'activation' columns.
#'   Must be specified.
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
#' @importFrom assertthat assert_that is.count
#' @importFrom Rcpp sourceCpp
#' @useDynLib spreadr
#'
#' @export
spreadr <- function(
    network, start_run, retention=0.5, time=10, threshold_to_stop=NULL,
    decay=0, suppress=0, create_names = TRUE
  ){

  ### ERROR MESSAGES ###
  # check if start_run is in the correct format
  if (!is.data.frame(start_run) || !all(colnames(start_run) == c('node', 'activation'))) {
    stop('Initial activation dataframe is not in the correct format. Must be a dataframe with -node-
         and -activation- columns.')
  }
  # check if the node column in start_run is a factor (if so, it must be converted to character class)
  if (is.factor(start_run$node)) {
    start_run$node <- as.character(start_run$node)
  }
  # check that decay is a number between 0 and 1 inclusive
  assert_that(decay >= 0 && decay <= 1)
  # check that retention is a number between 0 and 1 inclusive
  assert_that(retention >= 0 && retention <= 1)
  # check terminating conditions, time and threshold_to_stop
  assert_that(
    !(is.null(time) && is.null(threshold_to_stop)),
    msg="time and threshold_to_stop cannot both be NULL")
  assert_that(is.null(time) || is.count(time))
  assert_that(is.null(threshold_to_stop) || threshold_to_stop > 0)

  ### SET UP ###
  ## CONVERT IGRAPH TO ADJMAT IF NEEDED, GET DEGREE ##
  if(igraph::is.igraph(network)) {

    # check if the igraph object has a name attribute
    if (is.null(igraph::V(network)$name) == T) {
      if(create_names == TRUE){
        igraph::V(network)$name = 1:length(igraph::V(network))
      } else {
        stop('Network does not have a "name" attribute.')
      }
    }

    # check if node labels are unique
    if (length(unique(igraph::V(network)$name)) != igraph::gorder(network)) {
      stop('Nodes need to have unique labels.')
    }
    if(igraph::is.weighted(network)) { # if graph is weighted
      mat <- igraph::as_adj(network, sparse = F, attr="weight") # to keep the weights in a weighted graph
      d <- rowSums(mat) # GET THE DEGREE OF EACH NODE
      if(is.null(names(d))) names(d) <- 1:length(d) # names are simply column numbers
    } else {
      mat <- igraph::as_adj(network, sparse = F) # unweighted graph
      d <- rowSums(mat) # GET THE DEGREE OF EACH NODE
      if(is.null(names(d))) names(d) <- 1:length(d) # names are simply column numbers
    }
  } else {
    mat <- network # input is already an adj matrix
    d <- rowSums(mat) # GET THE DEGREE OF EACH NODE
    names(d) <- 1:length(d) # names are simply column numbers
  }

  ### SET UP FOR SPREADING ACTIVATION T = 0 ###
  ## CREATE THE ACTIVATION VECTOR ##
  n_nodes = length(d)
  a <- rep(0, n_nodes) # empty vector
  names(a) <- names(d)
  a[start_run$node] <- start_run$activation # add the activation input in; multiple values ok

  ## OBJECT TO STORE RESULTS ##
  out_df <- data.frame(node = names(a), activation = a, time = 0L)

  ## ACTIVATION AT TIME T ##
  a_t <- a

  current_time <- 1
  activations <- numeric(0)
  while (TRUE) {
    a_tm1 <- a_t
    mat_t <- create_mat_t(mat, a_tm1, d, retention)

    # create_mat_t creates an "activation matrix", where each (i, j) entry is
    # the activation at time t, at node j, due to node i.
    a_t <- colSums(mat_t)
    a_t <- (1-decay) * a_t
    a_t[a_t < suppress] <- 0

    activations <- c(activations, a_t)

    if (!is.null(time) && current_time >= time) break
    if (!is.null(threshold_to_stop) && any(a_t < threshold_to_stop)) break

    current_time <- current_time + 1
  }

  # OTHER OUT STUFF
  nodes <- rep(names(d), current_time) # keep original names of graph
  is <- rep(1:current_time, rep(n_nodes, current_time))

  data.frame('node' = nodes, # spread_fast does not include t=0
             'activation' = activations,
             'time' = is)
}
