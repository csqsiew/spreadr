#' Spread of activation process to determine activation of network nodes.
#'
#' This function takes in a dataframe with 'node' and 'activation' columns, and
#' simulates the spread of activation among nodes in a specified network structure.
#' Note that spreadr_2 is identical to spreadr, except that there is an additonal option for the user to 
#'
#' @param start_run A non-empty dataframe with 'node' and 'activation' columns. Must be specified.
#' @param decay Proportion of activation that is lost at each time step. Ranges from 0 to 1. Default is 0.
#' @param retention Proportion of activation that remains in the node from which activation is being spread from. Ranges from 0 to 1. Default is 0.5.
#' @param suppress Suppress nodes with a final activation of < x units at each time step to an activation value of 0. Default is 0.
#' @param network Network where the spreading occurs. Must be specified. Must be an igraph object with a "name" property or an adjacency matrix.
#' @param time Number of time steps to run the spreadr() function for. Default is 10.
#' @param ignore_time Indicate whether to ignore the number of time steps listed and allow for the spreading activation process to continue until the value listed in threshold_to_stop is achieved. Default = F
#' @param threshold_to_stop Stop spreading activation when the proportion of total activation in start_run is below the threshold. Default = 0.01
#' @param create_names Name nodes 1:number_of_nodes in case network is missing node names.
#' @return A compiled dataframe with 'node', 'activation' and 'time' columns showing the spread of activation in the network over time.
#' @examples
#' g_d_mat <- matrix(sample(c(0,1), 100, replace = TRUE), 10, 10) 
#' # make an adjacency matrix and randomly fill some cells with 1s 
#' diag(g_d_mat) <- 0 # remove self-loops 
#' library(spreadr)
#' initial_df <- data.frame(node = 1, activation = 20, stringsAsFactors = FALSE)
#' result_d <- spreadr_2(start_run = initial_df, decay = 0, retention = 0.5, 
#' suppress = 0, network = g_d_mat, ignore_time = TRUE, threshold_to_stop = 0.01) 
#' head(result_d, 10)
#' tail(result_d, 10)
#' @export


spreadr_2 <- function(
    network, # an `igraph` network object or an adjacency matrix
    start_run, # data.frame 'initial_df' with node and activation columns to specify activation at t = 0
    retention = 0.5, # retention parameter
    time = 10, # number of time steps
    decay = 0, # decay paramater
    suppress = 0, # suppress parameter,
    ignore_time = F, 
    threshold_to_stop = 0.01, # % of total activation in start_run as the threshold 
    create_names = TRUE # name nodes 1:size if needed
){
    ### ERROR MESSAGES ###
    # check if start_run is in the correct format
    if (is.data.frame(start_run) == F || colnames(start_run) != c('node', 'activation')) {
        stop('Initial activation dataframe is not in the correct format. Must be a dataframe with -node-
             and -activation- columns.')
    }
    
    # check if the node column in start_run is a factor (if so, it must be converted to character class)
    if (is.factor(start_run$node) == T) {
        start_run$node <- as.character(start_run$node)
    }
    
    # check if decay is a number from 0 to 1
    if (decay < 0 || decay > 1) {
        stop('Decay value is not a number from 0 to 1.')
    }
    
    # check if retention is a number from 0 to 1
    if (retention < 0 || retention > 1) {
        stop('Retention value is not a number from 0 to 1.')
    }
    
    # check if time is a non-negative number
    if (time < 0 || is.numeric(time) == F) {
        stop('Something is off with the time value.')
    }
    
    # threshold_to_stop_2 <- threshold_to_stop/100*sum(start_run$activation)
    
    ### SET UP ###
    ## CONVERT IGRAPH TO ADJMAT IF NEEDED, GET DEGREE ##
    if(igraph::is.igraph(network) == T) { # if an igraph object
        
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
        if(igraph::is.weighted(network) == T) { # if graph is weighted
            mat<-igraph::as_adj(network, sparse = F, attr="weight") # to keep the weights in a weighted graph
            d <- colSums(mat) # GET THE DEGREE OF EACH NODE
            if(is.null(names(d))) names(d) <- 1:length(d) # names are simply column numbers
        } else {
            mat<-igraph::as_adj(network, sparse = F) # unweighted graph
            d <- colSums(mat) # GET THE DEGREE OF EACH NODE
            if(is.null(names(d))) names(d) <- 1:length(d) # names are simply column numbers
        }
    } else {
        mat <- network # input is already an adj matrix
        d <- colSums(mat) # GET THE DEGREE OF EACH NODE
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
    
    if(ignore_time == F) {
        ## REPEAT PROCESS FOR EACH TIME STEP
        activations = numeric(time * n_nodes)
        for (i in seq_len(time)){
        # ACTIVATION AT TIME T - 1
        a_tm1 <- a_t
        
        # # ACTIVATION MATRIX AT TIME T
        # mat_t <- apply(mat, 1, function(x) (1 - retention) * x * a_tm1/d) # this generalizes to weighted networks
        # mat_t[is.nan(mat_t)] <- 0 # in case there are hermits in the network, convert NaNs to 0
        # diag(mat_t) <- retention * a_tm1
        mat_t = create_mat_t(mat, a_tm1, d, retention)
        
        # UPDATED ACTIVATION VECTOR
        a_t <- colSums(mat_t)
        a_t <- a_t * (1 - decay)  # decay proportion of activation at the end of each time step
        a_t[a_t < suppress] <- 0 # reduce activation to 0 cells less than suppress parameter
        
        # STORE RESULTS
        activations[((i-1) * n_nodes + 1) : (i * n_nodes)] = a_t
    }
    
        # OTHER OUT STUFF
        nodes = rep(names(d), time) # keep original names of graph
        is = rep(1:time, rep(n_nodes, time))
    
        return(data.frame('node' = nodes, # spread_fast does not include t=0
                      'activation' = activations,
                      'time' = is))
    } else {
        ### while function 
        
        ## REPEAT PROCESS FOR EACH TIME STEP
        # activations = numeric(time * n_nodes)
        activations = numeric(n_nodes * 1000) # max number of time steps = 1000  
        t <- 0
        guy <- 1
        
        while(length(guy) > 0 && t < 1001) {
                
            # counter for time 
            t <- t + 1
            
            # ACTIVATION AT TIME T - 1
            a_tm1 <- a_t
                
            # # ACTIVATION MATRIX AT TIME T
            mat_t = create_mat_t(mat, a_tm1, d, retention)
                
            # UPDATED ACTIVATION VECTOR
            a_t <- colSums(mat_t)
            a_t <- a_t * (1 - decay)  # decay proportion of activation at the end of each time step
            a_t[a_t < suppress] <- 0 # reduce activation to 0 cells less than suppress parameter
                
            # check if threshold difference of two matrices are less than threshold 
            guy <- which(abs(a_t-a_tm1) > threshold_to_stop)
                
            # STORE RESULTS
            activations[((t-1) * n_nodes + 1) : (t * n_nodes)] = a_t
                
            }
            # OTHER OUT STUFF
            nodes = rep(names(d), t) # keep original names of graph
            is = rep(1:t, rep(n_nodes, t))
            return(data.frame('node' = nodes, # spread_fast does not include t=0
                          'activation' = activations[1:(t*n_nodes)],
                          'time' = is))
    }
    
    }

