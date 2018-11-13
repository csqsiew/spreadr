## ---- eval=TRUE----------------------------------------------------------
options(stringsAsFactors = FALSE) # to prevent strings from being converted to a factor class
extrafont::loadfonts(quiet=TRUE)

# install.packages('devtools')
# library(devtools)
# install_github('csqsiew/spreadr') # download spreadr from my github page
library(spreadr)

## ---- warning=FALSE------------------------------------------------------
library(igraph)
set.seed(1)

g <- sample_gnp(20, 0.2, directed = F, loops = F) # make a random network
V(g)$name <- paste0('N', as.character(1:gorder(g))) # give some meaningful labels for the 'name' attribute

V(g)$color <- c('blue', rep('white', 19))
plot(g, l = layout_with_fr(g), vertex.size = 10, vertex.label.dist = 2, vertex.label.cex = 0.8)
# the blue node will be assigned activation at t = 0

## ------------------------------------------------------------------------
initial_df <- data.frame(node = 'N1', activation = 20, stringsAsFactors = F)
initial_df

## ------------------------------------------------------------------------
result <- spreadr::spreadr(start_run = initial_df, decay = 0,
                              retention = 0.5, suppress = 0,
                              network = g, time = 10)

## ------------------------------------------------------------------------
head(result) # view the results
# write.csv(result, file = 'result.csv') # save the results

library(ggplot2)
a1 <- data.frame(node = 'N1', activation = 20, time = 0) # add back initial activation at t = 0
result_t0 <- rbind(result,a1)
ggplot(data = result_t0, aes(x = time, y = activation, color = node, group = node)) +
  geom_point() + geom_line() # visualize the results

