# See https://github.com/csqsiew/spreadr/issues/7.

library(igraph)

test_that("error when edges are negative", {
  g <- graph_from_literal(1--2, 1--3, 1--4)
  E(g)$weight <- c(-1, -2, -1) # negative weights
  expect_snapshot_error(
    spreadr(g, data.frame(node = 1, activation = 100), time = 1)
  )
})
