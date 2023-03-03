# See https://github.com/csqsiew/spreadr/issues/4.

library(igraph)

test_that("Weighted graph, when represented by an igraph, works", {
  weighted_network <- graph_from_literal(a-b, a-c)
  E(weighted_network)$weight <- c(1, 9)
  results <- spreadr(
    weighted_network, data.frame(node="a", activation=10),
    time=1, retention=0, include_t0=TRUE)
  expected <- data.frame(
    node=as.factor(rep(c("a", "b", "c"), 2)),
    activation=c(10, 0, 0, 0, 1, 9),
    time=c(rep(0, 3), rep(1, 3)))
  expect_equal(results, expected)
})
