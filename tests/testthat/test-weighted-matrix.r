# See https://github.com/csqsiew/spreadr/issues/4.

test_that("Weighted graph, when represented by a matrix, works", {
  weighted_network <- matrix(
    c(0, 1, 9,
      1, 0, 0,
      9, 0, 0), nrow=3, byrow=TRUE)
  colnames(weighted_network) <- c("a", "b", "c")
  rownames(weighted_network) <- c("a", "b", "c")
  results <- spreadr(
    weighted_network, data.frame(node="a", activation=10),
    time=1, retention=0, include_t0=TRUE)
  expected <- data.frame(
    node=as.factor(rep(c("a", "b", "c"), 2)),
    activation=c(10, 0, 0, 0, 1, 9),
    time=c(rep(0, 3), rep(1, 3)))
  expect_equal(results, expected)
})
