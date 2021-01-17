test_that("GitHub issue 1 is fixed", {
  # See: https://github.com/csqsiew/spreadr/issues/1. Note that the input
  # adjacency matrix is different here. When the issue was written, the input
  # adjacency matrix had the (i, j) entry representing the edge from j to i
  # Now, the (i, j) entry represents the edge from i to j instead.
  adj_mat <- matrix(nrow=5, ncol=5, data=c(
    0, 0, 0, 1, 0,
    1, 0, 0, 1, 0,
    0, 0, 0, 1, 0,
    0, 1, 0, 0, 0,
    1, 1, 0, 0, 0))
  results <- spreadr(
    adj_mat, data.frame(node=1, activation=20),
    retention=0, time=3, suppress=0, decay=0)
  expected <- data.frame(
    node=as.factor(rep(1:5, 3)),
    activation=c(
      0, 10, 0, 0, 10,
      0, 0, 0, 5, 15,
      5/3, 5/3, 5/3, 0, 15),
    time=c(rep(1, 5), rep(2, 5), rep(3, 5)))

  expect_equal(results, expected)
})
