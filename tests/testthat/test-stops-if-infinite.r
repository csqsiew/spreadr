adj_mat <- matrix(
  c(0, 1,
    0, 0), nrow=2, byrow=TRUE)
initial <- data.frame(node=1, activation=20)

test_that("warning shown if infinite loop", {
  expect_snapshot_error(
    spreadr(adj_mat, initial, retention = 0, time = NULL, threshold_to_stop = 1),
    class="warning")
})
