adj_mat <- matrix(
  c(0, 1,
    0, 0), nrow=2, byrow=TRUE)

test_that("start_run without time column assumes t=0", {
  results <- spreadr(
    adj_mat, data.frame(node=as.factor(1:2), activation=c(10, 10)), time=3)
  expected <- data.frame(
    node=as.factor(c(1,  2,   1,    2,    1,     2)),
    activation=c(    5, 15, 2.5, 17.5, 1.25, 18.75),
    time      =c(    1,  1,   2,    2,    3,     3))
  expect_equal(results, expected)
})

test_that("start_run with time column adds activation at the right times", {
  results <- spreadr(
    adj_mat,
    data.frame(node=as.factor(c(1, 1)), activation=c(10, 10), time=1:2),
    time=3)
  expected <- data.frame(
    node=as.factor(c( 1, 2,  1, 2,   1,    2)),
    activation=c(    10, 0, 15, 5, 7.5, 12.5),
    time      =c(     1, 1,  2, 2,   3,    3))
  expect_equal(results, expected)
})
