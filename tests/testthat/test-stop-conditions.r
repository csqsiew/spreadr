context("spreadr stop conditions work well")

adj_mat <- matrix(
  c(0, 1,
    0, 0), nrow=2, byrow=TRUE)
initial <- data.frame(node=1, activation=20)

test_that("error thrown if both time, threshold_to_stop is NULL", {
  expect_error(
    spreadr(adj_mat, initial, time=NULL))  # threshold_to_stop defaults to NULL
})

test_that("terminates with time only", {
  results <- spreadr(adj_mat, initial, time=1)
  expected <- data.frame(
    node=as.factor(1:2),
    activation=c(10, 10),
    time=c(1, 1))
  expect_equal(results, expected)
})

test_that("terminates with threshold_to_stop only", {
  results <- spreadr(adj_mat, initial, time=NULL, threshold_to_stop=1)
  expected <- data.frame(
    node=as.factor(rep(1:2, 5)),
    activation=c(
    # t=  1,    2,        3,          4,            5
      10,10, 5,15, 2.5,17.5, 1.25,18.75, 0.625,18.125),
    time=c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
  expect_equal(results, expected)
})

test_that("terminates with both time and threshold_to_stop at threshold", {
  results <- spreadr(adj_mat, initial, time=6, threshold_to_stop=1)
  expected <- data.frame(
    node=as.factor(rep(1:2, 5)),
    activation=c(
    # t=  1,    2,        3,          4,            5
      10,10, 5,15, 2.5,17.5, 1.25,18.75, 0.625,18.125),
    time=c(1, 1, 2, 2, 3, 3, 4, 4, 5, 5))
  expect_equal(results, expected)
})

test_that("terminates with both time and threshold_to_stop at time", {
  results <- spreadr(adj_mat, initial, time=4, threshold_to_stop=1)
  expected <- data.frame(
    node=as.factor(rep(1:2, 4)),
    activation=c(
    # t=  1,    2,        3,          4,            5
      10,10, 5,15, 2.5,17.5, 1.25,18.75),
    time=c(1, 1, 2, 2, 3, 3, 4, 4))
  expect_equal(results, expected)
})
