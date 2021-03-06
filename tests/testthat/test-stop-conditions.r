adj_mat <- matrix(
  c(0, 1,
    0, 0), nrow=2, byrow=TRUE)
initial <- data.frame(node=1, activation=20)

test_that("error thrown if both time, threshold_to_stop is NULL", {
  expect_error(
    spreadr(adj_mat, initial, time=NULL),  # threshold_to_stop defaults to NULL
    "time and threshold_to_stop cannot both be NULL")
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
  results <- spreadr(adj_mat, initial, time=NULL, threshold_to_stop=2.5, decay=0.5)
  expected <- data.frame(
    node=as.factor(rep(1:2, 3)),
    activation=c(
      20/4,                20/2/2,                    # t=1
      20/4/4,    (20/2/2 + 20/4/2)/2,                 # t=2
      20/4/4/4, ((20/2/2 + 20/4/2)/2 + 20/4/4/2)/2),  # t=3
    time=c(1, 1, 2, 2, 3, 3))
  expect_equal(results, expected)
})

test_that("terminates with both time and threshold_to_stop at threshold", {
  results <- spreadr(adj_mat, initial, time=6, threshold_to_stop=2.5, decay=0.5)
  expected <- data.frame(
    node=as.factor(rep(1:2, 3)),
    activation=c(
      20/4,                20/2/2,                    # t=1
      20/4/4,    (20/2/2 + 20/4/2)/2,                 # t=2
      20/4/4/4, ((20/2/2 + 20/4/2)/2 + 20/4/4/2)/2),  # t=3
    time=c(1, 1, 2, 2, 3, 3))
  expect_equal(results, expected)
})

test_that("terminates with both time and threshold_to_stop at time", {
  results <- spreadr(adj_mat, initial, time=4, threshold_to_stop=1)
  expected <- data.frame(
    node=as.factor(rep(1:2, 4)),
    activation=c(
    # t=  1,    2,        3,          4
      10,10, 5,15, 2.5,17.5, 1.25,18.75),
    time=c(1, 1, 2, 2, 3, 3, 4, 4))
  expect_equal(results, expected)
})
