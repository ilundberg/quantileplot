library(quantileplot)

testthat::test_that("Test that incorrect arguments throw errors", {
  test_data <- data.frame(x = runif(100),
                          y = runif(100),
                          z = runif(100))
  # Error if you give more than one predictor
  testthat::expect_error(quantileplot(y ~ s(x) + z, data = test_data))
  # Error if you don't give data
  testthat::expect_error(quantileplot(y ~ s(x)))
  # Error if you give weights that are not a character
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, weights = c(1,2,3)))
  # Error if weights are not a column of the data
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, weights = "nonexistent_column"))
  # Error if quantiles outside (0,1)
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, q = -2))
  # Error if xlab or ylab is something that does not belong on an axis title, like a function
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, xlab = mean))
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, ylab = mean))
  # Error if x_data_range or y_data_range is not a vector of length 2
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, x_data_range = 1))
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, y_data_range = 1))
  # Error if x_breaks or y_breaks is something other than a function or numeric vector
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, x_breaks = c("a","b")))
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, y_breaks = c("a","b")))
  # Error if x_labels or y_labels given without breaks specified
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, x_labels = c("a","b")))
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, y_labels = c("a","b")))
  # Error if x_labels (y_labels) does not match the length of x_breaks (y_breaks)
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, x_breaks = c(.2,.5,.7), x_labels = c("a","b")))
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, y_breaks = c(.2,.5,.7), y_labels = c("a","b")))
  # Error if x_bw or y_bw is something other than a numeric scalar
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, x_bw = c(.2,.5,.7), x_labels = c("a","b")))
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, x_bw = "string"))
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, y_bw = c(.2,.5,.7), x_labels = c("a","b")))
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, y_bw = "string"))
  # Error if quantile_notation is some unknown string
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, quantile_notation = "not_an_option"))
  # Error if credibility level is not a numeric scalar
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, credibility_level = c(.5,.95)))
  # Error if granularity is not a whole number scalar
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, granularity = c(100,200)))
  # Error if argGam is not a list
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, argGam = "method"))
  # Error if previous_fit is not a quantileplot object
  testthat::expect_error(quantileplot(y ~ s(x), data = test_data, previous_fit = "not_a_quantileplot"))


  test <- quantileplot(y ~ s(x),
                       data = test_data,
                       quantile_notation = "label")
})
