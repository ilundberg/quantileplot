library(quantileplot)

testthat::test_that("Test functionality with a basic call", {
  test_data <- data.frame(x = runif(100),
                          y = runif(100))
  test <- quantileplot(y ~ s(x),
                       data = test_data)
  testthat::expect_true(inherits(test,"quantileplot"))
  testthat::expect_true(inherits(test$plot,"ggplot"))
  testthat::expect_true(inherits(test$mqgam.out,"mqgam"))
  testthat::expect_equal(length(test$plot$layers),6)
})

testthat::test_that("Test functionality with confidence bands", {
  test_data <- data.frame(x = runif(100),
                          y = runif(100))
  test <- quantileplot(y ~ s(x),
                       data = test_data,
                       show_ci = T)
  testthat::expect_true(inherits(test,"quantileplot"))
  testthat::expect_true(inherits(test$plot,"ggplot"))
  testthat::expect_equal(length(test$plot$layers),7)
})

testthat::test_that("Test functionality with truncated x_data_range and y_data_range", {
  test_data <- data.frame(x = runif(100),
                          y = runif(100))
  test <- quantileplot(y ~ s(x),
                       data = test_data,
                       x_data_range = c(.2,.8),
                       y_data_range = c(.3,.7))
  testthat::expect_true(inherits(test,"quantileplot"))
  testthat::expect_true(inherits(test$plot,"ggplot"))
  testthat::expect_equal(test$x_data_range, c(.2,.8))
  testthat::expect_equal(test$y_data_range, c(.3,.7))
})

testthat::test_that("Test using labels instead of a legend", {
  test_data <- data.frame(x = runif(100),
                          y = runif(100))
  test <- quantileplot(y ~ s(x),
                       data = test_data,
                       quantile_notation = "label")
  testthat::expect_true(inherits(test,"quantileplot"))
  testthat::expect_true(inherits(test$plot,"ggplot"))
})

testthat::test_that("Test with weights", {
  test_data <- data.frame(x = runif(100),
                          y = runif(100),
                          weight = runif(100, .4,.6))
  test <- quantileplot(y ~ s(x),
                       data = test_data,
                       weights = "weight")
  testthat::expect_true(inherits(test,"quantileplot"))
  testthat::expect_true(inherits(test$plot,"ggplot"))
})
