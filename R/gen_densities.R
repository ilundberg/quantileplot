#' Generate Density Estimates for a \code{quantileplot}
#'
#' @description Estimate the conditional density of the outcome at at particular values of the predictor. These conditional densities appear as vertical slices in the output of \code{\link{quantileplot}}. This function is typically called indirectly via a user call to \code{\link{quantileplot}}.
#'
#' @param data Data frame containing columns \code{x}, \code{y}, and \code{weight}. For a simple random sample, set \code{weight} to be constant.
#' @param slice_n Integer number of vertical slices (conditional densities of y given x) to be plotted. Default is 5.
#' @param x_data_range Numeric vector of length 2 containing the range of horizontal values to be plotted. Defaults to the range of the predictor variable in \code{data}. You may want to specify a narrower range if the predictor is extremely skewed. Quantile curves and densities will be estimated only on data in this range, and the plot will note the percent truncated.
#' @param y_data_range Numeric vector of length 2 containing the range of vertical values to be plotted. Defaults to the range of the outcome variable in \code{data}. You may want to specify a narrower range if the outcome is extremely skewed. Densities are truncated to this range. All data contribute to quantile curve estimation regardless of \code{y_data_range} to avoid selection on the outcome, though the visualization is truncated to \code{y_data_range}. The plot will note the percent truncated.
#' @param x_bw Numeric bandwidth for density estimation in the \code{x} dimension. The standard deviation of a Gaussian kernel. If \code{NULL}, this is set by the defaults in \code{stats::density()}.
#' @param y_bw Numeric bandwidth for density estimation in the \code{y} dimension. The standard deviation of a Gaussian kernel. If \code{NULL}, this is set by the defaults in \code{stats::density()}.
#' @param granularity Integer number of points at which to evaluate each density. Defaults to 512, as in \code{stats::density()}. Higher values yield more granular density estimates.
#'
#' @return List containing \code{marginal} and \code{conditional}, each of which is a data frame with density estimates.
#'
#' @references Lundberg, Ian, Robin C. Lee, and Brandon M. Stewart. 2021. "The quantile plot: A visualization for bivariate population relationships." Working paper.
#' @references Lundberg, Ian, and Brandon M. Stewart. 2020. "Comment: Summarizing income mobility with multiple smooth quantiles instead of parameterized means." Sociological Methodology 50(1):96-111.
#' @references Fasiolo, Matteo, Simon N. Wood, Margaux Zaffran, Raphaël Nedellec, and Yannig Goude. 2020. "Fast calibrated additive quantile regression." Journal of the American Statistical Association.
#'
#' @export
#' @import dplyr
gen_densities <- function(data, slice_n = 7, x_data_range = NULL, y_data_range = NULL, x_bw = NULL, y_bw = NULL, granularity = 512) {
  if (!all(c("x","y","weight") %in% colnames(data))) {
    stop("The data object passed to gen_densities must have these columns: x, y, weight")
  }
  if (any(is.na(data$x))) {
    stop("In the data object passed to gen_densities, x has missing values")
  }
  if (any(is.na(data$y))) {
    stop("In the data object passed to gen_densities, y has missing values")
  }
  if (any(is.na(data$weight))) {
    stop("In the data object passed to gen_densities, weight has missing values")
  }
  # Normalize the weight as requested by stats::density()
  data$weight <- data$weight / sum(data$weight)
  # If bandwidths were not specified by the user, set them by the default from stats::density()
  if (is.null(x_bw) | is.null(y_bw)) {
    cat("Bandwidths are being set at:\n")
  }
  if (is.null(x_bw)) {
    x_bw <- stats::density(data$x, weights = data$weight, kernel = "gaussian")$bw
    cat(paste("x_bw =",x_bw,"\n"))
  }
  if (is.null(y_bw)) {
    y_bw <- stats::density(data$y, weights = data$weight, kernel = "gaussian")$bw
    cat(paste("y_bw =",y_bw,"\n"))
  }
  # If x_data_range or y_data_range is null, replace with the range of the data
  if (is.null(x_data_range)) {
    x_data_range <- range(data$x)
  }
  if (is.null(y_data_range)) {
    y_data_range = range(data$y)
  }

  # Define the grid of values at which to estimate densities.
  # Spread the grid evenly over the range, excluding the endpoints.
  marginal_x_seq <- seq(x_data_range[1], x_data_range[2], length.out = granularity + 2)[2:(granularity + 1)]
  # Use labeling::extended to select slice locations to match the vertical major grid lines
  grid_line_locations <- labeling::extended(x_data_range[1], x_data_range[2], slice_n + 2)
  if (length(grid_line_locations) == slice_n + 2) {
    conditional_x_seq <- grid_line_locations[2:(slice_n + 1)]
  } else {
    # Sometimes labeling::extended chooses a different number of points to help with label making.
    # In that case, stick with the number of slices the user requested.
    conditional_x_seq <- seq(x_data_range[1], x_data_range[2], length.out = slice_n + 2)[2:(slice_n + 1)]
  }
  rm(grid_line_locations)
  # For the conditional y sequence, manually set a sequence based on the chosen granularity
  conditional_y_seq <- seq(y_data_range[1], y_data_range[2], length.out = granularity + 2)[2:(granularity + 1)]

  # Estimate the marginal density
  marginal_density <- data.frame(x = rep(NA, granularity),
                                 density = rep(NA, granularity))
  for (i in 1:granularity) {
    x_eval <- marginal_x_seq[i]
    contribution_each_observation <- 1 / x_bw * stats::dnorm((x_eval - data$x) / x_bw)
    estimate <- stats::weighted.mean(contribution_each_observation,
                                     w = data$weight)
    # Place those in the marignal_density data frame
    marginal_density$x[i] <- x_eval
    marginal_density$density[i] <- estimate
  }

  # Estimate the conditional densities
  conditional_density <- data.frame(x = rep(conditional_x_seq, each = granularity),
                                    y = rep(conditional_y_seq, slice_n),
                                    density = NA)
  for (i in 1:nrow(conditional_density)) {
    # For easier reference, extract this (x,y) for evaluation
    x_eval <- conditional_density$x[i]
    y_eval <- conditional_density$y[i]

    # Estimate contribution of each data point to the marginal density at (x_eval)
    m_each <- 1 / x_bw * stats::dnorm((x_eval - data$x) / x_bw)
    # Average those to produce the marginal density estimate at (x_eval)
    # Do the weighted mean manually to reduce risk of underflow problems in large samples.
    m <- sum(m_each * data$weight) / sum(data$weight)

    # Estimate contribution of each data point to the conditional density at (x_eval,y_eval)
    # using the product kernel.
    # Divide by m internal to the weighted mean rather than external so that the internal
    # product is not so small.
    # This is similar to Hyndman et al. 1996 eq. 2.4-2.5.
    g_each <- (m_each / m) * 1 / y_bw * stats::dnorm((y_eval - data$y) / y_bw)
    # Average those to produce the marginal density estimate at (x_eval, y_eval)
    g <- sum(g_each * data$weight) / sum(data$weight)

    # Place that in the data frame
    conditional_density$density[i] <- g
  }
  return(list(marginal = marginal_density,
              conditional = conditional_density,
              x_bw = x_bw,
              y_bw = y_bw))
}

