#' Create a Scatter for a Quantile Plot
#'
#' @description Converts a quantile plot into an analogous scatter plot.
#' @param object An object of class \code{quantileplot}, created by a call to \code{quantileplot()}.
#' @param fraction Numeric scalar in (0,1]. Fraction of points to be randomly sampled for inclusion in the scatter plot. Useful in very large data sets.
#' @param ... Other arguments
#'
#' @return A ggplot2 object containing a scatter plot.
#'
#' @references Lundberg, Ian, Robin C. Lee, and Brandon M. Stewart. 2021. "The quantile plot: A visualization for bivariate population relationships." Working paper.
#' @references Lundberg, Ian, and Brandon M. Stewart. 2020. "Comment: Summarizing income mobility with multiple smooth quantiles instead of parameterized means." Sociological Methodology 50(1):96-111.
#' @references Fasiolo, Matteo, Simon N. Wood, Margaux Zaffran, Raphaël Nedellec, and Yannig Goude. 2020. "Fast calibrated additive quantile regression." Journal of the American Statistical Association.
#'
#' @export
#' @import dplyr
#'
#' @examples
#' x <- rbeta(1000,1,2)
#' y <- log(1 + 9 * x) * rbeta(1000, 1, 2)
#' data <- data.frame(x = x, y = y)
#' qp <- quantileplot(y ~ s(x), data)
#' scatter.quantileplot(qp)

scatter.quantileplot <- function(object, fraction = 1, ...) {
  if (class(object) != "quantileplot") {
    stop("Argument error: The first argument object must be an object of class quantileplot")
  }
  if (fraction <= 0 | fraction > 1) {
    stop("Argument error: fraction should be a scalar greater than 0 and less than or equal to 1.")
  }
  # Initialize objects for non-standard evaluation
  x <- y <- weight <- NULL
  # Extract the plot
  for_scatter <- object$plot
  # Keep only the plot area (remove curves and densities)
  for_scatter$layers <- for_scatter$layers[(length(object$plot$layers) - 2):length(object$plot$layers)]
  # Extract the x and y variable names
  y_str <- all.vars(object$arguments$formula)[1]
  x_str <- all.vars(object$arguments$formula)[2]
  # Create a data frame for the scatter plot
  scatter_data <- data.frame(x = object$arguments$data[[x_str]],
                             y = object$arguments$data[[y_str]])
  # Append weights
  if (is.null(object$arguments$weights)) {
    scatter_data$weight <- 1
  } else {
    scatter_data$weight <- object$arguments$data[[object$arguments$weights]]
  }
  # Restrict to the *_data_range
  if (!is.null(object$arguments$x_data_range)) {
    scatter_data <- scatter_data %>%
      dplyr::filter(x > object$arguments$x_data_range[1] & x < object$arguments$x_data_range[2])
  }
  if (!is.null(object$arguments$y_data_range)) {
    scatter_data <- scatter_data %>%
      dplyr::filter(y > object$arguments$y_data_range[1] & y < object$arguments$y_data_range[2])
  }
  if (fraction < 1) {
    scatter_data <- scatter_data %>%
      dplyr::slice_sample(prop = fraction)
  }
  # Create the scatter plot
  scatter <- for_scatter +
    ggplot2::geom_point(data = scatter_data,
                        ggplot2::aes(x = x, y = y, size = weight),
                        color = "gray") +
    ggplot2::scale_size_continuous(range = c(.1,1)) +
    ggplot2::theme(legend.position = "none")
  return(scatter)
}
