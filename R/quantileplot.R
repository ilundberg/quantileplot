#' Create a Smooth Quantile Plot
#'
#' @description Creates a bivariate, smooth quantile plot. This is the central function of the \code{quantileplot} package. This plot visualizes estimates of the marginal density of the predictor, the conditional density of the outcome at selected values of the predictor, and smooth curves showing quantiles of the outcome as smooth functions of the predictor. This package is described in greater depth by Lundberg, Lee, and Stewart (2021), which is a generalization of Lundberg and Stewart (2020). The statistical core of the package relies on the methods of Fasiolo et al. (2020).
#' @param formula A bivariate model formula (e.g. \code{y ~ s(x)})
#' @param data Data frame containing the variables in \code{formula}. If \code{weights} are specified, they must be a column of \code{data}.
#' @param weights String name for sampling weights, which are a column of \code{data}. If not given, a simple random sample is assumed.
#' @param quantiles Numeric vector containing quantiles to be estimated. Values should be between 0 and 1.
#' @param slice_n Integer number of vertical slices (conditional densities of y given x) to be plotted. Default is 7.
#' @param show_ci Logical, defaults to \code{FALSE}. Whether to show credible intervals for the estimated smooth quantile curves.
#' @param quantile_notation String, either \code{legend} or \code{label}. If \code{legend} (the default), then quantile curves are denoted by colors with a legend. If \code{label}, then quantile curves are annotated in the plot.
#' @param xlab String x-axis title
#' @param ylab String y-axis title
#' @param x_data_range Numeric vector of length 2 containing the range of horizontal values to be plotted. Defaults to the range of the predictor variable in \code{data}. You may want to specify a narrower range if the predictor is extremely skewed. Quantile curves and densities will be estimated only on data in this range, and the plot will note the percent truncated.
#' @param y_data_range Numeric vector of length 2 containing the range of vertical values to be plotted. Defaults to the range of the outcome variable in \code{data}. You may want to specify a narrower range if the outcome is extremely skewed. Densities are truncated to this range. All data contribute to quantile curve estimation regardless of \code{y_data_range} to avoid selection on the outcome, though the visualization is truncated to \code{y_data_range}. The plot will note the percent truncated.
#' @param x_axis_range Numeric vector of length 2 for custom x-axis limits. This affects the plotting area but does not affect the data analyzed or displayed. To truncate the data, use \code{x_data_range}.
#' @param y_axis_range Numeric vector of length 2 for custom y-axis limits. This affects the plotting area but does not affect the data analyzed or displayed. To truncate the data, use \code{y_data_range}.
#' @param x_breaks Numeric vector of values for x-axis breaks. Alternatively, customize after producing the plot by modifying the resulting \code{ggplot2} object. See vignette for examples.
#' @param y_breaks Numeric vector of values for x-axis breaks. Alternatively, customize after producing the plot by modifying the resulting \code{ggplot2} object. See vignette for examples.
#' @param x_labels Vector of \code{length(x_breaks)} containing labels, or a function to convert breaks into labels. Alternatively, customize after producing the plot by modifying the resulting \code{ggplot2} object. See vignette for examples.
#' @param y_labels Vector of \code{length(y_breaks)} containing labels, or a function to convert breaks into labels. Alternatively, customize after producing the plot by modifying the resulting \code{ggplot2} object. See vignette for examples.
#' @param x_bw Numeric bandwidth for density estimation in the \code{x} dimension. The standard deviation of a Gaussian kernel. If \code{NULL}, this is set by the defaults in \code{stats::density()}.
#' @param y_bw Numeric bandwidth for density estimation in the \code{y} dimension. The standard deviation of a Gaussian kernel. If \code{NULL}, this is set by the defaults in \code{stats::density()}.
#' @param truncation_notation String, one of \code{label}, \code{label_no_pct}, or \code{none}. If \code{x_data_range} or \code{y_data_range} is narrower than the range of the data, this argument specifies how to note that truncation on the visualization. If \code{label}, then truncation is labeled including the percent of data truncated. If \code{label_no_pct}, then truncation is labeled but the percent truncated is omitted. If \code{none}, then truncation is not labeled on the plot.
#' @param credibility_level Numeric probability value for credible intervals; default to 0.95 to produce 95 percent credible intervals. Only relevant if \code{show_ci = TRUE}.
#' @param uncertainty_draws A whole number. If non-null, the number of simulated posterior draws to estimate for each smooth quantile curve. When used with the \code{plot} function, these appear in panels below the main plot.
#' @param inverse_transformation A function of a scalar argument. Only used in the rare use case where the outcome has an extremely skewed distribution and the user wants to estimate the quantile curves on a transformed outcome, to be brought back to the original scale for the visualization. In that case, this argument is the function to convert from the transformed outcome back to the original scale. For instance, if the outcome in the model formula is \code{log(y + 1)} then the inverse transformation should be \code{function(y) exp(y) - 1}. This is a rare use case because it is only relevant when a transformation of the outcome aids the estimation of quantile curves. If you want to visualize on a transformed scale, you should instead create a transformed variable in \code{data} rather than conducting the transformation within the model formula. For common transformations (e.g. log(y)), the \code{inverse_transformation} argument can left \code{NULL} and will be determined automatically.
#' @param granularity Integer number of points at which to evaluate each density. Defaults to 512, as in \code{stats::density()}. Higher values yield more granular density estimates.
#' @param second_formula Model formula to allow the learning rate to change as a function of the predictor. This is passed to \code{mqgam} as the second element in the \code{form} argument. Defaults to the same specification as \code{formula} but without the outcome variable.
#' @param argGam Additional arguments to the GAM for model fitting. Passed to mqgam.
#' @param previous_fit The result of a previous call to \code{quantileplot}. If provided, then the \code{mqgam} fit for the quantile curves will not be re-estimated, which can be useful for iteratively deciding about other arguments in settings that are computationally demanding. This argument must be paired with other arguments that match the previous call (e.g. \code{data}, \code{formula}).
#' @param ... Other arguments passed to \code{mqgam}.
#'
#' @return An object of S3 class \code{quantileplot}, which supports \code{summary()}, \code{print()}, and \code{plot()} functions. The returned object has several elements.
#' \itemize{
#' \item \code{plot} is a \code{ggplot2} object. This contains the most basic plot. The user can customize this output by passing additional layers to \code{quantileplot.out$plot} as they would for any \code{ggplot2} object.
#' \item \code{sim_curve_plots} is a list object of \code{ggplot2} objects, one for each quantile curve, which shows the point estimate for the curve in black and a series of simulated posterior samples in gray.
#' \item \code{densities} is a list of length four.
#' \itemize{
#' \item \code{marginal} and \code{conditional} are data frames containing the estimated marginal and conditional densities.
#' \item \code{x_bw} and \code{y_bw} are the bandwidths used for Gaussian kernel density estimation.
#' }
#' \item \code{curves} is a data frame containing the estimated quantile curves.
#' \item \code{mqgam.out} is the output from the call to the \code{mqgam} function in the \code{qgam} package, which is used to estimate the quantile curves.
#' \item \code{x_data_range} and \code{y_data_range} are the horizontal and vertical ranges of the plot.
#' \item \code{slice_x_values} are the predictor values at which vertical conditional densities are estimated.
#' \item \code{call} is the user's call that produced these results.
#' \item \code{arguments} is a list of all the arguments to the function, including those specified by the user and those specified by defaults.
#' }
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
#' quantileplot(y ~ s(x), data)

quantileplot <- function(
  formula,
  data,
  weights = NULL,
  quantiles = c(.1, .25, .5, .75, .9),
  slice_n = 7,
  show_ci = FALSE,
  quantile_notation = "legend",
  xlab = NULL,
  ylab = NULL,
  x_data_range = NULL,
  y_data_range = NULL,
  x_axis_range = NULL,
  y_axis_range = NULL,
  x_breaks = NULL,
  y_breaks = NULL,
  x_labels = ggplot2::waiver(),
  y_labels = ggplot2::waiver(),
  x_bw = NULL,
  y_bw = NULL,
  truncation_notation = "label",
  credibility_level = 0.95,
  uncertainty_draws = NULL,
  inverse_transformation = NULL,
  granularity = 512,
  second_formula = NULL,
  argGam = NULL,
  previous_fit = NULL,
  ...
) {

  # Make a list of all arguments, to return at end of the function
  arguments <- list(formula = formula,
                    data = data,
                    weights = weights,
                    quantiles = quantiles,
                    slice_n = slice_n,
                    show_ci = show_ci,
                    quantile_notation = quantile_notation,
                    xlab = xlab,
                    ylab = ylab,
                    x_data_range = x_data_range,
                    y_data_range = y_data_range,
                    x_axis_range = x_axis_range,
                    y_axis_range = y_axis_range,
                    x_breaks = x_breaks,
                    y_breaks = y_breaks,
                    x_labels = x_labels,
                    y_labels = y_labels,
                    x_bw = x_bw,
                    y_bw = y_bw,
                    truncation_notation = truncation_notation,
                    credibility_level = credibility_level,
                    uncertainty_draws = uncertainty_draws,
                    inverse_transformation = inverse_transformation,
                    granularity = granularity,
                    second_formula = second_formula,
                    argGam = argGam,
                    previous_fit = previous_fit)

  # Extract the x and y variables first because they are also used in checks

  if (!rlang::is_formula(formula)) {
    stop("Argument error: formula should be a model formula")
  }
  # Extract strings for the x and y variables
  y_str <- all.vars(formula)[1]
  x_str <- all.vars(formula)[2]

  # Initialize objects that will be called by non-standard evaluation.
  # This is only to stop a warning message in package building.
  x <- y <- estimate <- curve <- percentile <- percentile_num <-
    ci.max <- ci.min <- index <- num <- ymax <- ymin <- weight <-
    variable <- x_position <- y_position <- angle <- vjust <-
    label <- label_no_pct <- density <- x_low_pct <- x_high_pct <-
    y_low_pct <- y_high_pct <- NULL

  ################
  # Check inputs #
  ################


  if (length(all.vars(formula)) != 2) {
    stop("Argument error: formula should involve one predictor and one outcome")
  }
  if (!is.data.frame(data)) {
    stop("Argument error: data should be a data frame")
  }
  if (!is.function(x_labels) & class(x_labels) != "waiver") {
    if (!is.numeric(x_labels) & !is.character(x_labels)) {
      stop("Argument error: If x_labels is not a function, then it should be a numeric or character vector")
    }
    if (length(x_labels) != length(x_breaks)) {
      stop("Argument error: If x_labels is a vector, it should be the same length as x_breaks")
    }
  }
  if (!is.function(y_labels) & class(y_labels) != "waiver") {
    if (!is.numeric(y_labels) & !is.character(y_labels)) {
      stop("Argument error: If y_labels is not a function, then it should be a numeric or character vector")
    }
    if (length(y_labels) != length(y_breaks)) {
      stop("Argument error: If y_labels is a vector, it should be the same length as y_breaks")
    }
  }
  # Check that the data object contains the x and y variables
  if (!(x_str %in% colnames(data))) {
    stop("Argument error: The predictor specified in formula needs to be a column of data.")
  }
  if (!(y_str %in% colnames(data))) {
    stop("Argument error: The outcome specified in formula needs to be a column of data.")
  }
  if (!is.null(weights)) {
    if (length(weights) > 1) {
      stop("Argument error: weights should be a character string corresponding to the name of a column in data.")
    }
    if (!is.character(weights)) {
      stop("Argument error: weights should be a character string corresponding to the name of a column in data.")
    }
    if(!(weights %in% colnames(data))) {
      stop("Argument error: The weights that you specified need to be a column of data.")
    }
  }
  if (!is.numeric(slice_n) | length(slice_n) != 1) {
    stop("Argument error: slice_n should be a whole number")
  }
  if (abs(round(slice_n) - slice_n) > .00001) {
    stop("Argument error: slice_n should be a whole number")
  }
  if (!is.numeric(quantiles)) {
    stop("Argument error: quantiles should be a numeric vector")
  }
  if (any(quantiles <= 0 | quantiles >= 1)) {
    stop("Argument error: quantiles should be between 0 and 1")
  }
  if (!(quantile_notation %in% c("label","legend"))) {
    stop("Argument error: quantile_notation should be one of label or legend")
  }
  if (!is.null(uncertainty_draws)) {
    if (!is.numeric(uncertainty_draws)) {
      stop("Argument error: uncertainty_draws should be numeric")
    }
    if (trunc(uncertainty_draws) != uncertainty_draws) {
      stop("Argument error: uncertainty_draws should be an integer")
    }
  }
  if (!is.logical(show_ci)) {
    stop("Argument error: show_ci should be TRUE or FALSE")
  }
  if (!is.numeric(credibility_level) | length(credibility_level) != 1) {
    stop("Argument error: credibility_level should be a numeric scalar")
  }
  if (credibility_level <= 0 | credibility_level >= 1) {
    stop("Argument error: credibility_level should be a credible value between 0 and 1")
  }
  if (!is.null(second_formula)) {
    if (!rlang::is_formula(second_formula)) {
      stop("Argument error: A second_formula was supplied but it is not a model formula")
    }
    if (all.vars(second_formula) != x_str) {
      stop("Argument error: A second_formula was supplied but seems to involve something other than the one predictor variable from formula")
    }
  }
  # If not supplied, set the second formula to match the first but without the outcome variable
  if (is.null(second_formula)) {
    second_formula <- formula(paste(as.character(formula)[-2], collapse = ""))
  }
  if (!is.null(x_data_range)) {
    if (!is.numeric(x_data_range) | length(x_data_range) != 2) {
      stop("Argument error: x_data_range should be a numeric vector of length 2")
    }
    if (x_data_range[1] >= x_data_range[2]) {
      stop("Argument error: The first number in x_data_range should be less than the second number")
    }
  }
  if (!is.null(y_data_range)) {
    if (!is.numeric(y_data_range) | length(y_data_range) != 2) {
      stop("Argument error: y_data_range should be a numeric vector of length 2")
    }
    if (y_data_range[1] >= y_data_range[2]) {
      stop("Argument error: The first number in y_data_range should be less than the second number")
    }
  }
  if (!is.null(x_bw)) {
    if (!is.numeric(x_bw) | length(x_bw) != 1) {
      stop("Argument error: x_bw should be a numeric scalar")
    }
  }
  if (!is.null(y_bw)) {
    if (!is.numeric(y_bw) | length(y_bw) != 1) {
      stop("Argument error: y_bw should be a numeric scalar")
    }
  }
  if (!is.numeric(granularity) | length(granularity) != 1) {
    stop("Argument error: granularity should be a whole number scalar")
  }
  if (abs(round(granularity) - granularity) > .00001) {
    stop("Argument error: granularity should be a whole number")
  }
  if (!is.null(previous_fit)) {
    if (!all(dim(previous_fit$arguments$data) == dim(data))) {
      stop("Argument error: The supplied data argument is of a different size from the data in the supplied previous_fit")
    }
    if (!all(previous_fit$arguments$data == data)) {
      stop("Argument error: The supplied data argument and the data from the previous fit do not match")
    }
    if (formula != previous_fit$arguments$formula) {
      stop("Argument error: The supplied formula argument and the formula from the previous fit do not match")
    }
  }

  #################
  # Modify inputs #
  #################

  # Create axis labels if they were not given
  if (is.null(xlab)) {
    xlab <- x_str
  }
  if (is.null(ylab)) {
    ylab <- y_str
  }

  # Remove missing values from the data and print warnings.
  if (any(is.na(data[[x_str]]))) {
    message(paste("Warning: There are",sum(is.na(data[[x_str]])),"missing values in the predictor. Those cases will be dropped."))
    data <- data[!is.na(data[[x_str]]),]
  }
  if (any(is.na(data[[y_str]]))) {
    message(paste("Warning: There are",sum(is.na(data[[y_str]])),"missing values in the outcome. Those cases will be dropped."))
    data <- data[!is.na(data[[y_str]]),]
  }
  if (!is.null(weights)) {
    if (any(is.na(data[[weights]]))) {
      message(paste("Warning: There are",sum(is.na(data[[weights]])),"missing values in the weight. Those cases will be dropped."))
      data <- data[!is.na(data[[weights]]),]
    }
  }

  # Prepare a modified dataset where these are just called x and y
  # This simplifies non-standard evaluation calls
  if (is.null(weights)) {
    data.mod <- data.frame(x = data[[x_str]],
                           y = data[[y_str]],
                           weight = 1)
  } else {
    data.mod <- data.frame(x = data[[x_str]],
                           y = data[[y_str]],
                           weight = data[[weights]] / mean(data[[weights]]))
  }

  # Set the *_data_range to be the range of the data if missing
  if (is.null(x_data_range)) {
    x_data_range <- range(data.mod$x)
  }
  if (is.null(y_data_range)) {
    y_data_range <- range(data.mod$y)
  }

  # Determine the amount truncated at the lower and upper ends of x_data_range and y_data_range
  truncation_amounts <- data.mod %>%
    summarize(x_low_pct = stats::weighted.mean(x < x_data_range[1], w = weight),
              x_high_pct = stats::weighted.mean(x > x_data_range[2], w = weight),
              y_low_pct = stats::weighted.mean(y < y_data_range[1], w = weight),
              y_high_pct = stats::weighted.mean(y > y_data_range[2], w = weight),
              .groups = "drop")

  # Restrict to the predictor range of interest
  if (!is.null(x_data_range)) {
    data.mod <- data.mod[data.mod$x >= x_data_range[1] & data.mod$x <= x_data_range[2],]
  }

  # Prepare components of the plot area.
  # Find the minimum value of the horizontal grid lines to be plotted.
  # These grid lines avoid having incorrect y-labels in the region where the x-density is (where we want no labels).
  # This uses the labeling package that ggplot2 ordinarily calls,
  # with 5 major breaks (the ggplot2 default number).
  if (is.null(y_breaks)) {
    y_breaks <- labeling::extended(y_data_range[1], y_data_range[2], 5)
  }
  y_minor_grid_lines <- c(min(y_breaks) - .5 * mean(diff(y_breaks)),
                          y_breaks + .5 * mean(diff(y_breaks)))
  y_minor_grid_lines <- y_minor_grid_lines[y_minor_grid_lines >= y_data_range[1] & y_minor_grid_lines <= y_data_range[2]]

  # Determine the x-axis range to be plotted, if not provided by the user
  if (is.null(x_axis_range) & (quantile_notation == "label")) {
    # Slightly expand the axis to make room for the label
    x_axis_range <- x_data_range + c(0,.25 * diff(x_data_range))
  } else if (is.null(x_axis_range)) {
    # If not using labels, use the data range
    x_axis_range <- x_data_range
  }

  # Determine the y-axis range to be plotted, if not provided by the user
  if (is.null(y_axis_range)) {
    y_axis_range <- y_data_range
    # If the grid lines go lower or higher, slightly expand the axis.
    if (min(y_minor_grid_lines) < y_axis_range[1]) {
      y_axis_range[1] <- min(y_minor_grid_lines)
    }
    if (max(y_minor_grid_lines) > y_axis_range[2]) {
      y_axis_range[2] <- max(y_minor_grid_lines)
    }
    # If there is a note at the top or bottom, we need a little extra space
    current_axis_range <- diff(y_axis_range)
    if (y_data_range[1] > min(data.mod$y)) {
      y_axis_range[1] <- y_axis_range[1] - .03 * current_axis_range
    }
    if (y_data_range[2] < max(data.mod$y)) {
      y_axis_range[2] <- y_axis_range[2] + .03 * current_axis_range
    }
    rm(current_axis_range)
    # Add space at the bottom for the horizontal density
    y_axis_range[1] <- y_axis_range[1] - .2 * diff(y_axis_range)
  }

  ######################
  # Conduct estimation #
  ######################

  # Create a data frame of smooth curve predictions
  if (is.null(previous_fit)) {
    cat("Beginning quantile curve estimation via mqgam.\n")
    cat("Note that this may take time. It is computationally much harder than OLS.\n")
    gen_curves.out <- gen_curves(formula = formula,
                                 second_formula = second_formula,
                                 data = data[data[[x_str]] >= x_data_range[1] & data[[x_str]] <= x_data_range[2],],
                                 weights = weights,
                                 quantiles = quantiles,
                                 credibility_level = credibility_level,
                                 uncertainty_draws = uncertainty_draws,
                                 inverse_transformation = inverse_transformation,
                                 argGam = argGam,
                                 ...)
  } else {
    cat("Using quantile curves from previous_fit.\n")
    gen_curves.out <- list(curves = previous_fit$curves,
                           mqgam.out = previous_fit$mqgam.out)
  }
  curves <- gen_curves.out$curves

  # Generate density estimates
  cat("Beginning density estimation\n")
  densities <- gen_densities(data = data.mod,
                             slice_n = slice_n,
                             x_data_range = x_data_range,
                             y_data_range = y_data_range,
                             x_bw = x_bw,
                             y_bw = y_bw,
                             granularity = granularity)

  ################################################################
  # Convert estimation output into datasets prepared for ggplot2 #
  ################################################################

  # Prepare the marginal density for use with geom_ribbon()
  marginal <- densities$marginal %>%
    dplyr::mutate(x = x,
                  ymin = y_axis_range[1],
                  ymax = y_axis_range[1] + (density / max(density)) * 0.15 * diff(y_data_range))

  # Prepare the conditional density for use with geom_polygon()
  x_spacing <- .9*diff(sort(unique(densities$conditional$x)))[1]
  conditional <- densities$conditional %>%
    # Make the highest point of the highest conditional density
    # equal the amount of space between vertical slices
    dplyr::mutate(density = density / max(density) * x_spacing) %>%
    # Append density = 0 at the endpoints to facilitate polygon plotting.
    # This is needed because the polygons should go down to density = 0 at the ends.
    dplyr::group_by(x) %>%
    dplyr::arrange(x,y) %>%
    dplyr::mutate(index = 1:n()) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(densities$conditional %>%
                       dplyr::group_by(x) %>%
                       dplyr::arrange(x,y) %>%
                       dplyr::mutate(num = n()) %>%
                       # Keep only the first and last observations
                       dplyr::filter(1:n() %in% c(1,n())) %>%
                       # Assign them densities of zero and new indices
                       dplyr::mutate(density = 0,
                                     index = ifelse(1:n() == 1, 0, num + 1)) %>%
                       dplyr::select(-num) %>%
                       dplyr::ungroup()) %>%
    dplyr::arrange(x,index)

  # If not specified by user, use x-axis breaks that correspond to the data limits.
  # Ideally, use breaks to correspond to the vertical slices
  if (is.null(x_breaks)) {
    x_breaks <- labeling::extended(x_data_range[1], x_data_range[2], slice_n + 2)
    # Sometimes that selects a number of breaks not equal to slice_n + 2.
    # That happens if the slice_n + 2 breaks would produce weird numbers for the labels.
    # In that case, the breaks will not align with the slices.
    # Go with 5 breaks in that case, because this is a nice number of breaks.
    if (length(x_breaks) != slice_n + 2) {
      x_breaks <- labeling::extended(x_data_range[1], x_data_range[2], 5)
    }
  }

  # Prepare curves for plotting.
  # A key step here is to truncate the curves when they go beyond the range of the visualization.
  curves_point <- curves %>%
    dplyr::filter(curve == "point") %>%
    # Truncate to the x_data_range
    dplyr::filter(x >= x_data_range[1] & x <= x_data_range[2]) %>%
    # Truncate the estimate and confidence bands if they go vertically outside the plot.
    # We truncate them by making them NA if outside the range.
    dplyr::mutate(estimate = case_when(estimate >= y_data_range[1] & estimate <= y_data_range[2] ~ estimate),
                  ci.min = dplyr::case_when(
                    # If ci.min is in range, use ci.min
                    ci.min >= y_data_range[1] & ci.min <= y_data_range[2] ~ ci.min,
                    # If ci.min is below the range but ci.max is in range, use the bottom of the range for ci.min.
                    # This causes the confidence band to plot right up to the bottom of the visualization range.
                    ci.min < y_data_range[1] & ci.max >= y_data_range[1] & ci.max <= y_data_range[2] ~ y_data_range[1],
                    # If the band is the full range, use the full range
                    ci.min < y_data_range[1] & ci.max > y_data_range[2] ~ y_data_range[1]
                  ),
                  ci.max = dplyr::case_when(
                    # If ci.max is in range, use ci.max
                    ci.max >= y_data_range[1] & ci.max <= y_data_range[2] ~ ci.max,
                    # If ci.max is above the range but ci.min is in range, use the top of the range for ci.max
                    # This causes the confidence band to plot right up to the top of the visualization range.
                    ci.max > y_data_range[2] & ci.min >= y_data_range[1] & ci.min <= y_data_range[2] ~ y_data_range[2],
                    # If the band is the full range, use the full range
                    ci.min < y_data_range[1] & ci.max > y_data_range[2] ~ y_data_range[2]
                  ))

  ####################
  # PRODUCE THE PLOT #
  ####################

  p <- ggplot2::ggplot() +
    # Conditional densities (vertical slices)
    ggplot2::geom_polygon(data = conditional,
                          ggplot2::aes(x = x + density, y = y, group = x),
                          fill = "gray", alpha = .8) +
    # Marginal density (horizontal at bottom)
    ggplot2::geom_ribbon(data = marginal,
                         ggplot2::aes(x = x, ymin = ymin, ymax =  ymax),
                         fill = "gray", alpha = .8 ) +
    # Smooth curves for conditional quantiles
    ggplot2::geom_line(data = curves_point %>%
                         # Remove cases where the point estimate is NA because outside the range
                         filter(!is.na(estimate)),
                       ggplot2::aes(x = x, y = estimate, color = percentile)) +
    # Modify the color scale
    ggthemes::scale_color_colorblind(
      guide = ggplot2::guide_legend(reverse = TRUE, label.position = "left",
                                    title = "Percentile")
    ) +
    ggplot2::scale_x_continuous(breaks = x_breaks,
                                labels = x_labels) +
    ggplot2::scale_y_continuous(breaks = y_breaks,
                                minor_breaks = c(y_axis_range[1], y_minor_grid_lines),
                                labels = y_labels) +
    # Define the axis limits
    ggplot2::coord_cartesian(
      xlim = x_axis_range,
      ylim = y_axis_range,
      expand = TRUE,
      default = FALSE,
      clip = "on"
    ) +
    # Add axis labels
    ggplot2::labs(x  = xlab, y = ylab) +
    ggplot2::theme_bw()

  # If confidence bands were requested, then add them
  if (show_ci) {
    p <- p +
      ggplot2::geom_ribbon(data = curves_point,
                           ggplot2::aes(x = x, ymin = ci.min, ymax = ci.max,
                                        group = percentile),
                           fill = "gray", alpha = .4)
  }

  # If labels on the lines are requested, then use those instead of colors
  if (quantile_notation == "label") {
    # The suppressMessages() below just stops the message that ggplot2 is replacing
    # an existing scale and coordinate system. Those replacements are intentional
    # here, and the message might confuse a user.
    p <- suppressMessages(
      p +
        ggplot2::geom_text(data = curves_point %>%
                             filter(x == max(x)),
                           ggplot2::aes(x = x + .02 * diff(x_data_range),
                                        y = estimate,
                                        hjust = 0,
                                        label = paste0(percentile," percentile")),
                           size = 3) +
        ggplot2::scale_color_manual(values = rep("black",length(quantiles))) +
        # Expand the x range to make space for those labels
        ggplot2::coord_cartesian(
          xlim = x_axis_range,
          ylim = y_axis_range,
          expand = TRUE,
          default = FALSE,
          clip = "on"
        ) +
        ggplot2::theme(legend.position = "none")
    )
  }

  # If the x_data_range and/or y_data_range are truncated, note that in the plot
  pretty_pct <- function(raw_pct) {
    ifelse(raw_pct < .01, "< 1 %",
           paste0(round(100 * raw_pct)," %"))
  }
  truncation <- truncation_amounts %>%
    mutate(x_low_any = x_low_pct > 0,
           x_high_any = x_high_pct > 0,
           y_low_any = y_low_pct > 0,
           y_high_any = y_high_pct > 0) %>%
    reshape2::melt(id = NULL) %>%
    tidyr::separate(variable, into = c("variable","end","quantity")) %>%
    tidyr::pivot_wider(names_from = "quantity", values_from = "value") %>%
    dplyr::mutate(label_no_pct = dplyr::case_when(any == 0 ~ "",
                                                  variable == "x" ~ "Horizontal space truncated",
                                                  variable == "y" ~ "Vertical space truncated"),
                  label = case_when(any == 0 ~ "",
                                    T ~ paste0(label_no_pct,": ",pretty_pct(pct)))) %>%
    # Determine the x position of the annotation
    dplyr::mutate(x_position = dplyr::case_when(variable == "x" & end == "low" ~ x_data_range[1],
                                                variable == "x" & end == "high" ~ x_data_range[2],
                                                variable == "y" & end == "low" ~ mean(x_data_range),
                                                variable == "y" & end == "high" ~ mean(x_data_range))) %>%
    # Determine the y position of the annotation
    dplyr::mutate(y_position = dplyr::case_when(variable == "x" & end == "low" ~ mean(y_data_range),
                                                variable == "x" & end == "high" ~ mean(y_data_range),
                                                variable == "y" & end == "low" ~ y_data_range[1],
                                                variable == "y" & end == "high" ~ y_data_range[2])) %>%
    # Determine the angle of the annotation
    dplyr::mutate(angle = dplyr::case_when(variable == "x" ~ 90,
                                           variable == "y" ~ 0)) %>%
    # Determine the justification of the annotation
    # to place the annotation outside of the dashed line box
    dplyr::mutate(vjust = dplyr::case_when(variable == "x" & end == "low" ~ -.5,
                                           variable == "x" & end == "high" ~ 1.5,
                                           variable == "y" & end == "low" ~ 1.5,
                                           variable == "y" & end == "high" ~ -.5))

  # Add dashed lines for truncation to the plot
  if (truncation_notation != "none") {
    p <- p +
      ggplot2::geom_vline(xintercept = truncation$x_position[truncation$variable == "x"],
                          linetype = "dashed", color = "gray",
                          # The alpha = any makes this completely transparent
                          # if there was no truncation at this end of the range
                          alpha = truncation$any[truncation$variable == "x"]) +
      ggplot2::geom_hline(yintercept = truncation$y_position[truncation$variable == "y"],
                          linetype = "dashed", color = "gray",
                          # The alpha = any makes this completely transparent
                          # if there was no truncation at this end of the range
                          alpha = truncation$any[truncation$variable == "y"])
  }
  # Add labels for truncation to the plot
  if (truncation_notation == "label") {
    p <- p +
      ggplot2::geom_text(data = truncation,
                         ggplot2::aes(x = x_position, y = y_position,
                                      angle = angle, vjust = vjust,
                                      label = label),
                         size = 3, color = "gray")
  }
  if (truncation_notation == "label_no_pct") {
    p <- p +
      ggplot2::geom_text(data = truncation,
                         ggplot2::aes(x = x_position, y = y_position,
                                      angle = angle, vjust = vjust,
                                      label = label_no_pct),
                         size = 3, color = "gray")
  }

  # If requested, include a row at the bottom with the simulated curves for uncertainty
  if (!is.null(uncertainty_draws)) {
    sim_curve_plots <- lapply(quantiles, function(q) {
      sim_this_q <- ggplot2::ggplot() +
        # Simulated estimates
        ggplot2::geom_line(data = curves %>%
                             filter(curve != "point") %>%
                             filter(percentile_num == 100 * q),
                           ggplot2::aes(x = x, y = estimate, group = curve),
                           color = "gray", size = .1) +
        # Point estimate
        ggplot2::geom_line(data = curves %>%
                             filter(curve == "point") %>%
                             filter(percentile_num == 100 * q),
                           ggplot2::aes(x = x, y = estimate),
                           color = "black", size = .1) +
        # Define the axis limits
        ggplot2::coord_cartesian(
          xlim = x_axis_range,
          ylim = y_axis_range,
          expand = TRUE,
          default = FALSE,
          clip = "on"
        ) +
        ggplot2::scale_x_continuous(breaks = labeling::extended(x_data_range[1], x_data_range[2], 3),
                                    labels = x_labels) +
        ggplot2::scale_y_continuous(breaks = labeling::extended(y_data_range[1], y_data_range[2], 3),
                                    labels = y_labels) +
        ggplot2::theme_bw() +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 8),
                       axis.title = ggplot2::element_blank()) +
        ggplot2::ggtitle(paste0(100 * q,"th percentile"))
      return(sim_this_q)
    })
  } else {
    sim_curve_plots <- NULL
  }
  return_object <- list(
    plot = p,
    sim_curve_plots = sim_curve_plots,
    densities = densities,
    curves = gen_curves.out$curves,
    mqgam.out = gen_curves.out$mqgam.out,
    x_data_range = x_data_range,
    y_data_range = y_data_range,
    slice_x_values = unique(densities$conditional$x),
    call = match.call(),
    arguments = arguments
  )
  class(return_object) <- "quantileplot"
  cat("Done.\n")
  return(return_object)
}

