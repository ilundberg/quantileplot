#' Create a Smooth Quantile Plot
#'
#' @description Creates a bivariate, smooth quantile plot. This is the central function of the \code{quantileplot} package. This plot visualizes estimates of the marginal density of the predictor, the conditional density of the outcome at selected values of the predictor, and smooth curves showing quantiles of the outcome as smooth functions of the predictor. This package is described in greater depth by Lee et al. (2021), which is a generalization of Lundberg and Stewart (2020). The statistical core of the package relies on the methods of Fasiolo et al. (2020).
#' @param formula A bivariate model formula (e.g. \code{y ~ x})
#' @param data Data frame containing the variables in \code{formula}. If \code{weights} are specified, they must be a column of \code{data}.
#' @param weights String name for sampling weights, which are a column of \code{data}. If not given, a simple random sample is assumed.
#' @param xlab String x-axis title
#' @param ylab String y-axis title
#' @param x_break_labeller Function to convert labels on x-axis breaks into an alternative format.
#' @param y_break_labeller Function to convert labels on y-axis breaks into an alternative format.
#' @param slice_n Integer number of vertical slices (conditional densities of y given x) to be plotted. Default is 7.
#' @param quantiles Numeric vector containing quantiles to be estimated. Values should be between 0 and 1.
#' @param quantile_notation String, either \code{label} or \code{legend}. If \code{label} (the default), then quantile curves are annotated in the plot. If \code{legend}, then quantile curves are denoted by colors with a legend.
#' @param truncation_notation String, one of \code{label}, \code{label_no_pct}, or \code{none}. If \code{x_range} or \code{y_range} is narrower than the range of the data, this argument specifies how to note that truncation on the visualization. If \code{label}, then truncation is labeled including the percent of data truncated. If \code{label_no_pct}, then truncation is labeled but the percent truncated is omitted. If \code{none}, then truncation is not labeled on the plot.
#' @param uncertainty_draws Numeric. If non-null, the number of simulated posterior draws to estimate for each smooth quantile curve. When used with the \code{plot} function, these appear in panels below the main plot.
#' @param show_ci Logical, defaults to \code{FALSE}. Whether to show credible intervals for the estimated smooth quantile curves.
#' @param ci Numeric probability value for credible intervals; default to 0.95 to produce 95 percent credible intervals. Only relevant if \code{show_ci = TRUE}.
#' @param second_formula Model formula to allow the learning rate to change as a function of the predictor. This is passed to \code{mqgam} as the second element in the \code{form} argument. Defaults to the same specification as \code{formula} but without the outcome variable.
#' @param x_range Numeric vector of length 2 containing the range of horizontal values to be plotted. Defaults to the range of the predictor variable in \code{data}. You may want to specify a narrower range if the predictor is extremely skewed.
#' @param y_range Numeric vector of length 2 containing the range of vertical values to be plotted. Defaults to the range of the outcome variable in \code{data}. You may want to specify a narrower range if the outcome is extremely skewed.
#' @param xlim Numeric vector of length 2 for custom x-axis limits. This affects the plotting area but does not affect the data analyzed or displayed. To truncate the data, use \code{x_range}.
#' @param ylim Numeric vector of length 2 for custom y-axis limits. This affects the plotting area but does not affect the data analyzed or displayed. To truncate the data, use \code{y_range}.
#' @param x_bw Numeric bandwidth for density estimation in the \code{x} dimension. The standard deviation of a Gaussian kernel. If \code{NULL}, this is set by the defaults in \code{stats::density()}.
#' @param y_bw Numeric bandwidth for density estimation in the \code{y} dimension. The standard deviation of a Gaussian kernel. If \code{NULL}, this is set by the defaults in \code{stats::density()}.
#' @param inverse_transformation A function of an argument named x. This is only used if the argument passed to formula involves a transformation of the outcome variable (e.g. log(y + 1)), then you need to provide the inverse of that transformation so that the returned plot can be visualized on the original scale of the outcome variable. For common transformations (e.g. log(y)), this argument can be determined automatically. To produce a plot with the predictor or outcome visualized on a transformed scale, you should not place the transformation within the model formula but instead should create your transformed variable in the data before calling the quantileplot function.
#' @param granularity Integer number of points at which to evaluate each density. Defaults to 512, as in \code{stats::density()}. Higher values yield more granular density estimates.
#' @param previous_fit The result of a previous call to \code{quantileplot}. If provided, then the \code{mqgam} fit for the quantile curves will not be re-estimated, which can be useful for iteratively deciding about other arguments in settings that are computationally demanding. This argument must be paired with other arguments that match the previous call (e.g. \code{data}, \code{formula}).
#' @param argGam Additional arguments to the GAM for model fitting. Passed to mqgam.
#' @param ... Other arguments passed to \code{mqgam} for fitting of smooth quantile curves.
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
#' \item \code{x_range} and \code{y_range} are the horizontal and vertical ranges of the plot.
#' \item \code{slice_x_values} are the predictor values at which vertical conditional densities are estimated.
#' \item \code{call} is the user's call that produced these results.
#' \item \code{arguments} is a list of all the arguments to the function, including those specified by the user and those specified by defaults.
#' }
#'
#' @references Lee, Robin C., Ian Lundberg, and Brandon M. Stewart. 2021. "Smooth quantile visualizations enhance understanding of bivariate population distributions." Working paper.
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

quantileplot <- function(formula, data, weights = NULL, xlab = NULL, ylab = NULL, x_break_labeller = NULL, y_break_labeller = NULL, slice_n = 7, quantiles = c(.1, .25, .5, .75, .9), quantile_notation = "label", truncation_notation = "label", uncertainty_draws = NULL, show_ci = FALSE, ci = 0.95, second_formula = NULL, x_range = NULL, y_range = NULL, xlim = NULL, ylim = NULL, x_bw = NULL, y_bw = NULL, inverse_transformation = NULL, granularity = 512, previous_fit = NULL, argGam = NULL, ...) {

  # Make a list of all arguments, to return at end of the function
  arguments <- list(formula = formula,
                    data = data,
                    weights = weights,
                    xlab = xlab,
                    ylab = ylab,
                    x_break_labeller = x_break_labeller,
                    y_break_labeller = y_break_labeller,
                    slice_n = slice_n,
                    quantiles = quantiles,
                    quantile_notation = quantile_notation,
                    truncation_notation = truncation_notation,
                    uncertainty_draws = uncertainty_draws,
                    show_ci = show_ci,
                    ci = ci,
                    second_formula = second_formula,
                    x_range = x_range,
                    y_range = y_range,
                    xlim = xlim,
                    ylim = ylim,
                    x_bw = x_bw,
                    y_bw = y_bw,
                    inverse_transformation = inverse_transformation,
                    granularity = granularity,
                    previous_fit = previous_fit,
                    argGam = argGam)

  ################
  # Check inputs #
  ################

  if (!rlang::is_formula(formula)) {
    stop("Argument error: formula should be a model formula")
  }
  if (length(all.vars(formula)) != 2) {
    stop("Argument error: formula should involve one predictor and one outcome")
  }
  if (!is.data.frame(data)) {
    stop("Argument error: data should be a data frame")
  }
  # Extract strings for the x and y variables
  y_str <- all.vars(formula)[1]
  x_str <- all.vars(formula)[2]
  # Create axis labels if they were not given
  if (is.null(xlab)) {
    xlab <- x_str
  }
  if (is.null(ylab)) {
    ylab <- y_str
  }
  if (!is.null(x_break_labeller)) {
    if (!is.function(x_break_labeller)) {
      stop("Argument error: x_break_labeller must be a function")
    }
  }
  if (!is.null(y_break_labeller)) {
    if (!is.function(y_break_labeller)) {
      stop("Argument error: y_break_labeller must be a function")
    }
  }
  # If not provided, set the labellers to the identity function
  if (is.null(x_break_labeller)) {
    x_break_labeller <- identity
  }
  if (is.null(y_break_labeller)) {
    y_break_labeller <- identity
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
  if (!is.numeric(ci) | length(ci) != 1) {
    stop("Argument error: ci should be a numeric scalar")
  }
  if (ci <= 0 | ci >= 1) {
    stop("Argument error: ci should be a credible value between 0 and 1")
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
  if (!is.null(x_range)) {
    if (!is.numeric(x_range) | length(x_range) != 2) {
      stop("Argument error: x_range should be a numeric vector of length 2")
    }
    if (x_range[1] >= x_range[2]) {
      stop("Argument error: The first number in x_range should be less than the second number")
    }
  }
  if (!is.null(y_range)) {
    if (!is.numeric(y_range) | length(y_range) != 2) {
      stop("Argument error: y_range should be a numeric vector of length 2")
    }
    if (y_range[1] >= y_range[2]) {
      stop("Argument error: The first number in y_range should be less than the second number")
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

  # Initialize objects that will be called by non-standard evaluation.
  # This is only to stop a warning message in package building.
  x <- y <- estimate <- curve <- percentile <- percentile_num <-
    ci.max <- ci.min <- index <- num <- ymax <- ymin <- weight <-
    variable <- x_position <- y_position <- angle <- vjust <-
    label <- label_no_pct <- density <- NULL
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

  # Get rid of missing values
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
                           weight = 1 / nrow(data))
  } else {
    data.mod <- data.frame(x = data[[x_str]],
                           y = data[[y_str]],
                           weight = data[[weights]] / mean(data[[weights]]))
  }
  # Remove missing values from the data,
  # having warned of this above
  data.mod <- stats::na.omit(data.mod)

  ######################
  # Conduct estimation #
  ######################

  # Make a restricted version of the data that only has the predictor range of interest
  if (!is.null(x_range)) {
    data.restricted <- data[data[[x_str]] >= x_range[1] & data[[x_str]] <= x_range[2],]
  } else {
    data.restricted <- data
  }

  # Create a data frame of smooth curve predictions
  if (is.null(previous_fit)) {
    cat("Beginning quantile curve estimation via mqgam.\n")
    cat("Note that this may take time. It is computationally much harder than OLS.\n")
    gen_curves.out <- gen_curves(formula = formula,
                                 second_formula = second_formula,
                                 data = data.restricted,
                                 weights = weights,
                                 quantiles = quantiles,
                                 ci = ci,
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

  # Determine the x_range and y_range if NULL
  # This step is conducted here because it those
  # are affected by curve estimation and inputs to density estimation.
  x_range_provided <- !is.null(x_range)
  y_range_provided <- !is.null(y_range)
  # Set x_range to be the range of the data
  if (is.null(x_range)) {
    x_range <- range(data.mod$x)
  }
  # Set y_range to contain all of the observed data
  # and all of the curve estimates
  if (is.null(y_range)) {
    curve_y_range <- curves %>%
      dplyr::filter(curve == "point") %>%
      dplyr::summarize(min = min(ci.min),
                       max = max(ci.max))
    y_range <- c(min(c(data.mod$y, curves$ci.min[curves$curve == "point"])),
                 max(c(data.mod$y, curves$ci.max[curves$curve == "point"])))
  }

  # Generate density estimates
  cat("Beginning density estimation\n")
  densities <- gen_densities(data = data.mod,
                             slice_n = slice_n,
                             x_range = x_range,
                             y_range = y_range,
                             x_bw = x_bw,
                             y_bw = y_bw,
                             granularity = granularity)

  ############################
  # Below are plotting steps #
  ############################

  # Several inputs need to be modified to produce the plot.
  # These lines accomplish those tasks.

  # 1. Prepare components of the plot area.

  # Find the minimum value of the horizontal grid lines to be plotted.
  # These grid lines avoid having incorrect y-labels in the region where the x-density is.
  # This uses the labeling package that ggplot2 ordinarily calls,
  # with 5 major breaks (the ggplot2 default number).
  major_grid_lines <- labeling::extended(y_range[1], y_range[2], 5)
  minor_grid_lines <- c(min(major_grid_lines) - .5 * mean(diff(major_grid_lines)),
                        major_grid_lines + .5 * mean(diff(major_grid_lines)))
  minor_grid_lines <- minor_grid_lines[minor_grid_lines >= y_range[1] & minor_grid_lines <= y_range[2]]

  # Find the size to be added to the bottom of the y-axis to hold the horizontal density
  y_range_with_breaks <- c(
    ifelse(y_range[1] < min(minor_grid_lines), y_range[1], min(minor_grid_lines)),
    ifelse(y_range[2] > max(minor_grid_lines), y_range[2], max(minor_grid_lines))
  )
  # If there is a note at the bottom, we ned a little extra space there
  if (y_range[1] > min(data.mod$y)) {
    y_bottom_padding <- .03 * diff(y_range_with_breaks)
  } else {
    y_bottom_padding <- 0
  }
  # Prepare an offset space for the marginal density
  y_offset <- 0.2 * diff(y_range_with_breaks)
  # If there is a note at the top, we need a little extra space there
  if (y_range[2] < max(data.mod$y)) {
    y_range_with_breaks[2] <- y_range_with_breaks[2] + .03 * diff(y_range_with_breaks)
  }

  # 2. Prepare densities.

  # Modify the marginal density estimates to prepare for use with geom_ribbon()
  marginal <- densities$marginal %>%
    mutate(x = x,
           ymin = y_range_with_breaks[[1]] - y_offset - y_bottom_padding,
           ymax =  0.8 * (density / max(density)) * y_offset + y_range_with_breaks[[1]] - y_offset - y_bottom_padding)

  # Modify the conditional density estimates to prepare for use with geom_polygon()
  x_spacing <- .9*diff(sort(unique(densities$conditional$x)))[1]
  conditional <- densities$conditional %>%
    # Make the highest point of the highest conditional density
    # equal the amount of space between vertical slices
    mutate(density = density / max(density) * x_spacing) %>%
    # Begin modifying the dataset to have density = 0 at the endpoints
    # to faciliate polygon plotting
    group_by(x) %>%
    arrange(x,y) %>%
    mutate(index = 1:n()) %>%
    group_by() %>%
    bind_rows(densities$conditional %>%
                group_by(x) %>%
                arrange(x,y) %>%
                mutate(num = n()) %>%
                # Keep only the first and last observations
                filter(1:n() %in% c(1,n())) %>%
                # Assign them densities of zero and new indices
                mutate(density = 0,
                       index = ifelse(1:n() == 1, 0, num + 1)) %>%
                select(-num) %>%
                group_by()) %>%
    arrange(x,index)

  # 3. Prepare curves

  # For the main plot, extract the point estimate from the curves object
  # and address issues of estimates beyond the range of the visualization
  curves_point <- curves %>%
    dplyr::filter(curve == "point") %>%
    dplyr::filter(x >= x_range[1] & x <= x_range[2]) %>%
    # Truncate the confidence bands at the edges of the plot.
    # This is ok because it will be noted in the visualization.
    dplyr::mutate(estimate = ifelse(estimate >= y_range[1] & estimate <= y_range[2],
                                    estimate, NA),
                  ci.min = dplyr::case_when(
                    # If ci.min is in range, use ci.min
                    ci.min >= y_range[1] & ci.min <= y_range[2] ~ ci.min,
                    # If the whole band is out of range, use NA
                    #(ci.min < y_range[1] & ci.max < y_range[1]) |
                    #  (ci.min > y_range[2] & ci.max > y_range[2]) ~ NA,
                    # If ci.min is below the range but ci.max is in range, use the bottom of the range
                    ci.min < y_range[1] & ci.max >= y_range[1] & ci.max <= y_range[2] ~ y_range[1],
                    # If the band is the full range, use the full range
                    ci.min < y_range[1] & ci.max > y_range[2] ~ y_range[1]
                  ),
                  ci.max = dplyr::case_when(
                    # If ci.max is in range, use ci.max
                    ci.max >= y_range[1] & ci.max <= y_range[2] ~ ci.max,
                    # If the whole band is out of range, use NA
                    #(ci.min < y_range[1] & ci.max < y_range[1]) |
                    #  (ci.min > y_range[2] & ci.max > y_range[2]) ~ NA,
                    # If ci.max is above the range but ci.min is in range, use the top of the range
                    ci.max > y_range[2] & ci.min >= y_range[1] & ci.min <= y_range[2] ~ y_range[2],
                    # If the band is the full range, use the full range
                    ci.min < y_range[1] & ci.max > y_range[2] ~ y_range[2]
                  ))

  ####################
  # PRODUCE THE PLOT #
  ####################

  # Determine the axis range to be plotted, or use the user-specified range
  if (!is.null(xlim)) {
    # Use user-specified if provided
    x_range_for_plot <- xlim
  } else if (quantile_notation == "legend") {
    # Else, use range of x if we have a legend
    x_range_for_plot <- x_range
  } else {
    # Else, expand the range to make room for the labels
    x_range_for_plot <- x_range + c(0,.25 * diff(x_range))
  }
  if (is.null(ylim)) {
    y_range_for_plot <- c(y_range_with_breaks[[1]] - y_offset - y_bottom_padding, y_range_with_breaks[[2]])
  } else {
    y_range_for_plot <- ylim
  }

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
                         # Remove the estimates that are NA
                         # which would be because we removed them due to being
                         # outside the user-specified y_range.
                         dplyr::filter(!is.na(estimate)),
                       ggplot2::aes(x = x, y = estimate, color = percentile)) +
    # Modify the color scale
    ggthemes::scale_color_colorblind(
      guide = ggplot2::guide_legend(reverse = TRUE, label.position = "left",
                                    title = "Percentile")
    ) +
    ggplot2::scale_x_continuous(breaks = labeling::extended(x_range[1], x_range[2], 4),
                                labels = x_break_labeller) +
    ggplot2::scale_y_continuous(breaks = major_grid_lines,
                                minor_breaks = c(y_range_with_breaks[[1]] - y_offset - y_bottom_padding, minor_grid_lines),
                                labels = y_break_labeller) +
    # Define the axis limits
    ggplot2::coord_cartesian(
      xlim = x_range_for_plot,
      ylim = y_range_for_plot,
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
    # If the user did not specify the x-range, modify the default to make space for the annotations
    if (!x_range_provided) {
      x_range_modified <- x_range + c(0,.25 * diff(x_range))
    } else {
      # If the user specified the range, keep that one.
      x_range_modified <- x_range
    }
    # The suppressMessages() below just stops the message that ggplot2 is replacing
    # an existing scale and coordinate system. Those replacements are intentional
    # here, and the message might confuse a user.
    p <- suppressMessages(
      p +
        ggplot2::geom_text(data = curves_point %>%
                             filter(x == max(x)),
                           ggplot2::aes(x = x + .02 * diff(x_range),
                                        y = estimate,
                                        hjust = 0,
                                        label = paste0(percentile," percentile")),
                           size = 3) +
        ggplot2::scale_color_manual(values = rep("black",length(quantiles))) +
        # Expand the x range to make space for those labels
        ggplot2::coord_cartesian(
          # If the user specified the x_range, use that range.
          # Otherwise expand the range to make space for the annotations.
          xlim = x_range_for_plot,
          ylim = y_range_for_plot,
          expand = TRUE,
          default = FALSE,
          clip = "on"
        ) +
        ggplot2::theme(legend.position = "none")
    )
  }

  # If the x_range and/or y_range are truncated, note that in the plot
  pretty_pct <- function(raw_pct) {
    ifelse(raw_pct < .01, "< 1 %",
           paste0(round(100 * raw_pct)," %"))
  }
  truncation <- data.mod %>%
    dplyr::summarize(x_low_any = any(x < x_range[1]),
                     x_high_any = any(x > x_range[2]),
                     y_low_any = any(y < y_range[1]),
                     y_high_any = any(y > y_range[2]),
                     # Determine amount truncated
                     x_low_pct = stats::weighted.mean(x < x_range[1], w = weight),
                     x_high_pct = stats::weighted.mean(x > x_range[2], w = weight),
                     y_low_pct = stats::weighted.mean(y < y_range[1], w = weight),
                     y_high_pct = stats::weighted.mean(y > y_range[2], w = weight),
                     .groups = "drop") %>%
    reshape2::melt(id = NULL) %>%
    tidyr::separate(variable, into = c("variable","end","quantity")) %>%
    tidyr::pivot_wider(names_from = "quantity", values_from = "value") %>%
    dplyr::mutate(label_no_pct = dplyr::case_when(any == 0 ~ "",
                                                  variable == "x" ~ "Horizontal space truncated",
                                                  variable == "y" ~ "Vertical space truncated"),
                  label = case_when(any == 0 ~ "",
                                    T ~ paste0(label_no_pct,": ",pretty_pct(pct)))) %>%
    # Determine the x position of the annotation
    dplyr::mutate(x_position = dplyr::case_when(variable == "x" & end == "low" ~ x_range[1],
                                                variable == "x" & end == "high" ~ x_range[2],
                                                variable == "y" & end == "low" ~ mean(x_range),
                                                variable == "y" & end == "high" ~ mean(x_range))) %>%
    # Determine the y position of the annotation
    dplyr::mutate(y_position = dplyr::case_when(variable == "x" & end == "low" ~ mean(y_range),
                                                variable == "x" & end == "high" ~ mean(y_range),
                                                variable == "y" & end == "low" ~ y_range[1],
                                                variable == "y" & end == "high" ~ y_range[2])) %>%
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
          xlim = x_range,
          ylim = c(y_range_with_breaks[[1]] - y_offset - y_bottom_padding, y_range_with_breaks[[2]]),
          expand = TRUE,
          default = FALSE,
          clip = "on"
        ) +
        ggplot2::scale_x_continuous(breaks = labeling::extended(x_range[1], x_range[2], 3),
                                    labels = x_break_labeller) +
        ggplot2::scale_y_continuous(breaks = labeling::extended(y_range[1], y_range[2], 3),
                                    labels = y_break_labeller) +
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
    x_range = x_range,
    y_range = y_range,
    slice_x_values = unique(densities$conditional$x),
    call = match.call(),
    arguments = arguments
  )
  class(return_object) <- "quantileplot"
  cat("Done.\n")
  return(return_object)
}

