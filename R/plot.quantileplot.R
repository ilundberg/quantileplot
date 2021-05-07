#' Plot function for \code{quantileplot} objects
#' @description Plots a \code{quantileplot} object.
#' @param x An object of class \code{quantileplot}, which results from a call to the function \code{quantileplot}
#' @param bottom_xlab_angle Numeric degrees to rotate x-axis labels
#' @param bottom_height Proportion of vertical space in the full plot to be taken up by uncertainty draws at the bottom (only relevant if the \code{quantileplot} includes uncertainty draws).
#' @param ... Other arguments to \code{plot} commands
#' @return Prints a plot. If the \code{quantileplot} was specified with option \code{uncertainty_draws = TRUE}, this is a \code{gtable} object with a panel of plots. If the \code{quantileplot} was specified with \code{uncertainty_draws = FALSE} (the default), this is a \code{ggplot2} object.
#' @references Lee, Robin C., Ian Lundberg, and Brandon M. Stewart. 2021. "Smooth quantile visualizations enhance understanding of bivariate population distributions." Working paper.
#' @references Lundberg, Ian, and Brandon M. Stewart. 2020. "Comment: Summarizing income mobility with multiple smooth quantiles instead of parameterized means." Sociological Methodology 50(1):96-111.
#' @references Fasiolo, Matteo, Simon N. Wood, Margaux Zaffran, Raphaël Nedellec, and Yannig Goude. 2020. "Fast calibrated additive quantile regression." Journal of the American Statistical Association.
#' @export

plot.quantileplot <- function(x, bottom_xlab_angle = NULL, bottom_height = .3, ...) {
  if (is.null(x$arguments$uncertainty_draws)) {
    print(x$plot)
  } else {
    if (!is.numeric(bottom_height)) {
      stop("Argument error: Bottom height should be between 0 and 1 (a proportion of total plot height)")
    }
    if (bottom_height < 0 | bottom_height > 1) {
      stop("Argument error: Bottom height should be between 0 and 1 (a proportion of total plot height)")
    }
    all_plots <- x$sim_curve_plots
    if (!is.null(bottom_xlab_angle)) {
      for (i in 1:length(all_plots)) {
        all_plots[[i]] <- all_plots[[i]] +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = bottom_xlab_angle, hjust = 1))
      }
    }
    all_plots$point <- x$plot
    with_uncertainty_draws <- gridExtra::grid.arrange(grobs = all_plots,
                                                      heights = c(1 - bottom_height, bottom_height),
                                                      layout_matrix = rbind(length(x$sim_curve_plots) + 1,
                                                                            1:length(x$sim_curve_plots)))

    print(with_uncertainty_draws)
  }
}
