#' Summary function for \code{quantileplot} objects
#' @description Summarizes the S3 class object returned by the quantileplot function
#' @param object An object of class \code{quantileplot}, which results from a call to the function \code{quantileplot}
#' @param ... Other arguments to \code{summary} commands
#' @return Prints a summary of the estimates.
#' @references Lundberg, Ian, Robin C. Lee, and Brandon M. Stewart. 2021. "The quantile plot: A visualization for bivariate population relationships." Working paper.
#' @references Lundberg, Ian, and Brandon M. Stewart. 2020. "Comment: Summarizing income mobility with multiple smooth quantiles instead of parameterized means." Sociological Methodology 50(1):96-111.
#' @references Fasiolo, Matteo, Simon N. Wood, Margaux Zaffran, Raphaël Nedellec, and Yannig Goude. 2020. "Fast calibrated additive quantile regression." Journal of the American Statistical Association.
#' @export

summary.quantileplot <- function(object, ...) {

  # Return the call to the user
  cat("Call:\n")
  print(object$call)

  # Say a bit about the plot
  cat("\nA quantileplot with the following elements:\n")
  cat("\n1. A horizontal density for the predictor\n")
  cat(paste0("\n2. Vertical densities for the conditional outcome distribution at predictor values:\n   ",
             paste0(format(object$slice_x_values, digits = 3), collapse = ", "),"\n"))
  cat(paste0("\n3. Curves for conditional quantiles of the outcome given the predictor, at quantiles:\n   ",
             paste0(names(object$mqgam.out$fit), collapse = ", "),"\n"))

  cat(paste0("\nDensities are estimated by a Gaussian product kernel with horizontal bandwidth ",
            format(object$densities$x_bw, digits = 3),
            " and vertical bandwidth ",
            format(object$densities$y_bw, digits = 3),
            ".\n"))

  # Summarize convergence of the mqgam
  converged <- sapply(object$mqgam.out$fit, function(each_fit) each_fit$converged)
  if (all(converged)) {
    cat("\nConvergence achieved for every quantile curve.\n")
  }
  if (any(!converged)) {
    cat("\nWarning: convergence not achieved for the following quantile curves.\n")
    print(names(converged[!converged]))
    cat("\nIt is possible that you have requested a very extreme quantile for which there is very little data.\n")
    cat("See the mqgam function in the qgam package to address this problem.")
  }

  # Print the plot
  if (object$arguments$uncertainty_draws) {
    print(object$with_uncertainty_draws)
  } else {
    print(object$plot)
  }
}
