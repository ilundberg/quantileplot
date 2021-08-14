#' Generate Smooth Curves for a \code{quantileplot}
#'
#' @description Estimate smooth curves for quantiles of the outcome as a function of the predictor. This function calls \code{mqgam} from the \code{qgam} package. This function is typically called indirectly via a user call to \code{\link{quantileplot}}.
#' @param formula A bivariate model formula (e.g. \code{y ~ x})
#' @param second_formula Model formula to allow the learning rate to change as a function of the predictor. This is passed to \code{mqgam} as the second element in the \code{form} argument. Defaults to the same specification as \code{formula} but without the outcome variable.
#' @param data Data frame containing the variables in \code{formula}. If \code{weights} are specified, they must be a column of \code{data}.
#' @param weights String name for sampling weights, which are a column of \code{data}. If not given, a simple random sample is assumed.
#' @param quantiles Numeric vector containing quantiles to be estimated. Values should be between 0 and 1.
#' @param show_ci Logical, defaults to \code{FALSE}. Whether to show credible intervals for the estimated smooth quantile curves.
#' @param credibility_level Numeric probability value for credible intervals; default to 0.95 to produce 95 percent credible intervals. Only relevant if \code{show_ci = TRUE}.
#' @param uncertainty_draws Numeric. If non-null, the number of simulated posterior draws to estimate for each smooth quantile curve. When used with the \code{plot} function, these appear in panels below the main plot.
#' @param inverse_transformation A function of an argument named x. This is only used if the argument passed to formula involves a transformation of the outcome variable (e.g. log(y + 1)), then you need to provide the inverse of that transformation so that the returned plot can be visualized on the original scale of the outcome variable. For common transformations (e.g. log(y)), this argument can be determined automatically. To produce a plot with the predictor or outcome visualized on a transformed scale, you should not place the transformation within the model formula but instead should create your transformed variable in the data before calling the quantileplot function.
#' @param argGam Additional arguments to the GAM for model fitting. Passed to mqgam.
#' @param ... Other arguments passed to \code{mqgam} for fitting of smooth quantile curves.
#' @return A list of length 2. Element \code{curves} is a data frame containing the data for plotting smooth curves for quantiles of the outcome given the predictor. Element \code{mqgam.out} is the fitted object from \code{mqgam}.
#' @references Lundberg, Ian, Robin C. Lee, and Brandon M. Stewart. 2021. "The quantile plot: A visualization for bivariate population relationships." Working paper.
#' @references Lundberg, Ian, and Brandon M. Stewart. 2020. "Comment: Summarizing income mobility with multiple smooth quantiles instead of parameterized means." Sociological Methodology 50(1):96-111.
#' @references Fasiolo, Matteo, Simon N. Wood, Margaux Zaffran, Raphaël Nedellec, and Yannig Goude. 2020. "Fast calibrated additive quantile regression." Journal of the American Statistical Association.
#'
#' @export
#' @import dplyr
gen_curves <- function(formula,
                       second_formula,
                       data,
                       weights = NULL,
                       quantiles = c(.1, .25, .5, .75, .9),
                       show_ci = FALSE,
                       credibility_level = 0.95,
                       uncertainty_draws = 10,
                       inverse_transformation = NULL,
                       argGam = NULL,
                       ...) {
  # Initialize objects that will be called by non-standard evaluation.
  i <- curve <- x <- estimate <- se <- NULL

  # Extract the x variable name from the formula
  x_str <- all.vars(formula)[2]

  # See if any smooth terms used in the formula
  contains_smooth_terms <- any(grepl("s[(]",as.character(formula)))

  # Extract any transformations on the y variable
  outcome_with_transformation <- as.character(formula)[2]
  outcome_variable <- all.vars(formula)[1]
  # Determine the inverse of the transformation on the y-variable.
  # We will use this to return estimated curves to the untransformed y-scale.
  if (is.null(inverse_transformation)) {
    if (outcome_with_transformation == outcome_variable) {
      inverse_transformation <- identity
    } else if (outcome_with_transformation == paste0("log(",outcome_variable,")")) {
      inverse_transformation <- exp
    } else if (outcome_with_transformation == paste0("exp(",outcome_variable,")")) {
      inverse_transformation <- log
    } else if (outcome_with_transformation == paste0("sqrt(",outcome_variable,")")) {
      inverse_transformation <- function(x) x ^ 2
    } else {
      stop(paste("Your model formula uses a transformation",outcome_with_transformation,". This transformation will be used for fitting quantile curves, but you need to provide the inverse_transformation argument so that we can return the plot on the original scale."))
    }
  }

  # Estimate the smooth quantile curves
  if (is.null(weights) & contains_smooth_terms) {
    mqgam.out <- qgam::mqgam(list(formula, second_formula),
                             data = data, qu = quantiles,
                             argGam = argGam,
                             ...)
  } else if (!is.null(weights) & contains_smooth_terms) {
    mqgam.out <- qgam::mqgam(list(formula, second_formula),
                             data = data, qu = quantiles,
                             # When passing in the weights, normalize them to sum to the number of observations
                             argGam = c(argGam, list(weights = data[[weights]] / mean(data[[weights]]))),
                             ...)
  } else if (is.null(weights) & !contains_smooth_terms) {
    mqgam.out <- qgam::mqgam(formula,
                             data = data, qu = quantiles,
                             argGam = argGam,
                             ...)
  } else if (!is.null(weights) & !contains_smooth_terms) {
    mqgam.out <- qgam::mqgam(formula,
                             data = data, qu = quantiles,
                             argGam = c(argGam, list(weights = data[[weights]] / mean(data[[weights]]))),
                             ...)
  }

  # Split the x range into 200 points where we will make predictions
  to_predict <- data.frame(x = seq(min(data %>% group_by() %>% dplyr::select_at(x_str)),
                                   max(data %>% group_by() %>% dplyr::select_at(x_str)),
                                   length.out = 200)) %>%
    rename_with(.fn = function(x) x_str)

  # Generate point estimate and simulated curves
  quantile_curves_list <- lapply(quantiles, function(q) {
    quantile_index <- which(quantiles == q)
    # Get the model matrix on the expanded basis
    if (!contains_smooth_terms) {
      X_basis_expansion <- mgcv::predict.gam(mqgam.out$fit[[1]],
                                             type = "lpmatrix",
                                             newdata = to_predict)
    } else {
      # For smooth terms, need a different function to predict the model matrix
      # cbind adds the intercept to the smooth
      X_basis_expansion <- cbind(1,mgcv::PredictMat(mqgam.out$smooth[[1]],
                                                    data = to_predict))
    }
    # Extract the fitted coefficients beta
    beta <- mqgam.out$fit[[quantile_index]]$coefficients
    # Extract the variance-covariance matrix Sigma
    rV <- mqgam.out$fit[[quantile_index]]$rV
    sig2 <- mqgam.out$fit[[quantile_index]]$sig2
    Sigma <- rV %*% t(rV) * sig2

    # Calculate the point estimate and standard error
    yhat_point <- X_basis_expansion %*% beta
    yhat_se <- sqrt(diag(X_basis_expansion %*% Sigma %*% t(X_basis_expansion)))
    # Create a data frame of the point estimate
    point_df <- data.frame(curve = "point",
                           x = to_predict[[1]],
                           estimate = inverse_transformation(yhat_point),
                           ci.min = inverse_transformation(yhat_point - stats::qnorm((1 + credibility_level) / 2) * yhat_se),
                           ci.max = inverse_transformation(yhat_point + stats::qnorm((1 + credibility_level) / 2) * yhat_se))

    # Calculate uncertainty_draws simulated curves
    if (!is.null(uncertainty_draws)) {
      beta_star <- t(mvtnorm::rmvnorm(uncertainty_draws, mean = beta, sigma = Sigma))
      yhat_star <- inverse_transformation(X_basis_expansion %*% beta_star)
      # Create a data frame of the simulated curves
      sim_curves_df <- data.frame(yhat_star) %>%
        dplyr::rename_all(.funs = function(x) gsub("X","sim",x)) %>%
        dplyr::mutate(x = to_predict[[1]]) %>%
        reshape2::melt(id = "x", variable.name = "curve", value.name = "estimate") %>%
        dplyr::select(curve, x, estimate) %>%
        mutate(ci.min = NA, ci.max = NA)
    }
    to_return <- point_df
    if (!is.null(uncertainty_draws)) {
      to_return <- to_return  %>%
        dplyr::bind_rows(sim_curves_df)
    }
    to_return <- to_return %>%
      dplyr::mutate(Form = "Smooth",
                    Summary = "quantiles",
                    percentile = paste0(100 * q,"th"),
                    percentile_num = 100 * q)

    return(to_return)
  })
  # Convert the result to a data frame
  quantile_curves_df <- do.call(rbind, quantile_curves_list)

  return(list(curves = quantile_curves_df,
              mqgam.out = mqgam.out))
}

