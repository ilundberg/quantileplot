% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatter.quantileplot.R
\name{scatter.quantileplot}
\alias{scatter.quantileplot}
\title{Create a Scatter for a Quantile Plot}
\usage{
scatter.quantileplot(object, fraction = 1, ...)
}
\arguments{
\item{object}{An object of class \code{quantileplot}, created by a call to \code{quantileplot()}.}

\item{fraction}{Numeric scalar in (0,1]. Fraction of points to be randomly sampled for inclusion in the scatter plot. Useful in very large data sets.}

\item{...}{Other arguments}
}
\value{
A ggplot2 object containing a scatter plot.
}
\description{
Converts a quantile plot into an analogous scatter plot.
}
\examples{
x <- rbeta(1000,1,2)
y <- log(1 + 9 * x) * rbeta(1000, 1, 2)
data <- data.frame(x = x, y = y)
qp <- quantileplot(y ~ s(x), data)
scatter.quantileplot(qp)
}
\references{
Lundberg, Ian, Robin C. Lee, and Brandon M. Stewart. 2021. "The quantile plot: A visualization for bivariate population relationships." Working paper.

Lundberg, Ian, and Brandon M. Stewart. 2020. "Comment: Summarizing income mobility with multiple smooth quantiles instead of parameterized means." Sociological Methodology 50(1):96-111.

Fasiolo, Matteo, Simon N. Wood, Margaux Zaffran, Raphaël Nedellec, and Yannig Goude. 2020. "Fast calibrated additive quantile regression." Journal of the American Statistical Association.
}
