## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  dpi = 300, 
  fig.width = 6.5, 
  fig.height = 4, 
  out.width = "650px"
)

## -----------------------------------------------------------------------------
library(quantileplot)
x <- rbeta(1000,1,2)
y <- log(1 + 9 * x) * rbeta(1000, 1, 2)
sim_data <- data.frame(x = x, y = y)

## ---- results = F-------------------------------------------------------------
quantileplot(y ~ s(x), data = sim_data)

## ---- results = F-------------------------------------------------------------
quantileplot(y ~ s(x), data = sim_data, show_ci = T)

## ---- results = F, fig.height = 5---------------------------------------------
quantileplot(y ~ s(x), data = sim_data, uncertainty_draws = T)

## ---- results = F-------------------------------------------------------------
quantileplot(
  y ~ s(x), 
  data = sim_data, 
  # Provide axis titles
  xlab = "A name for the predictor", 
  ylab = "A name for the outcome",
  # Customize the number of vertical slices
  slice_n = 3,
  # Customize which quantiles are depicted
  quantiles = c(.3,.5,.7),
  # Denote quantiles by colors with a legend instead of by labels
  quantile_notation = "legend"
)

## ---- results = F-------------------------------------------------------------
quantileplot(y ~ s(x), 
             data = sim_data,
             quantile_notation = "legend",
             x_range = c(0,.5),
             y_range = c(0,1),
             show_ci = T)

## ---- results = F-------------------------------------------------------------
library(ggplot2)
my_plot <- quantileplot(y ~ s(x), data = sim_data, quantile_notation = "legend")
my_plot$plot +
  ggtitle("A custom title for the plot") +
  theme_light() +
  scale_color_manual(values = rainbow(5),
                     guide = guide_legend(reverse = TRUE, label.position = "left",
                                          title = "Custom\nlegend\ntitle and\ncolors")) +
  xlab(expression(Custom~axis~title~could~have~something~bold(bold))) +
  scale_y_continuous(breaks = c(0,1,2),
                     name = "Custom y-axis\nbreaks and\nrotated title") +
  theme(axis.title.y = element_text(angle = 0, vjust = .5))

## ---- results = "hide"--------------------------------------------------------
quantileplot(y ~ s(x), data = sim_data, previous_fit = my_plot)

## ---- results = F-------------------------------------------------------------
quantileplot(y ~ s(x), data = sim_data, lsig = log(10))
quantileplot(y ~ s(x), data = sim_data, lsig = log(.01))

