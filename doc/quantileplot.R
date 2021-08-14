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
quantileplot(y ~ s(x), data = sim_data, uncertainty_draws = 10)

## ---- results = F, fig.height = 5---------------------------------------------
qp <- quantileplot(y ~ s(x), data = sim_data)
scatter.quantileplot(qp, fraction = .5)

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
  # Denote quantiles by colors with labels instead of colors
  quantile_notation = "label"
)

## ---- results = F-------------------------------------------------------------
quantileplot(y ~ s(x), 
             data = sim_data,
             x_data_range = c(0,.5),
             y_data_range = c(0,1),
             show_ci = T)

## ---- results = F-------------------------------------------------------------
library(ggplot2)
my_plot <- quantileplot(y ~ s(x), data = sim_data)
my_plot$plot +
  ggtitle("A custom title for the plot") +
  theme_light() +
  scale_color_manual(values = rainbow(5),
                     guide = guide_legend(reverse = TRUE, label.position = "left",
                                          title = "Custom\nlegend\ntitle and\ncolors")) +
  xlab(expression(Custom~axis~title~could~have~something~bold(bold))) +
  scale_y_continuous(breaks = c(0,1,2),
                     labels = c("Custom\nlabel at 0",
                                "Another\ncustom\nlabel at 1",
                                "Custom\nlabel at 2"),
                     name = "Custom y-axis\nbreaks and\nrotated title") +
  theme(axis.title.y = element_text(angle = 0, vjust = .5))

## ---- results = F-------------------------------------------------------------
my_plot <- quantileplot(y ~ s(x), data = sim_data, quantile_notation = "label")
my_plot$plot +
  coord_cartesian(xlim = c(0,2)) +
  annotate(geom = "label", x = 1.75, y = 1,
           label = "Extra space\nadded with\ncoord_cartesian()\nto allow more\nroom for\nannotations.",
           size = 3)

## ---- results = "hide"--------------------------------------------------------
quantileplot(y ~ s(x), data = sim_data, previous_fit = my_plot)

## ---- results = F-------------------------------------------------------------
quantileplot(y ~ s(x), data = sim_data, lsig = log(10))
quantileplot(y ~ s(x), data = sim_data, lsig = log(.01))

