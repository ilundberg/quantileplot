# quantileplot

An R Package to visualize bivariate relationships with a quantile plot.

Install this package with the command `devtools::install_github("ilundberg/quantileplot")`.

**Warning:** This package is actively being developed. Functionality is likely to change in the near future. You should think of it as a directory of code that exists on Github. If you use any of this code (or the package as a whole), you should store a copy of the version you are using so that you can reproduce your results. The package as available here on Github will change. This warning will be updated when the package is stable.

## Sample Use
`quantileplot(y ~ s(x), data)`

```
x <- rbeta(1000,1,2)
y <- log(1 + 9 * x) * rbeta(1000, 1, 2)
data <- data.frame(x = x, y = y)
quantileplot(y ~ s(x), data)
```

![Output of the quantileplot function](example.png)
