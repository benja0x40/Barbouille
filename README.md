Barbouille
================================================================================

My collection of plotting and color mapping functions, home cooked with
[base R graphics](https://stat.ethz.ch/R-manual/R-patched/library/graphics/html/00Index.html),
[ash](https://CRAN.R-project.org/package=ash) and
[colorspace](https://cran.r-project.org/package=colorspace).

### Main features

* Empirical distributions: `BivariateDensity`, `SideBySideDensity`
* Piecewise color mapping: `colorize`, `DefineColorMap`, `MakeColors`, `ColorLegend`

Basic documentation for each function is available in the `Barbouille` package
reference manual.  
See also the [code examples](#examples)
and the [installation instructions](#install)
in the following sections.

### <a name="examples"></a>Examples

```r
# Load package
library(Barbouille)
```

#### Empirical distributions

```r
# Create a color mapping function
cmf <- function(k) colorize(k, mode = "01", col = "Wry")
```

With any matrix representing observations of numeric variables,
the `SideBySideDensity` function can represent the empirical distributions
of these variables as juxtaposed density heatmaps.
In the following examples, `x` is a matrix of
50000 observations (rows) x 5 variables (columns named `A`, `B`, `C`, `D`, `E`)
which was randomly generated using mixtures of normal distributions.

```r
# Examples using the default color mapping function
r <- SideBySideDensity(x, nx = 200)                 # Example 1
r <- SideBySideDensity(x, nx = 200, smoothx = 25)   # Example 2
r <- SideBySideDensity(x, nx = 200, method = "ash") # Example 3
```

![](./images/gallery/barbouille_01.png "example")
![](./images/gallery/barbouille_02.png "example")
![](./images/gallery/barbouille_03.png "example")

```r
# Examples using a custom color mapping function (cmf)
r <- SideBySideDensity(x, nx = 200, mapper = cmf, smoothx = 5)                     # Example 1
r <- SideBySideDensity(x, nx = 200, mapper = cmf, jitter = "norm")                 # Example 2
r <- SideBySideDensity(x, nx = 200, mapper = cmf, jitter = "norm", method = "ash") # Example 3
```

![](./images/gallery/barbouille_04.png "example")
![](./images/gallery/barbouille_05.png "example")
![](./images/gallery/barbouille_06.png "example")

```r
# Examples showing the joint distribution of variables A and E from the matrix x
r <- BivariateDensity(x[, c(1, 5)])                               # Example 1
r <- BivariateDensity(x[, c(1, 5)], method = "ash")               # Example 2
r <- BivariateDensity(x[, c(1, 5)], method = "ash", mapper = cmf) # Example 3
```

![](./images/gallery/barbouille_07.png "example")
![](./images/gallery/barbouille_08.png "example")
![](./images/gallery/barbouille_09.png "example")

#### Piecewise color mapping

```r
# Define a common range for the plot regions
rng <- c(-4.5, 4.5)
```

In the following examples, `x` and `y` are two vectors with 2000 observations
randomly generated using the standard normal distribution.

```r
# Variable for color mapping
v <- expression(sqrt(x^2 + y^2))

# Example 1
cmp <- DefineColorMap(thresholds = seq(0, 2, 0.5), colors = grey(c(0.2, 0.8)), above = "red")
ScatterPlot(x, y, clr = eval(v), clr.prm = cmp, xlim = rng, ylim = rng, main = v)
ColorLegend("tl", parameters = cmp, cex = 0.8)

# Example 2
cmp <- DefineColorMap(thresholds = c(0, 2), colors = grey(c(0.2, 1.0)), above = "red", levels = 4)
ScatterPlot(x, y, clr = eval(v), clr.prm = cmp, xlim = rng, ylim = rng, main = v)
ColorLegend("tl", parameters = cmp, ticks = seq(0, 2, length.out = 5), cex = 0.8)
```

![](./images/gallery/barbouille_10.png "example")
![](./images/gallery/barbouille_11.png "example")

```r
# Variable for color mapping
v <- expression(over(180 ~~ atan2(y, x), pi))
a <- 180 * atan2(y, x) / pi

# Example 1
cmp <- DefineColorMap(seq(-180, 180, 60), colors = SuperRainbow(7))
ScatterPlot(x, y, clr = a, clr.prm = cmp, xlim = rng, ylim = rng, main = v)
ColorLegend("t", parameters = cmp, horiz = T, size = c(60, 3), cex = 0.8)

# Example 2
cmp <- UpdateDefinition(cmp, levels = 3)
cmp <- TransformColors(cmp, S.range = 0.6)
ScatterPlot(x, y, clr = a, clr.prm = cmp, xlim = rng, ylim = rng, main = v)
ColorLegend("t", parameters = cmp, horiz = T, size = c(60, 3), cex = 0.8)
```

![](./images/gallery/barbouille_12.png "example")
![](./images/gallery/barbouille_13.png "example")

```r
# Work in progress...
```

![](./images/gallery/barbouille_14.png "example")
![](./images/gallery/barbouille_15.png "example")

### <a name="install"></a>Installation

Run the `R` code below to install `Barbouille`.

```R
library("devtools")
install_github("benja0x40/Barbouille", dependencies = T)
```

If the installation fails, try to install dependencies as indicated
in the following section.

#### Dependencies

  - [R environment](https://www.r-project.org/) version 3.4 or newer
  - To develop and execute `R` scripts we recommend using [RStudio](https://www.rstudio.com/products/rstudio/download)
  - CRAN packages `devtools`, `colorspace`, `ash`, `triangle`, `caTools`

After installing the R environment (and RStudio), run the `R` code below
to install all dependencies as well as `Barbouille`.

```R
# Setting value below to TRUE will reinstall all required packages (optional)
reinstall <- FALSE

# Detect already installed packages
pkg <- ifelse(reinstall, c(), installed.packages()[, "Package"])

# CRAN packages
lst <- c("devtools", "colorspace", "ash", "triangle", "caTools")
lst <- setdiff(lst, pkg)
if(length(lst) > 0) install.packages(lst, dependencies = T)

# GitHub packages
library("devtools")
install_github("benja0x40/Barbouille")
```
