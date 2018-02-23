Barbouille
================================================================================

My collection of plotting and coloring functions, home cooked with the base
R graphics library.

### Main features

* Empirical distributions: `BivariateDensity` and `SideBySideDensity` functions
* Piecewise color mapping: `DefineColorMap`, `MakeColors`, `ColorLegend` functions

Basic documentation of each function is available in the reference manual.  
See also the [code examples and images](#examples) below.

### Installation

Run the `R` code below to install `Barbouille`.

```R
library("devtools")
install_github("benja0x40/Barbouille")
```

If the installation fails, try to install dependencies as indicated below.

#### Dependencies
  - [R environment](https://www.r-project.org/) version 3.4 or newer
  - To develop and execute `R` scripts we recommend using [RStudio](https://www.rstudio.com/products/rstudio/download)
  - CRAN packages `devtools`, `colorspace`, `ash`, `triangle`, `caTools`

After installing the R environment (and RStudio), run the `R` code below
to install all dependencies for `Barbouille`.

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

### <a name="examples"></a>Examples

```r
library(Barbouille)
```

#### Empirical distributions

```r
# x being a matrix of 50000 rows x 5 columns randomly generated with mixtures of normal distributions
h <- SideBySideDensity(x, nx = 200, plot = T)
h <- SideBySideDensity(x, nx = 200, plot = T, smoothx = 25)
```

![](./images/gallery/barbouille_07.png "example")
![](./images/gallery/barbouille_08.png "example")

```r
cmf <- function(k) colorize(k, mode = "01", col = "ry")
h <- SideBySideDensity(x, nx = 200, plot = T, clr.mapper = cmf, smoothx = 5)
h <- SideBySideDensity(x, nx = 200, plot = T, clr.mapper = cmf, jitter = "norm")
```

![](./images/gallery/barbouille_10.png "example")
![](./images/gallery/barbouille_09.png "example")

```r
h <- BivariateDensity(x[, c(1, 3)], plot = T, method = "ash")
h <- BivariateDensity(x[, c(1, 5)], plot = T, clr.mapper = cmf)
h <- BivariateDensity(x[, c(2, 4)], plot = T, clr.mapper = cmf)
```

![](./images/gallery/barbouille_11.png "example")
![](./images/gallery/barbouille_12.png "example")
![](./images/gallery/barbouille_13.png "example")

#### Piecewise color mapping

![](./images/gallery/barbouille_01.png "example")
![](./images/gallery/barbouille_03.png "example")
![](./images/gallery/barbouille_05.png "example")
![](./images/gallery/barbouille_02.png "example")
![](./images/gallery/barbouille_04.png "example")
![](./images/gallery/barbouille_06.png "example")
