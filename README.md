
<!-- README.md is generated from README.Rmd.  Please edit that file -->

# rforcpue

<!-- badges: start -->

<!-- badges: end -->

**rforcpue** is designed to assist with the standardization of fisheries
cpue data and the intent is that it will replace the **cede** and
**r4cpue** packages, which are now deprecated and will become obsolete.
As a package, **rforcpue** is tied to no particular fishery or species
and attempts are made to make the functions customizable to suit any
given species or fishery. Functions will be developed that will enable
the use of simple linear models (with log-transformed cpue data), use of
GLMs (with different underlying distributions), and GAMs, along with
examples of each. We will use a *News.md* file to track developments in
this package as they occur.

This package is currently under development and has no vignettes, as
yet. There are many additional functions I intend to add that implement
and illustrate GLMs with alternative residual distributions and GAMs,
should they be wanted. It imports **rutilsMH**, so that package will
also be needed should you wish to use this.

## Installation

You can install this development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("haddonm/rforcpue")
```

## Example

This will need to wait until I have some exampe data built in.

``` r
library(rforcpue)
## basic example code
```
