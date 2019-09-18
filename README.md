
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dscore

<!-- badges: start -->

<!-- badges: end -->

The `dscore` package contains tools to

  - Calculate D-score from item level responses
  - Transform the D-scores into DAZ, age-standardised Z-scores

The required input consists of *item level* responses on milestones from
widely used instruments for measuring child development.

## Installation

You can install the released version of dscore from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dscore")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("stefvanbuuren/dscore")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(dscore)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`?
You can include R chunks like so:

``` r
summary(cars)
#>      speed           dist    
#>  Min.   : 4.0   Min.   :  2  
#>  1st Qu.:12.0   1st Qu.: 26  
#>  Median :15.0   Median : 36  
#>  Mean   :15.4   Mean   : 43  
#>  3rd Qu.:19.0   3rd Qu.: 56  
#>  Max.   :25.0   Max.   :120
```

You’ll still need to render `README.Rmd` regularly, to keep `README.md`
up-to-date.

You can also embed plots, for example:

<img src="man/figures/README-pressure-1.png" width="100%" />

In that case, don’t forget to commit and push the resulting figure
files, so they display on GitHub\!
