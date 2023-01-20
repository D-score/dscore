cran-comments
================

## Reason for update

`dscore 1.8.0` provides new features and resolves some problems

## Test environments

### Local

``` r
R.Version()
```

    ## $platform
    ## [1] "aarch64-apple-darwin20"
    ## 
    ## $arch
    ## [1] "aarch64"
    ## 
    ## $os
    ## [1] "darwin20"
    ## 
    ## $system
    ## [1] "aarch64, darwin20"
    ## 
    ## $status
    ## [1] ""
    ## 
    ## $major
    ## [1] "4"
    ## 
    ## $minor
    ## [1] "2.2"
    ## 
    ## $year
    ## [1] "2022"
    ## 
    ## $month
    ## [1] "10"
    ## 
    ## $day
    ## [1] "31"
    ## 
    ## $`svn rev`
    ## [1] "83211"
    ## 
    ## $language
    ## [1] "R"
    ## 
    ## $version.string
    ## [1] "R version 4.2.2 (2022-10-31)"
    ## 
    ## $nickname
    ## [1] "Innocent and Trusting"

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK dscore_1.8.0.tar.gz
```

Status: OK

## win-builder

``` r
devtools::check_win_devel()
```

### WIN_DEVEL

`devtools::check_win_devel()` resulted in:

    * checking CRAN incoming feasibility ... [11s] NOTE
    Maintainer: 'Stef van Buuren <stef.vanbuuren@tno.nl>'

    Found the following (possibly) invalid URLs:
      URL: https://support.posit.co/hc/en-us/articles/201141096-Getting-Started-with-R
        From: inst/doc/scoring_GSED.html
        Status: 403
        Message: Forbidden

The URL is reachable by browser. I assume this status results from a
setting made by Posit.

### RHUB

``` r
check_rhub()
```

The result is: `SUCCESS` for all four builds

## Downstream dependencies

There are no downstream dependencies for this package.
