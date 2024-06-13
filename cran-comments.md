cran-comments
================

## Reason for update

`dscore 1.9.0` is a major update providing new features and resolving
some problems

## Resubmission 1

`rhub_setup()` did not add `^.github$` to `.Rbuildignore`. Now done
manually.

## Resubmission 2

Removed markup from DOI field in CITATION file

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
    ## [1] "4.0"
    ## 
    ## $year
    ## [1] "2024"
    ## 
    ## $month
    ## [1] "04"
    ## 
    ## $day
    ## [1] "24"
    ## 
    ## $`svn rev`
    ## [1] "86474"
    ## 
    ## $language
    ## [1] "R"
    ## 
    ## $version.string
    ## [1] "R version 4.4.0 (2024-04-24)"
    ## 
    ## $nickname
    ## [1] "Puppy Cup"

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK dscore_1.9.0.tar.gz
```

Status: OK

## win-builder

``` r
devtools::check_win_devel()
```

### WIN_DEVEL

`devtools::check_win_devel()` resulted in:

    Found the following (possibly) invalid URLs:
      URL: https://tnochildhealthstatistics.shinyapps.io/dcalculator
        From: inst/doc/scoring_GSED.html
        Status: 202
        Message: Accepted
      URL: https://tnochildhealthstatistics.shinyapps.io/dcalculator/
        From: README.md
        Status: 202
        Message: Accepted

    Found the following (possibly) invalid DOIs:
      DOI: https://doi.org/10.1201/9781003216315
        From: inst/CITATION
        Status: 404
        Message: Not Found

These URLs are reachable by the browser, so I assume these are false
positives.

### RHUB

``` r
rhub::rhub_check()
```

Using five builds: Results <https://github.com/D-score/dscore/actions>
are OK.

## Downstream dependencies

There are no downstream dependencies for this package.
