cran-comments
================

## Reason for update

`dscore 2.0.0` is a major new release that builds on more extensive data

## Test environments

### Local

``` r
R.Version()$version.string
```

    ## [1] "R version 4.5.1 (2025-06-13)"

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK ../dscore_2.0.0.tar.gz
```

Status: OK

## win-builder

``` r
devtools::check_win_devel()
```

Upload of the dscore package to win-builder failed.

### RHUB

``` r
rhub::rhub_check()
```

Using three builds: linux, windows, macos.

Status: OK

## Downstream dependencies

There are no downstream dependencies for this package.
