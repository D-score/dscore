cran-comments
================

## Reason for update

`dscore 1.10.0` is a major update providing new features and resolving
some problems

## Test environments

### Local

``` r
R.Version()$version.string
```

    ## [1] "R version 4.5.0 (2025-04-11)"

## Local check

Package built by

``` r
library("devtools")
build()
```

``` bash
R CMD CHECK dscore_1.10.0.tar.gz
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

Status: OK <https://github.com/D-score/dscore/actions/runs/15450842438>

## Downstream dependencies

There are no downstream dependencies for this package.
