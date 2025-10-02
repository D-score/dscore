cran-comments
================

## Reason for update

`dscore 2.0.0` is a major new release that builds on more extensive data

## Resubmission 1

*Why is such a quick update needed? Please explain. Please also re-read
the CRAN policies about submission frequency.And why is there a major
new release after only 6 days? This is very confusing….*

I realize 6 days is short. Version 1.11.0 was a regular maintenance
release and provides a stable fallback for users not yet ready to
transition. Version 2.0.0 introduces a new key and changes an important
default, which some collaborators need immediately, but may break code
for existing users. Having both versions on CRAN ensures continuity for
existing users and access to the new functionality.

*Is there some reference about the method you can add in the Description
field in the form Authors (year) <doi:10.....>?*

I updated DESCRIPTION with the relevant DOI reference.

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
