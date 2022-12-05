cran-comments
================

## Reason for update

`dscore 1.7.0` provides new features and resolves some problems

## Test environments

### Local

``` r
R.Version()
```

    ## $platform
    ## [1] "x86_64-apple-darwin17.0"
    ## 
    ## $arch
    ## [1] "x86_64"
    ## 
    ## $os
    ## [1] "darwin17.0"
    ## 
    ## $system
    ## [1] "x86_64, darwin17.0"
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
R CMD CHECK dscore_1.6.9.tar.gz
```

Status: OK

## win-builder

``` r
devtools::check_win_devel()
```

### WIN_DEVEL

`devtools::check_win_devel()` resulted in:

    Status: OK

### RHUB

``` r
check_rhub()
```

The result is:

    ── dscore 1.6.9: OK

      Build ID:   dscore_1.6.9.tar.gz-0128e87e7c4a472d9179d9b1df43073e
      Platform:   Windows Server 2022, R-devel, 64 bit
      Submitted:  2m 18.2s ago
      Build time: 2m 15.9s

    0 errors ✔ | 0 warnings ✔ | 0 notes ✔

    dscore 1.4.0: OK
    Build ID:   dscore_1.4.0.tar.gz-b7c485c90af2483588faae90192af7d9
    Platform:   Ubuntu Linux 16.04 LTS, R-release, GCC
    Submitted:  47 minutes 33.8 seconds ago
    Build time: 47 minutes 31.6 seconds

    dscore 1.4.0: OK
    Build ID:   dscore_1.4.0.tar.gz-07cb0e7218cb4fc2920b459722c0dd79
    Platform:   Fedora Linux, R-devel, clang, gfortran
    Submitted:  54 minutes 40.1 seconds ago
    Build time: 54 minutes 38.3 seconds

    dscore 1.4.0: PREPERROR
    Build ID:   dscore_1.4.0.tar.gz-bd4ae33d5a6b4b8caaff07a0d493a465
    Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN
    Submitted:  1 hour 14 minutes 53.8 seconds ago
    Build time: 1 hour 14 minutes 42.8 seconds

    > ERROR: dependencies ‘xml2’, ‘rvest’ are not available for package ‘kableExtra’
    > ERROR: dependencies ‘CDM’, ‘TAM’, ‘pbv’ are not available for package ‘sirt’```

SvB: Test environment Debian Linux is incomplete. Unlikely to be related
to the `dscore` package.

## Downstream dependencies

There are no downstream dependencies for this package.
