cran-comments
================

## Reason

Request from CRAN because of breaking change in `tibble 3.0.0`

## Test environments

  - local OS X install, 10.15.4, R 3.6.2
  - win-builder, using `devtools::check_win_devel()`
  - rhub

### Local build

Package built by

``` r
library("devtools")
build(manual = TRUE)
```

``` bash
R CMD CHECK /Users/buurensv/Package/dscore/dscore_1.2.0.tar.gz

...
* checking data for non-ASCII characters ... NOTE
  Note: found 3 marked UTF-8 strings
...

Status: 1 NOTE
```

UTF-8 codes are supported by CRAN, so I assume we can ignore the note.

### WIN\_DEVEL

`devtools::check_win_devel()` resulted in:

    Status: OK

### RHUB

``` r
check_rhub()
```

The result is:

    ── dscore 1.2.0: NOTE
    
      Build ID:   dscore_1.2.0.tar.gz-ace2b0cbcddd4eb980f5659a20a2c170
      Platform:   Windows Server 2008 R2 SP1, R-devel, 32/64 bit
      Submitted:  7m 50s ago
      Build time: 7m 32s
    
    > checking for non-standard things in the check directory ... NOTE
      Found the following files/directories:
        'dscore-Ex_i386.Rout' 'dscore-Ex_x64.Rout' 'examples_i386'
        'examples_x64' 'tests_i386' 'tests_x64'
    
    0 errors ✓ | 0 warnings ✓ | 1 note x

    ── dscore 1.2.0: OK
    
      Build ID:   dscore_1.2.0.tar.gz-b23ec143969242e49a6f750f640a28f4
      Platform:   Ubuntu Linux 16.04 LTS, R-release, GCC

    ── dscore 1.2.0: OK
    
      Build ID:   dscore_1.2.0.tar.gz-e8125f39d6dd48ea8c54610080f7c200
      Platform:   Fedora Linux, R-devel, clang, gfortran

    ── dscore 1.2.0: PREPERROR
    
    STATUS: success
    
      Build ID:   dscore_1.2.0.tar.gz-12c76df788da4dc7a782826f051d749c
      Platform:   Debian Linux, R-devel, GCC ASAN/UBSAN

I believe none of the messages is caused by the `dscore` package.

## Downstream dependencies

There are no downstream dependencies for this package.
