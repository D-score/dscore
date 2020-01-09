cran-comments
================

## Resubmission

This is a resubmission. In this version I have

  - Rewritten the `Description:` field according to the suggestions.
    Thanks.
  - Removed the `LICENSE` and `LICENSE.md` files from the package root,
    and added a `Copyright:` field to the DESCRIPTION file. My
    impression was that I needed to the `LICENSE.md` to make it valid
    GPL-3, but I am happy to live without it.

<!-- end list -->

    The Title field should be in title case. Current version is:
    'D-score for Child Development'
    In title case that is:
    'D-Score for Child Development'

I did not alter the package title. The submitted title `D-score for
Child Development` *is* title case. The expression `D-score` is a single
word, like `F-test`, `P-value`, `K-means` or `Q-technique`, all spelled
according to the Cambridge Dictionary of Statistics. I believe the
correct spelling is `D-score`, not `D-Score`. I have consistently used
`D-score` in all publications on the topic, so changing the word to
`D-Score` would create confusion. My suggestion is to add `D-score` to
the exceptions list of the function that scans for title case.

## Reason

This is a new release.

## Checks

Package built by

``` r
library("devtools")
build(manual = TRUE)
```

``` bash
R CMD CHECK /Users/buurensv/Package/dscore/dscore_1.0.0.tar.gz

...
* checking data for non-ASCII characters ... NOTE
  Note: found 3 marked UTF-8 strings
...

Status: 1 NOTE
```

As I believe UTF-8 codes are supported by CRAN, we can ignore the note.

## Test environments

  - local OS X install, 10.15.2, R 3.6.2
  - win-builder, using `devtools::check_win_devel()`
  - rhub

`devtools::check_win_devel()` resulted in 1 NOTE:

    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Stef van Buuren <stef.vanbuuren@tno.nl>'
    
    New submission
    
    License components with restrictions and base license permitting such:
      GPL-3 + file LICENSE
    File 'LICENSE':
      YEAR: 2020
      COPYRIGHT HOLDER: Stef van Buuren; Iris Eekhout; Arjan Huizing
    
    Possibly mis-spelled words in DESCRIPTION:
      DAZ (11:25)
      GSED (13:5)
      dscore (8:18)
    
    The Title field should be in title case. Current version is:
    'D-score for Child Development'
    In title case that is:
    'D-Score for Child Development'

I believe this is standard for a new package. The title should read
“D-score”.

``` r
check_rhub()
```

This throws the following:

    Windows Server 2008 R2 SP1, R-devel, 32/64 bit
    
    dscore 1.0.0: NOTE
    
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: 'Stef van Buuren <stef.vanbuuren@tno.nl>'
    
    New submission
    
    License components with restrictions and base license permitting such:
      GPL-3 + file LICENSE
    File 'LICENSE':
      YEAR: 2020
      COPYRIGHT HOLDER: Stef van Buuren; Iris Eekhout; Arjan Huizing
    
    Possibly mis-spelled words in DESCRIPTION:
      DAZ (11:25)
      GSED (13:5)
      dscore (8:18)
    
    The Title field should be in title case. Current version is:
    'D-score for Child Development'
    In title case that is:
    'D-Score for Child Development'
    
    * checking for non-standard things in the check directory ... NOTE
    Found the following files/directories:
      'dscore-Ex_i386.Rout' 'dscore-Ex_x64.Rout' 'examples_i386'
      'examples_x64' 'tests_i386' 'tests_x64'
      
    0 errors ✓ | 0 warnings ✓ | 2 notes x
    
    ------------
    
    Ubuntu Linux 16.04 LTS, R-release, GCC
    
    dscore 1.0.0: NOTE
    
    * checking CRAN incoming feasibility ... NOTE
    Maintainer: ‘Stef van Buuren <stef.vanbuuren@tno.nl>’
    
    New submission
    
    License components with restrictions and base license permitting such:
      GPL-3 + file LICENSE
    File 'LICENSE':
      YEAR: 2020
      COPYRIGHT HOLDER: Stef van Buuren; Iris Eekhout; Arjan Huizing
    
    Possibly mis-spelled words in DESCRIPTION:
      DAZ (11:25)
      dscore (8:18)
      GSED (13:5)
    
    The Title field should be in title case. Current version is:
    ‘D-score for Child Development’
    In title case that is:
    ‘D-Score for Child Development’
    
    ------------
      
    Fedora Linux, R-devel, clang, gfortran
    
    dscore 1.0.0: NOTE
    
    * checking CRAN incoming feasibility ...NB: need Internet access to use CRAN incoming checks
     NOTE
    Maintainer: ‘Stef van Buuren <stef.vanbuuren@tno.nl>’
    
    License components with restrictions and base license permitting such:
      GPL-3 + file LICENSE
    File 'LICENSE':
      YEAR: 2020
      COPYRIGHT HOLDER: Stef van Buuren; Iris Eekhout; Arjan Huizing
    
    Possibly mis-spelled words in DESCRIPTION:
      DAZ (11:25)
      GSED (13:5)
      dscore (8:18)
    
    The Title field should be in title case. Current version is:
    ‘D-score for Child Development’
    In title case that is:
    ‘D-Score for Child Development’
    
    ------------
      
    Debian Linux, R-devel, GCC ASAN/UBSAN
    
    dscore 1.0.0: PREPERROR
    
    Status for build dscore_1.0.0.tar.gz-34f374f542484e50afb89ddbe9988cbd
    Status: success 
    Duration: 1 hour 7 minutes 6.9 seconds
    
    ...
    11212#> Build step 'Send files or execute commands over SSH' changed build result to SUCCESS
    11213#> Pinging https://builder.r-hub.io/build/SUCCESS/dscore_1.0.0.tar.gz-34f374f542484e50afb89ddbe9988cbd/2019-12-30T16:42:30Z
    11214#> {"status":"ok"}
    11215#> Finished: SUCCESS

I do not understand all details of these checks. To me, it appears that
the message are unrelated to the `dscore` package.

## Downstream dependencies

There are no downstream dependencies for this package.