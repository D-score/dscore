<!-- README.md is generated from README.Rmd. Please edit that file -->
dscore: Measuring child development by the D-score
==================================================

The D-score is a numerical score that measures generic development in children 0-4 years. The D-score is an attempt to analyze and predict development of children using tools developed for numerical measures, like height and weight.

Installation
------------

If you have been marked as a collaborator on GitHub, generated a personal access token (PAT) on <https://github.com/settings/tokens>, and stored your PAT by `Sys.setenv(GITHUB_PAT = "51he..")`, then install the `brokenstick` package from GitHub as follows:

``` r
install.packages("devtools")
devtools::install_github(repo = "stefvanbuuren/dscore", oath = Sys.getenv("GITHUB_PAT"))
```

Overview
--------

The `dscore` package contains functions for

-   Fitting and evaluating the Rasch model to data,
-   Calculating individual trajectories of the D-score,
-   Transforming the D-scores into Z-scores, and back.

Main functions
--------------

The main functions in the `dscore` package are:

| Function name | Description                                    |
|---------------|------------------------------------------------|
| `rasch()`     | Estimate difficulty of items                   |
| `anchor()`    | Transform difficulties to fixed anchors        |
| `dscore()`    | Estimate D-scores of children                  |
| `daz()`       | Transform to age-adjusted standardized D-score |
| `zad()`       | Inverse of `daz()`                             |
