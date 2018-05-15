<!-- README.md is generated from README.Rmd. Please edit that file -->
dscore: Measuring child development by the D-score
==================================================

The D-score is a numerical score that measures generic development in children 0-4 years. The D-score can be used to analyze and predict development of children using tools developed for numerical measures, like height and weight.

Installation
------------

If you have been marked as a collaborator on GitHub, generated a personal access token (PAT) on <https://github.com/settings/tokens>. Store your PAT by `Sys.setenv(GITHUB_PAT = "51he..")`, where `51he..` is the 40-character token. Alternatively, put the line `GITHUB_PAT=51he...` in your `.Renviron` file. The latter method is persistent over sessions. Install the `dscore` package from GitHub as follows:

``` r
install.packages("devtools")
devtools::install_github(repo = "stefvanbuuren/dscore")
```

Overview
--------

The `dscore` package contains functions to

-   Calculate D-score from item level responses
-   Transform the D-scores into standardised Z-scores, and back
-   Plot individual trajectories of the D-score (not yet implemented).

Main functions
--------------

The main functions in the `dscore` package are:

| Function name | Description                                    |
|---------------|------------------------------------------------|
| `dscore()`    | Estimate D-scores of children                  |
| `daz()`       | Transform to age-adjusted standardized D-score |
| `zad()`       | Inverse of `daz()`                             |
