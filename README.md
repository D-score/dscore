
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dscore

<!-- badges: start -->

[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
[![CRAN
status](https://www.r-pkg.org/badges/version/dscore)](https://CRAN.R-project.org/package=dscore)
[![](http://cranlogs.r-pkg.org/badges/dscore)](https://cran.r-project.org/package=dscore)
[![](https://img.shields.io/badge/github%20version-1.4.0-orange.svg)](https://github.com/d-score/dscore)
<!-- badges: end -->

The *D*-score is a numerical score that measures generic development in
children. You may use the *D*-score to analyze and predict development
of children similar to measures like height and weight.

The `dscore` package contains tools to

-   Map your item names to the GSED convention
-   Calculate *D*-score from item level responses
-   Transform the *D*-scores into DAZ, age-standardised Z-scores

The required input consists of *item level* responses on milestones from
widely used instruments for measuring child development.

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
install.packages("remotes")
remotes::install_github("d-score/dscore")
```

## Overview

You may estimate the *D*-score and the *D*-score age-adjusted Z-score
(DAZ) from child data on developmental milestones. Four steps are
needed:

1.  Identify whether the `dscore` package covers your measurement
    instrument;
2.  Map your variable names to the GSED 9-position schema;
3.  Calculate *D*-score and DAZ;
4.  Summarise your results.

The `dscore` package provides various function that support these steps.
See [Getting started](https://d-score.org/dscore/articles/start.html)
for more details.

## Resources

### Books and reports

1.  [*D*-score: Turning milestones into
    measurement](https://d-score.org/dbook1/)
2.  [Inventory of 147 instruments for measuring early child
    development](https://documents.worldbank.org/en/publication/documents-reports/documentdetail/384681513101293811/a-toolkit-for-measuring-early-childhood-development-in-low-and-middle-income-countries):
    [Fernald et al.](#ref-fernald2017) ([2017](#ref-fernald2017))

### Keys

1.  Project with `dutch` key, 0-2 years: [van
    Buuren](#ref-vanbuuren2014) ([2014](#ref-vanbuuren2014))
2.  Project with `gcdg` key: [Weber et al.](#ref-weber2019)
    ([2019](#ref-weber2019))
3.  Project with `gsed` key: [GSED team (Maureen Black, Kieran Bromley,
    Vanessa Cavallera (lead author), Jorge Cuartas, Tarun Dua
    (corresponding author), Iris Eekhout, Günther Fink, Melissa
    Gladstone, Katelyn Hepworth, Magdalena Janus, Patricia Kariger,
    Gillian Lancaster, Dana McCoy, Gareth McCray, Abbie Raikes, Marta
    Rubio-Codina, Stef van Buuren, Marcus Waldman, Susan Walker and Ann
    Weber)](#ref-gsedteam2019) ([2019](#ref-gsedteam2019))

### Methodology

1.  Interval scale: [Jacobusse, van Buuren, and
    Verkerk](#ref-jacobusse2006) ([2006](#ref-jacobusse2006))
2.  Adaptive testing: [Jacobusse and van Buuren](#ref-jacobusse2007)
    ([2007](#ref-jacobusse2007))

### Shiny app

If you want to calculate the D-score on your own data, and you’re not an
`R` user, you might wish to check out our interactive Shiny
[dcalculator](https://tnochildhealthstatistics.shinyapps.io/dcalculator/)
app.

## Acknowledgement

This study was supported by the Bill & Melinda Gates Foundation. The
contents are the sole responsibility of the authors and may not
necessarily represent the official views of the Bill & Melinda Gates
Foundation or other agencies that may have supported the primary data
studies used in the present study. The authors wish to recognize the
principal investigators and their study team members for their generous
contribution of the data that made this tool possible and the members of
the Ki team who directly or indirectly contributed to the study: Amina
Abubakar, Claudia R. Lindgren Alves, Orazio Attanasio, Maureen M. Black,
Maria Caridad Araujo, Susan M. Chang-Lopez, Gary L. Darmstadt, Bernice
M. Doove, Wafaie Fawzi, Lia C.H. Fernald, Günther Fink, Emanuela
Galasso, Melissa Gladstone, Sally M. Grantham-McGregor, Cristina
Gutierrez de Pineres, Pamela Jervis, Jena Derakhshani Hamadani,
Charlotte Hanlon, Simone M. Karam, Gillian Lancaster, Betzy Lozoff,
Gareth McCray, Jeffrey R Measelle, Girmay Medhin, Ana M. B. Menezes,
Lauren Pisani, Helen Pitchik, Muneera Rasheed, Lisy Ratsifandrihamanana,
Sarah Reynolds, Linda Richter, Marta Rubio-Codina, Norbert Schady,
Limbika Sengani, Chris Sudfeld, Marcus Waldman, Susan P. Walker, Ann M.
Weber and Aisha K. Yousafzai.

### Literature

<div id="refs" class="references csl-bib-body hanging-indent">

<div id="ref-fernald2017" class="csl-entry">

Fernald, L. C. H., E. Prado, P. Kariger, and A. Raikes. 2017. “A Toolkit
for Measuring Early Childhood Development in Low and Middle-Income
Countries.”
<https://documents.worldbank.org/en/publication/documents-reports/documentdetail/384681513101293811/a-toolkit-for-measuring-early-childhood-development-in-low-and-middle-income-countries>.

</div>

<div id="ref-gsedteam2019" class="csl-entry">

GSED team (Maureen Black, Kieran Bromley, Vanessa Cavallera (lead
author), Jorge Cuartas, Tarun Dua (corresponding author), Iris Eekhout,
Günther Fink, Melissa Gladstone, Katelyn Hepworth, Magdalena Janus,
Patricia Kariger, Gillian Lancaster, Dana McCoy, Gareth McCray, Abbie
Raikes, Marta Rubio-Codina, Stef van Buuren, Marcus Waldman, Susan
Walker and Ann Weber). 2019. “The Global Scale for Early Development
(GSED).” *Early Childhood Matters*.
<https://earlychildhoodmatters.online/2019/the-global-scale-for-early-development-gsed/>.

</div>

<div id="ref-jacobusse2007" class="csl-entry">

Jacobusse, G., and S. van Buuren. 2007. “Computerized Adaptive Testing
for Measuring Development of Young Children.” *Statistics in Medicine*
26 (13): 2629–38.
<https://stefvanbuuren.name/publication/jacobusse-2007/>.

</div>

<div id="ref-jacobusse2006" class="csl-entry">

Jacobusse, G., S. van Buuren, and P. H. Verkerk. 2006. “An Interval
Scale for Development of Children Aged 0-2 Years.” *Statistics in
Medicine* 25 (13): 2272–83.
<https://stefvanbuuren.name/publication/jacobusse-2006/>.

</div>

<div id="ref-vanbuuren2014" class="csl-entry">

van Buuren, S. 2014. “Growth Charts of Human Development.” *Statistical
Methods in Medical Research* 23 (4): 346–68.
<https://stefvanbuuren.name/publication/van-buuren-2014-gc/>.

</div>

<div id="ref-weber2019" class="csl-entry">

Weber, A. M., M. Rubio-Codina, S. P. Walker, S. van Buuren, I. Eekhout,
S. Grantham-McGregor, M. C. Araujo, et al. 2019. “The D-Score: A Metric
for Interpreting the Early Development of Infants and Toddlers Across
Global Settings.” *BMJ Global Health* 4: e001724.
<https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf>.

</div>

</div>
