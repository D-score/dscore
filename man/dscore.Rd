% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dscore.R
\name{dscore}
\alias{dscore}
\alias{dscore_posterior}
\title{D-score estimation}
\usage{
dscore(
  data,
  items = names(data),
  xname = "age",
  xunit = c("decimal", "days", "months"),
  key = NULL,
  itembank = dscore::builtin_itembank,
  metric = c("dscore", "logit"),
  prior_mean = NULL,
  prior_sd = NULL,
  transform = NULL,
  qp = -10:100,
  population = NULL,
  dec = c(2L, 3L),
  relevance = c(-Inf, Inf)
)

dscore_posterior(
  data,
  items = names(data),
  xname = "age",
  xunit = c("decimal", "days", "months"),
  key = NULL,
  itembank = dscore::builtin_itembank,
  metric = c("dscore", "logit"),
  prior_mean = NULL,
  prior_sd = NULL,
  transform = NULL,
  qp = -10:100,
  population = NULL,
  dec = c(2L, 3L),
  relevance = c(-Inf, Inf)
)
}
\arguments{
\item{data}{A \code{data.frame} with the data.
A row collects all observations made on a child on a set of
milestones administered at a given age. The function calculates
a D-score for each row. Different rows correspond to different
children or different ages.}

\item{items}{A character vector containing names of items to be
included into the D-score calculation. Milestone scores are coded
numerically as \code{1} (pass) and \code{0} (fail). By default,
D-score calculation is done on all items found in the data
that have a difficulty parameter under the specified \code{key}.}

\item{xname}{A string with the name of the age variable in
\code{data}. The default is \code{"age"}.}

\item{xunit}{A string specifying the unit in which age is measured
(either \code{"decimal"}, \code{"days"} or \code{"months"}).
The default (\code{"decimal"}) means decimal age in years.}

\item{key}{A string that selects a subset in the itembank that
makes up the key, the set of difficulty
estimates from a fitted Rasch model.
The built-in keys are:  \code{"gsed2212"} (default), \code{"gsed2208"} (deprecated),
\code{"gsed2206"} (deprecated), \code{"gsed1912"}, \code{"lf2206"}, \code{"sf2206"}, \code{"gcdg"},
and \code{"dutch"}. Since version 1.5.0, the \code{key = "gsed"}
selects the latest key starting with the string "gsed".
Use \code{key = ""} to use all item names,
which should only be done if there are no duplicate itemnames
in the itembank.}

\item{itembank}{A \code{data.frame} with columns
\code{key}, \code{item}, \code{tau}, \code{instrument}, \code{domain},
\code{mode}, \code{number} and \code{label}. Only columns \code{item}
and \code{tau} are required.
The function uses \code{dscore::builtin_itembank} by
default.}

\item{metric}{A string, either \code{"dscore"} (default) or
\code{"logit"}, signalling the metric in which ability is estimated.}

\item{prior_mean}{A string specifying where the mean of the
prior for the D-score calculation should come from. It could be
a column name in \code{data} (when you want your own prior for every row),
but normally this is one of the keywords \code{".dutch"}, \code{".gcdg"} or \code{".phase1"}.
The default depends on the \code{key}. If \code{key == "dutch"} then
\code{prior_mean = ".dutch"}. The choice \code{prior_mean = ".dutch"}
calculates \code{prior_mean} from the Count model coded in
\code{dscore:::count_mu_dutch()}).
If \code{key} is #' \code{"gcdg"}, \code{"gsed1912"},
\code{"gsed2206"}, \code{"lf2206"} or \code{"sf2206"} then \code{prior_mean = ".gcdg"}.
This setting calculates an age-dependent prior mean internally according
to function \code{dscore:::count_mu_gcdg()}. In other cases, \code{prior_mean = ".phase1"}
which uses the function \code{dscore:::count_mu_phase1()}.
Normally, you should not touch this parameter, but feel free to use
\code{prior_mean} to override the automatic choices.}

\item{prior_sd}{A string specifying a column name in \code{data}
with the standard deviation of the prior for the D-score calculation.
If not specified, the standard deviation is taken as 5 for every row.}

\item{transform}{Vector of length 2, signalling the intercept
and slope respectively of the linear transform that converts an
observation in the logit scale to the the D-score scale. Only
needed if \code{metric == "logit"}.}

\item{qp}{Numeric vector of equally spaced quadrature points.
This vector should span the range of all D-score values. The default
(\code{qp = -10:100}) is suitable for age range 0-4 years.}

\item{population}{A string describing the population. Currently
supported are \code{"phase1"} (default), \code{"dutch"}, \code{"gcdg"}.}

\item{dec}{A vector of two integers specifying the number of
decimals for rounding the D-score and DAZ, respectively.
The default is \code{dec = c(2L, 3L)}.}

\item{relevance}{A numeric vector of length with the lower and
upper bounds of the relevance interval. The procedure calculates
a dynamic EAP for each item. If the difficulty level (tau) of the
next item is outside the relevance interval around EAP, the procedure
ignore the score on the item. The default is \code{c(-Inf, +Inf)} does not
ignore scores.}
}
\value{
The \code{dscore()} function returns a \code{data.frame} with
\code{nrow(data)} rows and the following columns:\tabular{ll}{
   Name \tab Label \cr
   \code{a} \tab Decimal age \cr
   \code{n} \tab Number of items with valid (0/1) data \cr
   \code{p} \tab Percentage of passed milestones \cr
   \code{d} \tab Ability estimate, mean of posterior \cr
   \code{sem} \tab Standard error of measurement, standard deviation of the posterior \cr
   \code{daz} \tab D-score corrected for age, calculated in Z-scale \cr
}


The \code{dscore_posterior()} function returns a numeric matrix with
\code{nrow(data)} rows and \code{length(qp)} columns with the
density at each quadrature point. The vector represents the full
posterior ability distribution. If no valid responses were obtained,
\code{dscore_posterior()} returns the prior.
}
\description{
The function \code{dscore()} function estimates the D-score,
a numeric score that measures child development, from PASS/FAIL
observations on milestones.
}
\details{
The algorithm is based on the method by Bock and Mislevy (1982). The
method uses Bayes rule to update a prior ability into a posterior
ability.

The item names should correspond to the \code{"gsed"} lexicon.

A key is defined by the set of estimated item difficulties.\tabular{lrrrcl}{
   Key \tab Model \tab Quadrature \tab Instruments \tab Direct/Caregiver \tab Reference \cr
   \code{"dutch"} \tab \verb{75_0} \tab \code{-10:80} \tab 1 \tab direct \tab Van Buuren, 2014/2020 \cr
   \code{"gcdg"} \tab \verb{565_18} \tab \code{-10:100} \tab 14 \tab direct \tab Weber, 2019 \cr
   \code{"gsed1912"} \tab \verb{807_17} \tab \code{-10:100} \tab 20 \tab mixed \tab GSED Team, 2019 \cr
   \code{"gsed2206"} \tab \verb{818_17} \tab \code{-10:100} \tab 22 \tab mixed \tab GSED Team, 2022 \cr
   \code{"gsed2208"} \tab \verb{818_6} \tab \code{-10:100} \tab 22 \tab mixed \tab GSED Team, 2022 \cr
   \code{"gsed2212"} \tab \verb{818_6} \tab \code{-10:100} \tab 22 \tab mixed \tab GSED Team, 2022 \cr
   \code{"lf2206"} \tab \verb{155_0} \tab \code{-10:100} \tab 1 \tab direct \tab GSED Team, 2022 \cr
   \code{"sf2206"} \tab \verb{139_0} \tab \code{-10:100} \tab 1 \tab caregiver \tab GSED Team, 2022 \cr
}


As a general rule, one should only compare D-scores
that are calculated using the same key and the same
set of quadrature points. For calculating D-scores on new data,
the advice is to use the default, which currently links to
\code{"gsed2212"}.

The default starting prior is a mean calculated from a so-called
"Count model" that describes mean D-score as a function of age. The
Count models are stored as internal functions
\code{dscore:::count_mu_phase1()}, \code{dscore:::count_mu_gcdg()} and
\code{dscore:::count_mu_dutch()}. The spread of the starting prior
is 5 D-score points around this mean D-score, which corresponds to
approximately 1.5 to 2 times the normal spread of child of a given age. The
starting prior is thus somewhat informative for low numbers of
valid items, and uninformative for large number of items (say >10 items).
}
\examples{
data <- data.frame(
  age = rep(round(21 / 365.25, 4), 10),
  ddifmd001 = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  ddicmm029 = c(NA, NA, NA, 0, 1, 0, 1, 0, 1, 1),
  ddigmd053 = c(NA, 0, 0, 1, 0, 0, 1, 1, 0, 1)
)
items <- names(data)[2:4]

# third item is not part of default key
get_tau(items)

# calculate D-score
dscore(data)

# calculate full posterior
p <- dscore_posterior(data)

# plot posterior for row 7
plot(x = -10:100, y = p[7, ], type = "l", xlab = "D-score",
 ylab = "Density", xlim = c(0, 30))
}
\references{
Bock DD, Mislevy RJ (1982).
Adaptive EAP Estimation of Ability in a Microcomputer Environment.
Applied Psychological Measurement, 6(4), 431-444.

Van Buuren S (2014). Growth charts of human development.
Stat Methods Med Res, 23(4), 346-368.
\url{https://stefvanbuuren.name/publication/van-buuren-2014-gc/}

Weber AM, Rubio-Codina M, Walker SP, van Buuren S, Eekhout I,
Grantham-McGregor S, Caridad Araujo M, Chang SM, Fernald LCH,
Hamadani JD, Hanlon A, Karam SM, Lozoff B, Ratsifandrihamanana L,
Richter L, Black MM (2019). The D-score: a metric for interpreting
the early development of infants and toddlers across global settings.
BMJ Global Health, BMJ Global Health 4: e001724.
\url{https://gh.bmj.com/content/bmjgh/4/6/e001724.full.pdf}
}
\seealso{
\code{\link[=get_tau]{get_tau()}},
\code{\link[=builtin_itembank]{builtin_itembank()}}, \code{\link[=posterior]{posterior()}},
\code{\link[=builtin_references]{builtin_references()}}
}
\author{
Stef van Buuren, Iris Eekhout, Arjan Huizing (2022)
}
