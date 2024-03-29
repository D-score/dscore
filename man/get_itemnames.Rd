% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_itemnames.R
\name{get_itemnames}
\alias{get_itemnames}
\title{Extract item names}
\usage{
get_itemnames(
  x,
  instrument = NULL,
  domain = NULL,
  mode = NULL,
  number = NULL,
  strict = FALSE,
  itemtable = NULL,
  order = "idnm"
)
}
\arguments{
\item{x}{A character vector, \code{data.frame} or an object of
class \code{lean}. If not specified,
the function will return all item names in \code{itemtable}.}

\item{instrument}{A character vector with 3-position codes of instruments
that should match. The default \code{instrument = NULL} allows for
all instruments.}

\item{domain}{A character vector with 2-position codes of domains
that should match. The default \code{instrument = NULL} allows for
all domains.}

\item{mode}{A character vector with 1-position codes of the mode
of administration. The default \code{mode = NULL} allows for all
modes.}

\item{number}{A numeric or character vector with item numbers.
The default \code{number = NULL} allows for all numbers.}

\item{strict}{A logical specifying whether the resulting item
names must conform to one of the built-in names. The default is
\code{strict = FALSE}.}

\item{itemtable}{A \code{data.frame} set up according to the
same structure as \code{\link[=builtin_itemtable]{builtin_itemtable()}}. If not specified,
the \code{builtin_itemtable} is used.}

\item{order}{A four-letter string specifying the sorting order.
The four letters are: \code{i} for instrument, \code{d} for domain,
\code{m} for mode and \code{n} for number. The default is
\code{"idnm"}.}
}
\value{
A vector with names of items
}
\description{
The \code{get_itemnames()} function matches names against the 9-code
template. This is useful for quickly selecting names of items from a larger
set of names.
}
\details{
The gsed-naming convention is as follows. Position 1-3 codes the
instrument, position 4-5 codes the domain, position 6 codes
direct/caregiver/message, positions 7-9 is a item sequence number.
}
\examples{
itemnames <- c("aqigmc028", "grihsd219", "", "age", "mdsgmd999")

# filter out impossible names
get_itemnames(itemnames)
get_itemnames(itemnames, strict = TRUE)

# only items from specific instruments
get_itemnames(itemnames, instrument = c("aqi", "mds"))
get_itemnames(itemnames, instrument = c("aqi", "mds"), strict = TRUE)

# get all items from the se domain of iyo instrument
get_itemnames(domain = "se", instrument = "iyo")

# get all item from the se domain with direct assessment mode
get_itemnames(domain = "se", mode = "d")

# get all item numbers 70 and 73 from gm domain
get_itemnames(number = c(70, 73), domain = "gm")
}
\seealso{
\code{\link[=sort_itemnames]{sort_itemnames()}}
}
\author{
Stef van Buuren 2020
}
