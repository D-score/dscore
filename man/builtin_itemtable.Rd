% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/builtin_itemtable.R
\docType{data}
\name{builtin_itemtable}
\alias{builtin_itemtable}
\title{Collection of items from instruments measuring early child development}
\format{
A \code{data.frame} with variables:\tabular{ll}{
   Name \tab Label \cr
   \code{item} \tab Item name, gsed lexicon \cr
   \code{equate} \tab Equate group \cr
   \code{label} \tab Label (English) \cr
}
}
\usage{
builtin_itemtable
}
\description{
The built-in variable \code{builtin_itemtable} contains the name and label
of items for measuring early child development.
}
\details{
The \code{builtin_itemtable} is created by script
\code{data-raw/R/save_builtin_itemtable.R}.

Updates:
\itemize{
\item May 30, 2022 - added gto (LF) and gpa (SF) items
\item June 1, 2022 - added seven gsd items
\item Nov 24, 2022 - Added instruments gs1, gs2
\item Dec 01, 2022 - Labels of gto replaced by correct order.
Incorrect item order affects analyses done on LF between 20220530 - 20221201 !!!
\item Dec 05, 2022 - Redefines gs1 and instrument for Phase 2, removes gs2 (139)
Adds gl1 (Long Form Phase 2 items 155)
\item Jan 05, 2023 - Adds 55 items from GSED-HF
}
}
\author{
Compiled by Stef van Buuren using different sources
}
\keyword{datasets}
