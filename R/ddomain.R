#' Domain specific D-score
#'
#' @param domainset String. The name of the set of domains to use. See
#' `with(builtin_domaintable, table(set, domain))` for the domain names in each
#' set.
#' @param domain character vector of the name of the domain(s) for which to
#'   compute the domain score. Per default all domains in the `domainset` are
#'   used .
#' @param vote_weight minimum proportion of votes (weight) for a domain that an
#' item needs to have to count for that domain.
#' @inheritParams dscore
#'
#' @return The `ddomain()` function returns a list of `data.frame` objects with
#' each `nrow(data)` rows. The name of the list is the name of the domain.
#' The `data.frame` consists of the following columns:
#'
#' Name | Label
#' ---  | ---------
#' `a`  | Decimal age (years)
#' `n`  | Number of items with valid (0/1) data
#' `p`  | Percentage of passed milestones
#' `d`  | D-score, mean of posterior distribution
#' `sem` | Standard error of measurement, standard deviation of the posterior
#' `daz` | D-score corrected for age, calculated in Z-scale (for metric `"dscore"`)
#'
#' The D-score in column `d` is a linear scale, with values usually ranging
#' from 0 to 100. The D-score is `NA` if age is missing or if age is lower
#' than -1/12. It is possible to calculate D-scores for cases with missing ages
#' by setting `prior_mean_NA` and `prior_sd_NA` to some reasonable value, e.g.,
#' `prior_mean_NA = 50` and `prior_sd_NA = 20`, for the sample at hand.
#'
#' The SEM is a positive number that quantifies the uncertainty of the D-score.
#' It is `NA` if the D-score is `NA`.
#'
#' The DAZ in column `daz` is a Z-score that corrects the D-score for age. It
#' is `NA` when there are no reference values for the given age, or when
#' the D-score is extremely unlikely to be valid at the given age.
#'
#' @seealso [dscore()] [builtin_domaintable()]
#'
#' @export
#' @examples
#' sample <- dscore::gsample
#' colnames(sample) <- rename_vector(colnames(sample), lexin = "gsed2", lexout = "gsed3")
#' sample <- sample |> select(subjid, agedays, starts_with("gs1")) |>
#'  mutate(age = agedays / 365.25)
#' ddomain(sample, domainset = "GFCLS")
#' ddomain(sample, domainset = "GFCLS", domain = c("finemotor", "grossmotor"))
#' ddomain(sample, domainset = "GFCLS", domain = c("language"))
ddomain <- function(data,
                     domainset,
                     domain = NULL,
                     vote_weight = NULL,
                     items = names(data),
                     key = NULL,
                     population = NULL,
                     xname = "age",
                     xunit = c("decimal", "days", "months"),
                     itembank = NULL,
                     ...){

  xunit <- match.arg(xunit)
   data <- as.data.frame(data)

  #domain specific code
  domaintable <- builtin_domaintable |> filter(set == domainset)

  if(is.null(vote_weight)) {vote_weight <- 0}

  if(is.null(domain)) {
    domain = unique(domaintable$domain)
   }

  domain_select = intersect(domain, domaintable$domain)

  if(length(domain_select) == 0){stop("domain names are not present in the domainset")}

  ddomain_list <- list()
  for(dom in domain_select){
    dom_items <- domaintable |> filter(domain == dom & weight > vote_weight) |> pull(item) |> as.vector()
    dom_items <- intersect(dom_items, items)

    ddomain_list[[dom]] <-
    dscore(
      data = data,
      items = dom_items,
      key = key,
      population = population,
      xname = xname,
      xunit = xunit,
      itembank = itembank,
      ...
    )

  }

  return(ddomain_list)

}
