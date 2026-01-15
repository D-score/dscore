# extrapolate cohorts gsed 2510

# extrapolate ages 3.56-4.00
extrapolate_cohorts_gsed2510 <- function(data) {
  cohorts <- unique(data$cohort)
  newdays <- seq(1309, 1463, 7)
  newlist <- vector("list", length = length(cohorts))
  names(newlist) <- cohorts
  for (pop in cohorts) {
    mu <- get_mu_gsed_cohorts(newdays / 365.25, key = "gsed2510", cohort = pop)
    ref <- subset(data, cohort == pop)
    newdata <- ref[rep(nrow(ref), 23), , drop = FALSE] |>
      `row.names<-`(NULL)
    newdata$agedays <- newdays
    newdata$mu <- mu
    newlist[[pop]] <- newdata
  }
  newdata <- bind_rows(newlist)
  result <- bind_rows(data, newdata) |>
    dplyr::arrange(cohort, agedays)
  return(result)
}
