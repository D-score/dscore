# estimate_tau.R using RM
# SvB 27/12/2016
# re-estimate tau from ddata::NL
library("ddata")
library("dscore")
library("eRm")

# recalculate published tau
smocc <- NL[, paste0("n",1:57)]

# remove rows with 0 or 1 observations
items_observed <- apply(!is.na(smocc), 1, sum)
data <- smocc[items_observed > 1, ]

# by custom function
fit.pair <- rasch(data)
tau <- anchor(get_diff(fit.pair), items = c("n12", "n26"))

# erm::RM
fit.RM <- eRm::RM(data)
plot(get_diff(fit.pair), -c(fit.RM$betapar))
abline(0,1)

d <- get_diff(fit.pair) - (-fit.RM$betapar)
hist(d, xlab = "pair - RM")


## conclusion:
## similar difficulty estimates (about -0.3 to +0.3 differences)
## but RM() is critical on input data and
## and takes very long, even when given the
## pair solution as starting configuration
