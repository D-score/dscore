# recreate_tau.R
# SvB 27/12/2016

# re-estimate tau from ddata::NL
library("ddata")
library("dscore")

# recalculate published tau
smocc <- NL[, paste0("n",1:57)]
fit <- rasch(smocc)
tau <- anchor(get_diff(fit), items = c("n12", "n26"))
round(tau, 1)

# compare with published data (Van Buuren 2014, Table 2)
cor(itembank[!is.na(itembank$lex.dutch1983), "tau"], tau)
plot(itembank[!is.na(itembank$lex.dutch1983), "tau"], tau,
     xlab = "Published", ylab = "Calculated")
abline(0,1)
