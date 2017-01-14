# test_eRm.R
# This script tests out some facilities of the eRm package

library("ddata")
library("dscore")
library("dplyr", warn.conflicts = FALSE)
library("sirt")
library("eRm")

# Only Dutch items: 57 items
items <- item_names("NL")
data <- ddata::gcdg %>%
  select_(.dots = c("study", items)) %>%
  filter(study == "Netherlands 1") %>%
  select(-study)

# there are 20974 rows and 57 items in data
dim(data)

# there are 16735 non-empty rows
data2 <- data[rowSums(is.na(data)) != ncol(data),]
dim(data2)

# Estimate difficulties
fit1 <- dscore::rasch(data)

# Solution identical, but note that get_diff(fit1) and fit2 have opposite sign
fit2 <- sirt::rasch.pairwise.itemcluster(data, progress = FALSE)


# --- Experiments with functions from the eRm package ---

# RM() is cannot handle the data, so let's forget
# fit3 <- RM(data2)
# Error in datcheck(X, W, mpoints, groupvec, model) : 
#   Subjects with only 1 valid response must be removed!

# obtain person parameters (assuming normality)
pp <- person.parameter(fit1)

# theoretical person-item probabilities (for subset of 12433 rows)
pm <- pmat(pp)

# calculate squared standardized residuals (for subset of 12433 rows)
pr <- residuals(pp)

# calculate itemfit statistics
ifit <- itemfit(pp)

# calculate personfit statistics (for subset of 12433 rows)
pfit <- personfit(pp)

# standard plotPWmap() fails, presumably because of missing standard errors
# in fit1
# plotPWmap(fit1, pp = pp)

# however, including persons DOES produce a plot
plotPWmap(fit1, pp = pp, pmap = TRUE)

# person-item map shows that persons are non-normal
plotPImap(fit1, sorted = TRUE)

# The following plot reproduces figure 3a in Jacobusse 2006
hist(ifit$i.outfitZ, breaks = 15)

# The following plot reproduces figure 3b in Jacobusse 2006
hist(ifit$i.outfitMSQ, breaks = 14)

# A major problem in application of these functions to these data 
# is the huge reduction in number of rows by person.parameter().

# Question: 
# Are there really good estimates of item fit, given the drastic 
# person reduction and the overwelming number of missing data??

# Suggestions:
# - Do a better job in estimating the person parameters (e.g. with EAP and prior)
# - Carefully check n's that are used in calculation of fit statistics
