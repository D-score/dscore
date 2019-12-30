pkgname <- "dscore"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('dscore')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("builtin_itembank")
### * builtin_itembank

flush(stderr()); flush(stdout())

### Name: builtin_itembank
### Title: Built-in itembank
### Aliases: builtin_itembank
### Keywords: datasets

### ** Examples

head(builtin_itembank)



cleanEx()
nameEx("builtin_references")
### * builtin_references

flush(stderr()); flush(stdout())

### Name: builtin_references
### Title: Age-conditional reference distribution of D-score
### Aliases: builtin_references
### Keywords: datasets

### ** Examples

head(builtin_references)



cleanEx()
nameEx("count_mu_dutch")
### * count_mu_dutch

flush(stderr()); flush(stdout())

### Name: count_mu_dutch
### Title: Median of Dutch references
### Aliases: count_mu_dutch

### ** Examples

dscore:::count_mu_dutch(0:2)



cleanEx()
nameEx("count_mu_gcdg")
### * count_mu_gcdg

flush(stderr()); flush(stdout())

### Name: count_mu_gcdg
### Title: Median of gcdg references
### Aliases: count_mu_gcdg

### ** Examples

dscore:::count_mu_gcdg(0:2)



cleanEx()
nameEx("daz")
### * daz

flush(stderr()); flush(stdout())

### Name: daz
### Title: D-score standard deviation score: DAZ
### Aliases: daz zad

### ** Examples

# using gcdg-reference
daz(d = c(35, 50), x = c(0.5, 1.0))

# using Dutch reference
daz(d = c(35, 50), x = c(0.5, 1.0), reference = get_reference("dutch"))
# population median at ages 0.5, 1 and 2 years, gcdg reference
zad(z = rep(0, 3), x = c(0.5, 1, 2))

# population median at ages 0.5, 1 and 2 years, dutch reference
zad(z = rep(0, 3), x = c(0.5, 1, 2), reference = get_reference("dutch"))

# percentiles of D-score reference
g <- expand.grid(age = seq(0.1, 2, 0.1), p = c(0.1, 0.5, 0.9))
d <- zad(z = qnorm(g$p), x = g$age)
matplot(
  x = matrix(g$age, ncol = 3), y = matrix(d, ncol = 3), type = "l",
  lty = 1, col = "blue", xlab = "Age (years)", ylab = "D-score"
)



cleanEx()
nameEx("decompose_itemnames")
### * decompose_itemnames

flush(stderr()); flush(stdout())

### Name: decompose_itemnames
### Title: Decomposes item names into their four components
### Aliases: decompose_itemnames

### ** Examples

itemnames <- c("aqigmc028", "grihsd219", "", "by1mdd157", "mdsgmd006")
decompose_itemnames(itemnames)



cleanEx()
nameEx("dscore")
### * dscore

flush(stderr()); flush(stdout())

### Name: dscore
### Title: D-score estimation
### Aliases: dscore dscore_posterior

### ** Examples

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



cleanEx()
nameEx("get_age_equivalent")
### * get_age_equivalent

flush(stderr()); flush(stdout())

### Name: get_age_equivalent
### Title: Get age equivalents of items that have a difficulty estimate
### Aliases: get_age_equivalent

### ** Examples

get_age_equivalent(c("ddicmm030", "ddicmm050"), key = "dutch")



cleanEx()
nameEx("get_itemnames")
### * get_itemnames

flush(stderr()); flush(stdout())

### Name: get_itemnames
### Title: Extract item names
### Aliases: get_itemnames

### ** Examples

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



cleanEx()
nameEx("get_labels")
### * get_labels

flush(stderr()); flush(stdout())

### Name: get_labels
### Title: Get labels for items
### Aliases: get_labels

### ** Examples

# get labels of first two Macarthur items
get_labels(get_itemnames(instrument = "mac", number = 1:2), trim = 40)



cleanEx()
nameEx("get_tau")
### * get_tau

flush(stderr()); flush(stdout())

### Name: get_tau
### Title: Obtain difficulty parameters from item bank
### Aliases: get_tau

### ** Examples

# difficulty levels in the GHAP lexicon
get_tau(items = c("ddifmd001", "DDigmd052", "xyz"))



cleanEx()
nameEx("milestones")
### * milestones

flush(stderr()); flush(stdout())

### Name: milestones
### Title: Outcomes on developmental milestones for preterm-born children
### Aliases: milestones
### Keywords: datasets

### ** Examples

head(milestones)



cleanEx()
nameEx("normalize")
### * normalize

flush(stderr()); flush(stdout())

### Name: normalize
### Title: Normalize distribution
### Aliases: normalize

### ** Examples

dscore:::normalize(c(5, 10, 5), qp = c(0, 1, 2))

sum(dscore:::normalize(rnorm(5), qp = 1:5))



cleanEx()
nameEx("rename_gcdg_gsed")
### * rename_gcdg_gsed

flush(stderr()); flush(stdout())

### Name: rename_gcdg_gsed
### Title: Rename items from gcdg into gsed lexicon
### Aliases: rename_gcdg_gsed

### ** Examples

from <- c(
  "ag28", "gh2_19", "a14ps4", "b1m157", "mil6",
  "bm19", "a16fm4", "n22", "ag9", "gh6_5"
)
to <- rename_gcdg_gsed(from, copy = FALSE)
to



cleanEx()
nameEx("sort_itemnames")
### * sort_itemnames

flush(stderr()); flush(stdout())

### Name: sort_itemnames
### Title: Sorts item names according to user-specified priority
### Aliases: sort_itemnames order_itemnames

### ** Examples

itemnames <- c("aqigmc028", "grihsd219", "", "by1mdd157", "mdsgmd006")
decompose_itemnames(itemnames)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
