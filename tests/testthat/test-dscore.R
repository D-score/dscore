context("dscore")

# dscore, gsed lexicon
data <- data.frame(
  age = rep(round(21/365.25, 4), 10),
  ddifmd001 = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  ddicmm029 = c(NA, NA, 0, 0, 1, 0, 1, 0, 1, 1),
  ddigmd053 = c(NA,  0, 0, 1, 0, 0, 1, 1, 0, 1))
z <- dscore(data)

# dscore
data <- data.frame(
  age = rep(round(21/365.25, 4), 10),
  GSFIXEYE = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  GSRSPCH =  c(NA, NA, 0, 0, 1, 0, 1, 0, 1, 1),
  GSMLEG =   c(NA,  0, 0, 1, 0, 0, 1, 1, 0, 1))
items <- c("GSFIXEYE", "GSRSPCH", "GSMLEG")
z <- dscore(data, items = items, lexicon = "ghap", dec = c(4, 4))

test_that("result has nrow(data) rows", {
  expect_identical(nrow(z), nrow(data))
})

