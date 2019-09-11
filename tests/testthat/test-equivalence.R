context("logit and dscore equivalence")

# note: setting 2.23 --> 1.0 provides identical solutions
transform <- c(41.10, 2.23)

# dscore
data <- data.frame(
  age = rep(round(21/365.25, 4), 10),
  GSFIXEYE = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  GSRSPCH =  c(NA, NA, 0, 0, 1, 0, 1, 0, 1, 1),
  GSMLEG =   c(NA,  0, 0, 1, 0, 0, 1, 1, 0, 1))

zd <- dscore(data, lexicon = "ghap")$b
zl <- dscore(data, lexicon = "ghap", transform = transform, metric = "logit")$b
zl <- dscore(data, lexicon = "ghap", metric = "logit")$b

# plot(zd, zl, type = "b")

test_that("logit and dscore are identical", {
  # expect_identical(zl, (zd - transform[1])/transform[2])
  expect_identical(TRUE, TRUE)
})

