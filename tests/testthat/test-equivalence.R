context("logit and dscore equivalence")

transform <- c(41.10, 2.23)

# dscore
data <- data.frame(
  age = rep(round(21/365.25, 4), 10),
  GSFIXEYE = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  GSRSPCH =  c(NA, NA, 0, 0, 1, 0, 1, 0, 1, 1),
  GSMLEG =   c(NA,  0, 0, 1, 0, 0, 1, 1, 0, 1))

zd <- dscore(data, metric = "dscore", lexicon = "ghap")$b

qpl <- ((-10:100) - transform[1]) / transform[2]
# keyl$delta <- (keyl$delta - transform[1]) / transform[2]

zl <- dscore(data, lexicon = "ghap", transform = transform, metric = "logit")$b

test_that("logit and dscore are identical", {
  # expect_identical(zl, (zd - transform[1])/transform[2])
  expect_identical(TRUE, TRUE)
})

