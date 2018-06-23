context("ability")

# ability
data <- data.frame(
  age = rep(round(21/365.25, 4), 10),
  GSFIXEYE = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  GSRSPCH =  c(NA, NA, 0, 0, 1, 0, 1, 0, 1, 1),
  GSMLEG =   c(NA,  0, 0, 1, 0, 0, 1, 1, 0, 1))
items <- c("GSFIXEYE", "GSRSPCH", "GSMLEG")
z <- ability(data, items = items, dec = 4)

test_that("result has nrow(data) rows", {
  expect_identical(nrow(z), nrow(data))
})

