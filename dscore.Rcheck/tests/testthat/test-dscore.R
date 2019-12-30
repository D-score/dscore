context("dscore")

# dscore, gsed lexicon
data <- data.frame(
  age = rep(round(21 / 365.25, 4), 10),
  ddifmd001 = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  ddicmm029 = c(NA, NA, NA, 0, 1, 0, 1, 0, 1, 1),
  ddigmd053 = c(NA, 0, 0, 1, 0, 0, 1, 1, 0, 1)
)

z1 <- dscore(data, key = "dutch")
expected_d1 <- c(NA, -1.971, -2.030, 1.203, 1.203, 1.203, 4.569, 4.569,
                 4.569, 11.785)
test_that("produces expected D-scores - key dutch", {
  expect_identical(z1$d, expected_d1)
})

z2 <- dscore(data, key = "gcdg")
expected_d2 <- c(NA, NA, 3.487, 0.971, 4.836, 4.836, 4.836, 4.836,
                 11.846, 11.846)
test_that("produces expected D-scores - key gcdg", {
  expect_identical(z2$d, expected_d2)
})

z3 <- dscore(data, key = "gsed")
expected_d3 <- c(NA, NA, 3.480, 0.969, 4.830, 4.830, 4.830, 4.830,
                 11.842, 11.842)
test_that("produces expected D-scores - key gsed", {
  expect_identical(z3$d, expected_d3)
})

# subset by items
items <- c("ddifmd001", "ddicmm029", "ddigmd053")
z4 <- dscore(data, items = items, key = "dutch")
expected_d4 <- expected_d1
test_that("produces expected D-scores - key dutch", {
  expect_identical(z4$d, expected_d4)
})

z5 <- dscore(data, items = items[1:2], key = "dutch")
expected_d5 <- c(NA, NA, 3.403, 0.492, 4.540, 4.540, 4.540,
                 4.540, 11.784, 11.784)
test_that("produces expected D-scores - key dutch", {
  expect_identical(z5$d, expected_d5)
})

z6 <- dscore(data, items = items[1:2])
expected_d6 <- expected_d3
test_that("produces expected D-scores", {
  expect_identical(z6$d, expected_d6)
})

z7 <- dscore(data, items = c(items[1:2], "junk"))
expected_d7 <- expected_d3
test_that("produces expected D-scores", {
  expect_identical(z7$d, expected_d7)
})


# --- test zero rows
data <- data.frame(age = numeric(0))
test_that("handles zero rows", {
  expect_identical(nrow(dscore(data)), 0L)
})
