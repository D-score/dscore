context("dscore")

# dscore, gsed lexicon
data <- data.frame(
  age = rep(round(21 / 365.25, 4), 10),
  ddifmd001 = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  ddicmm029 = c(NA, NA, NA, 0, 1, 0, 1, 0, 1, 1),
  ddigmd053 = c(NA, 0, 0, 1, 0, 0, 1, 1, 0, 1)
)

expect_silent(dscore(data, items = character(0)))

# explicit key "gsed2212"
expected_d <- c(NA, NA, 6.61, 5.60, 9.09, 9.09, 9.09, 9.09, 15.30, 15.30)
expected_daz <- c(
  NA,
  NA,
  -2.019,
  -2.235,
  -1.447,
  -1.447,
  -1.447,
  -1.447,
  0.277,
  0.277
)
z <- dscore(data, key = "gsed2212", algorithm = "1.8.7")
test_that("produces expected D-scores - key gsed2212", {
  expect_identical(z$d, expected_d)
  expect_identical(z$daz, expected_daz)
})

z1 <- dscore(data, key = "dutch", algorithm = "1.8.7")
expected_d1 <- c(
  NA,
  -1.87,
  -1.94,
  1.26,
  1.26,
  1.26,
  4.63,
  4.63,
  4.63,
  12.06
)
test_that("produces expected D-scores - key dutch", {
  expect_identical(z1$d, expected_d1)
})

z2 <- dscore(data, key = "gcdg", algorithm = "1.8.7")
expected_d2 <- c(
  NA,
  NA,
  3.47,
  0.96,
  4.83,
  4.83,
  4.83,
  4.83,
  11.81,
  11.81
)
test_that("produces expected D-scores - key gcdg", {
  expect_identical(z2$d, expected_d2)
})

# subset by items
items <- c("ddifmd001", "ddicmm029", "ddigmd053")
z4 <- dscore(data, items = items, key = "dutch", algorithm = "1.8.7")
expected_d4 <- expected_d1
test_that("produces expected D-scores - key dutch", {
  expect_identical(z4$d, expected_d4)
})

z5 <- dscore(data, items = items[1:2], key = "dutch", algorithm = "1.8.7")
expected_d5 <- c(
  NA,
  NA,
  3.53,
  0.59,
  4.61,
  4.61,
  4.61,
  4.61,
  12.06,
  12.06
)
test_that("produces expected D-scores - key dutch", {
  expect_identical(z5$d, expected_d5)
})

test_that("Silently handles outside item code", {
  expect_silent(dscore(
    data,
    items = c(items[1:2], "gpagmc013"),
    algorithm = "1.8.7"
  ))
})


# --- test zero rows
data <- data.frame(age = numeric(0))
test_that("handles zero rows", {
  expect_identical(nrow(dscore(data)), 0L)
})

# --- test negative ages
# dscore, gsed lexicon
data <- data.frame(
  age = c(-0.26, -0.26, NA, 1000, 0.5, -100, Inf, -Inf, 0, 1),
  ddifmd001 = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  ddicmm029 = c(NA, NA, NA, 0, 1, 0, 1, 0, 1, 1),
  ddigmd053 = c(NA, 0, 0, 1, 0, 0, 1, 1, 1, 1)
)
test_that("silently handles out-of-range ages", {
  expect_silent(dscore(data, key = "dutch"))
})


## D-score - test equivalence of dscore and logit metric
data <- data.frame(
  age = rep(round(21 / 365.25, 4), 10),
  ddifmd001 = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  ddicmm029 = c(NA, NA, NA, 0, 1, 0, 1, 0, 1, 1),
  ddigmd053 = c(NA, 0, 0, 1, 0, 0, 1, 1, 0, 1)
)
my_itembank <- data.frame(
  key = "mykey",
  item = items,
  tau = get_tau(items = items, key = "dutch")
)
my_reference <- builtin_references[builtin_references$key == "dutch", ]
my_reference$key <- "mykey"

# externally specified transformation
# transform <- c(0, 1)
# transform <- c(0, 2)
transform <- c(50, 3)
qp <- -10:100

algorithm <- "1.8.7"
algorithm <- "current"
zd <- dscore(
  data,
  items = items,
  dec = 4,
  metric = "dscore",
  itembank = my_itembank,
  transform = transform,
  qp = qp,
  key = "mykey",
  population = "dutch",
  algorithm = algorithm,
  verbose = TRUE
)
zl <- dscore(
  data,
  items = items,
  dec = 4,
  metric = "logit",
  itembank = my_itembank,
  key = "mykey",
  transform = transform,
  qp = qp,
  algorithm = algorithm
)

lastkey <- builtin_keys[nrow(builtin_keys), ]
transform <- c(lastkey$intercept, lastkey$slope)
algorithm <- "1.8.7"
zd <- dscore(data, metric = "dscore", algorithm = algorithm, verbose = FALSE)
zl <- dscore(data, metric = "logit", algorithm = algorithm)
test_that("logit and dscore are identical (1.8.7)", {
  expect_equal(zl$d, (zd$d - transform[1]) / transform[2], tolerance = 0.001)
  expect_equal(zl$d * transform[2] + transform[1], zd$d, tolerance = 0.001)
})

algorithm <- "current"
zd <- dscore(data, metric = "dscore", algorithm = algorithm, verbose = FALSE)
zl <- dscore(data, metric = "logit", algorithm = algorithm)
test_that("logit and dscore are identical (current)", {
  expect_equal(zl$d, (zd$d - transform[1]) / transform[2], tolerance = 0.001)
  expect_equal(zl$d * transform[2] + transform[1], zd$d, tolerance = 0.001)
})

# check prior mean as column in data
data <- cbind(data, start = rep(c(0, 10), times = 5))
zp0 <- dscore(data)
zp1 <- dscore(data, prior_mean = "start")
test_that("D-score difference at uneven rows (with start 0) is higher than on uneven rows (with start 10)", {
  expect_gt(zp0$d[3] - zp1$d[3], zp0$d[4] - zp1$d[4])
  expect_gt(zp0$d[5] - zp1$d[5], zp0$d[6] - zp1$d[6])
  expect_gt(zp0$d[7] - zp1$d[7], zp0$d[8] - zp1$d[8])
  expect_gt(zp0$d[9] - zp1$d[9], zp0$d[10] - zp1$d[10])
})

test_that("get_mu() handles missing ages", {
  expect_silent(get_mu(t = c(NA, NA), key = "gsed2406", prior_mean_NA = 50))
  expect_silent(get_mu(
    t = c(NA, -3, 1:3, NA),
    key = "gsed2406",
    prior_mean_NA = 50
  ))
})


# test empty score vector
scores <- structure(
  list(
    age = 0,
    ddifmd001 = NA_integer_,
    ddifmd002 = NA_real_,
    ddifmd003 = NA_real_,
    ddifmm004 = NA_integer_,
    ddifmd005 = NA_integer_,
    ddigmd006 = NA_real_,
    ddifmd007 = NA_integer_,
    ddifmd008 = NA_integer_,
    ddifmm009 = NA_real_,
    ddifmd010 = NA_real_,
    ddifmd011 = NA_real_,
    ddifmm012 = NA_integer_,
    ddifmd013 = NA_real_,
    ddifmm014 = NA_integer_,
    ddifmd015 = NA_real_,
    ddifmm016 = NA_integer_,
    ddifmd017 = NA_integer_,
    ddifmd018 = NA_integer_,
    ddifmm019 = NA_integer_,
    ddifmd020 = NA_integer_,
    ddifmd021 = NA_integer_,
    ddifmd022 = NA_integer_,
    ddifmd023 = NA_integer_,
    ddifmd024 = NA_integer_,
    ddifmm025 = NA_integer_,
    ddifmd026 = NA_integer_,
    ddifmd027 = NA_integer_,
    ddifmd028 = NA_integer_,
    ddicmm029 = NA_integer_,
    ddicmm030 = NA_integer_,
    ddicmm031 = NA_integer_,
    ddicmm032 = NA_integer_,
    ddicmm033 = NA_integer_,
    ddicmm034 = NA_integer_,
    ddicmm035 = NA_integer_,
    ddicmm036 = NA_integer_,
    ddicmm037 = NA_integer_,
    ddicmm038 = NA_integer_,
    ddicmm039 = NA_integer_,
    ddicmm040 = NA_integer_,
    ddicmm041 = NA_integer_,
    ddicmm042 = NA_integer_,
    ddicmm043 = NA_integer_,
    ddicmd044 = NA_integer_,
    ddicmm045 = NA_integer_,
    ddicmm046 = NA_integer_,
    ddicmm047 = NA_integer_,
    ddicmm048 = NA_integer_,
    ddicmd049 = NA_integer_,
    ddicmm050 = NA_integer_,
    ddicmm051 = NA_integer_,
    ddigmd052 = NA_real_,
    ddigmd053 = NA_real_,
    ddigmd054 = NA_integer_,
    ddigmd155 = NA_real_,
    ddigmd255 = NA_real_,
    ddigmd355 = NA_real_,
    ddigmd055 = NA_real_,
    ddigmd056 = NA_integer_,
    ddigmd057 = NA_integer_,
    ddigmd058 = NA_integer_,
    ddigmd059 = NA_real_,
    ddigmm060 = NA_integer_,
    ddigmd061 = NA_integer_,
    ddigmd062 = NA_integer_,
    ddigmd063 = NA_integer_,
    ddigmm064 = NA_integer_,
    ddigmm065 = NA_integer_,
    ddigmm066 = NA_integer_,
    ddigmm067 = NA_integer_,
    ddigmd068 = NA_real_,
    ddigmd168 = NA_real_,
    ddigmd268 = NA_real_,
    ddigmd069 = NA_integer_,
    ddigmd070 = NA_integer_,
    ddigmd071 = NA_real_,
    ddigmd072 = NA_integer_,
    ddigmm073 = NA_integer_,
    ddigmd074 = NA_integer_,
    ddigmd075 = NA_real_
  ),
  class = "data.frame",
  row.names = c(NA, -1L)
)

test_that("empty vector works with all keys", {
  expect_silent(dscore(scores, key = "dutch"))
  expect_silent(dscore(scores, key = "gcdg"))
  expect_silent(dscore(scores, key = "gsed"))
})

# Variables to append
n <- nrow(data)
ids <- data.frame(
  id_chr = LETTERS[1:n],
  id_num = 128 + 1:n,
  a = NA_real_,
  d = rnorm(n)
)
data2 <- data.frame(ids, data)

test_that("prepend attaches two ID columns", {
  expect_equal(ncol(dscore(data2, prepend = c("id_chr", "id_num"))), 2 + 6)
})
test_that("unknown variables names produce notfound warning", {
  expect_warning(
    dscore(data2, prepend = c("idonotexist")),
    "Not found: idonotexist"
  )
})
test_that("reserved names produce overwrite warning", {
  expect_warning(dscore(data2, prepend = c("a", "d")))
})
