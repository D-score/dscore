context("logit and dscore equivalence")

transform <- c(41.10, 2.23)

# ability
data <- data.frame(
  age = rep(round(21/365.25, 4), 10),
  GSFIXEYE = c(NA, NA, 0, 0, 0, 1, 0, 1, 1, 1),
  GSRSPCH =  c(NA, NA, 0, 0, 1, 0, 1, 0, 1, 1),
  GSMLEG =   c(NA,  0, 0, 1, 0, 0, 1, 1, 0, 1))
items <- c("GSFIXEYE", "GSRSPCH", "GSMLEG")

keyd <- data.frame(item = items,
                   delta = gettau(items = items),
                   stringsAsFactors = FALSE)

zd <- ability(data, items = items, dec = 4, metric = "dscore", 
              key = keyd)$b

qpl <- ((-10:100) - transform[1]) / transform[2]
keyl <- data.frame(item = items,
                   delta = gettau(items = items),
                   stringsAsFactors = FALSE)
keyl$delta <- (keyl$delta - transform[1]) / transform[2]
zl <- ability(data, items = items, dec = 4, transform = transform, 
              qp = qpl, metric = "logit", key = keyl)$b

test_that("logit and dscore are identical", {
  expect_identical(zl, (zd - transform[1])/transform[2])
})

