context("prior")

# Calculate the custom prior mean by adding 5 to the default prior mean
data <- milestones[1:10, ]
mymean <- dscore:::count_mu_preliminary_standards(t = data$age) + 5

# Method 1: Added variable
adj1 <- dscore(data = cbind(data, mymean), prior_mean = "mymean")

# Method 2: Direct vector
adj2 <- dscore(data = data, prior_mean = mymean)

test_that("Added variable and Direct vector prior yield same result", {
  expect_identical(adj1, adj2)
})
