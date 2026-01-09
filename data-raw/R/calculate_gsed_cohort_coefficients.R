# Calculate regression coefficients for get_mu_gsed_cohorts() function
# This script fits Count models (with log-transformed age) for each GSED cohort
# across three age groups, following the pattern used in get_mu.R
#
# Author: Generated script
# Date: 2026-01-09

library(dplyr)
library(tidyr)

# Load the cohort predictions data
cohort_data <- read.csv(
  "data-raw/data/references/cohort_predictions_unified.csv"
)

# Convert age from days to years
cohort_data <- cohort_data %>%
  mutate(age = agedays / 365.25)

# Get unique cohorts
cohorts <- unique(cohort_data$cohort)
cat("Found cohorts:", paste(cohorts, collapse = ", "), "\n\n")

# Define age groups based on the pattern in get_mu.R (lines 190-202)
# Age group 1: age < 0.75 (< 9 months)
# Age group 2: age >= 0.75 & age < 3.2 (9 months to 3.2 years)
# Age group 3: age >= 3.2 (> 3.2 years)

# Initialize results data frame
results <- data.frame(
  cohort = character(),
  age_group = character(),
  age_range = character(),
  model_type = character(),
  intercept = numeric(),
  age_coef = numeric(),
  log_coef = numeric(),
  log_offset = numeric(),
  n_obs = integer(),
  r_squared = numeric(),
  formula = character(),
  stringsAsFactors = FALSE
)

# Function to fit model for an age group
fit_age_group_model <- function(
  data,
  age_group_name,
  age_range_text,
  log_offset
) {
  n <- nrow(data)

  if (n < 3) {
    warning(paste("Not enough observations (n =", n, ") for", age_range_text))
    return(NULL)
  }

  # Fit Count model: mu ~ age + log(age + offset)
  model <- lm(mu ~ age + I(log(age + log_offset)), data = data)

  # Extract coefficients
  coefs <- coef(model)

  # Get R-squared
  r_sq <- summary(model)$r.squared

  # Create formula string
  formula_str <- sprintf(
    "%.5f + %.5f * age + %.5f * log(age + %.2f)",
    coefs[1],
    coefs[2],
    coefs[3],
    log_offset
  )

  return(data.frame(
    age_group = age_group_name,
    age_range = age_range_text,
    model_type = "Count",
    intercept = coefs[1],
    age_coef = coefs[2],
    log_coef = coefs[3],
    log_offset = log_offset,
    n_obs = n,
    r_squared = r_sq,
    formula = formula_str,
    stringsAsFactors = FALSE
  ))
}

# Loop through each cohort
for (cohort_name in cohorts) {
  cat("\n========================================\n")
  cat("Processing cohort:", cohort_name, "\n")
  cat("========================================\n")

  # Filter data for this cohort
  cohort_subset <- cohort_data %>%
    filter(cohort == cohort_name)

  # Age group 1: < 9 months (< 0.75 years)
  # Based on line 195: log(age + 10) for descriptive
  # But looking at phase1 (line 135): log(age + 0.2)
  # Let's use log(age + 0.2) as in the active code
  ref1 <- cohort_subset %>%
    filter(age < 0.75)

  cat("\nAge Group 1: < 9 months (age < 0.75 years)\n")
  cat("Number of observations:", nrow(ref1), "\n")

  if (nrow(ref1) >= 3) {
    result1 <- fit_age_group_model(
      ref1,
      "group1",
      "age < 0.75",
      log_offset = 0.2
    )
    result1$cohort <- cohort_name
    results <- rbind(results, result1)
    cat("Formula:", result1$formula, "\n")
    cat("R-squared:", sprintf("%.4f", result1$r_squared), "\n")
  }

  # Age group 2: 9 months to 3.2 years (>= 0.75 & < 3.2)
  # Based on line 199: log(age + 0.25) for descriptive
  # But looking at phase1 (line 138): log(age + 0.92)
  # Let's use log(age + 0.92) as in the active code
  ref2 <- cohort_subset %>%
    filter(age >= 0.75 & age < 3.2)

  cat("\nAge Group 2: 9 months to 3.2 years (0.75 <= age < 3.2)\n")
  cat("Number of observations:", nrow(ref2), "\n")

  if (nrow(ref2) >= 3) {
    result2 <- fit_age_group_model(
      ref2,
      "group2",
      "0.75 <= age < 3.2",
      log_offset = 0.92
    )
    result2$cohort <- cohort_name
    results <- rbind(results, result2)
    cat("Formula:", result2$formula, "\n")
    cat("R-squared:", sprintf("%.4f", result2$r_squared), "\n")
  }

  # Age group 3: >= 3.2 years
  # Based on line 202: Linear model (no log transform)
  # Following phase1 (line 140): simple linear model
  ref3 <- cohort_subset %>%
    filter(age >= 3.2)

  cat("\nAge Group 3: >= 3.2 years (age >= 3.2)\n")
  cat("Number of observations:", nrow(ref3), "\n")

  if (nrow(ref3) >= 3) {
    # Fit simple linear model: mu ~ age
    model3 <- lm(mu ~ age, data = ref3)
    coefs3 <- coef(model3)
    r_sq3 <- summary(model3)$r.squared

    formula_str3 <- sprintf("%.5f + %.5f * age", coefs3[1], coefs3[2])

    result3 <- data.frame(
      cohort = cohort_name,
      age_group = "group3",
      age_range = "age >= 3.2",
      model_type = "Linear",
      intercept = coefs3[1],
      age_coef = coefs3[2],
      log_coef = NA_real_,
      log_offset = NA_real_,
      n_obs = nrow(ref3),
      r_squared = r_sq3,
      formula = formula_str3,
      stringsAsFactors = FALSE
    )

    results <- rbind(results, result3)
    cat("Formula:", result3$formula, "\n")
    cat("R-squared:", sprintf("%.4f", result3$r_squared), "\n")
  }
}

# Print summary table
cat("\n\n========================================\n")
cat("SUMMARY: Regression Coefficients by Cohort and Age Group\n")
cat("========================================\n\n")

# Reorder columns for better display
results <- results %>%
  select(
    cohort,
    age_group,
    age_range,
    model_type,
    intercept,
    age_coef,
    log_coef,
    log_offset,
    n_obs,
    r_squared,
    formula
  )

print(results, row.names = FALSE)

# Save results
output_file <- "data-raw/data/references/gsed_cohort_mu_coefficients.csv"
write.csv(results, output_file, row.names = FALSE)
cat("\n\nResults saved to:", output_file, "\n")

# Create a formatted table for easy copying into R code
cat("\n\n========================================\n")
cat("FORMATTED OUTPUT FOR get_mu_gsed_cohorts() FUNCTION\n")
cat("========================================\n\n")

for (cohort_name in cohorts) {
  cat(sprintf('# %s\n', cohort_name))

  cohort_results <- results %>% filter(cohort == cohort_name)

  # Group 1
  if (any(cohort_results$age_group == "group1")) {
    row1 <- cohort_results %>% filter(age_group == "group1")
    cat(sprintf(
      '# Age < 0.75: %.5f + %.5f * t + %.5f * log(t + %.2f)\n',
      row1$intercept,
      row1$age_coef,
      row1$log_coef,
      row1$log_offset
    ))
  }

  # Group 2
  if (any(cohort_results$age_group == "group2")) {
    row2 <- cohort_results %>% filter(age_group == "group2")
    cat(sprintf(
      '# Age 0.75-3.2: %.5f + %.5f * t + %.5f * log(t + %.2f)\n',
      row2$intercept,
      row2$age_coef,
      row2$log_coef,
      row2$log_offset
    ))
  }

  # Group 3
  if (any(cohort_results$age_group == "group3")) {
    row3 <- cohort_results %>% filter(age_group == "group3")
    cat(sprintf(
      '# Age >= 3.2: %.5f + %.5f * t\n',
      row3$intercept,
      row3$age_coef
    ))
  }

  cat("\n")
}

cat("\n========================================\n")
cat("Script completed successfully!\n")
cat("========================================\n")
