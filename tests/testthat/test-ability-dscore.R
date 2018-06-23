context("ability-dscore")

# dscore
items <- c("GSFIXEYE", "GSRSPCH", "GSMLEG")
age <- round(rep(21/365.25, 3), 4)  # age 21 days
dscore(c(1, 0, 0), items, age, dec = 4)

# ability
data <- data.frame(
  age = round(21/365.25, 3),
  GSFIXEYE = 1,
  GSRSPCH = 0,
  GSMLEG = 0)
ability(data, items = items, dec = 4)
