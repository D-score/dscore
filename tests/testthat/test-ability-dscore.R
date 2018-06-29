context("ability-dscore")

# dscore
items <- c("GSFIXEYE", "GSRSPCH", "GSMLEG")
age <- round(rep(21/365.25, 3), 4)  # age 21 days
dsco <- as.vector(dscore(c(1, 0, 0), items, age, dec = 4))

# ability
data <- data.frame(
  age = round(21/365.25, 4),
  GSFIXEYE = 1,
  GSRSPCH = 0,
  GSMLEG = 0)
abil <- ability(data, items = items, dec = 4)$b

test_that("ability() and dscore() produce same D-scores", {
  expect_identical(abil, dsco)
})


# pops
items <- names(popsdemo)[8:64]
itemset <- !is.na(itembank$lex_dutch1983)
ib <- itembank[itemset,c("lex_dutch1983", "lex_ghap", "labelEN", "tau")]

item_locations <- names(popsdemo) %in% items
names(popsdemo)[item_locations] <- as.character(ib$lex_ghap)

delta <- gettau(items = names(popsdemo)[item_locations])
items <- names(delta)

data <- popsdemo
data$age <- round(data$daycor / 365.25, 3)

key <- data.frame(item = items, 
                  delta = delta, 
                  stringsAsFactors = FALSE)
abil <- ability(data, items, age = "age", key = key)

# --- using dscore()

dsco <- data %>%
  select(patid, age, one_of(items)) %>%
  gather(item, score, -age, -patid, na.rm = TRUE) %>%
  arrange(patid, age) %>%
  group_by(patid, age) %>%
  summarise(d = dscore::dscore(scores = score, items = item,
                               ages = age, 
                               itembank = dscore::itembank, 
                               lexicon = "ghap")) %>%
  ungroup() %>%
  pull(d)

test_that("ability() produces nrow(data) rows", {
  expect_identical(length(abil$b), nrow(data))
  expect_identical(nrow(abil), nrow(data))
})

# test_that("ability() and dscore() produce same D-scores", {
#   expect_identical(abil, dsco)
# })

