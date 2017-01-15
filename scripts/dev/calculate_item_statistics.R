# Calcultes infit and outfit, both items and persons

library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("dscore")

model_name <- "fx_1310"
# model_name <- "fr_1310"
fn <- file.path(getwd(), "store", paste(model_name, "RData", sep = "."))
load(file = fn)

# merge data to obtain d
data <- left_join(data, model$dscore, by = c("study", "id", "age"))

# calculate residuals
res <- ddata::gcdg %>%
  select_(.dots = c("study", "id", "age", model$items)) %>%
  left_join(model$dscore, by = c("study", "id", "age")) %>%
  gather(key = item, value = value, -study, -id, -age, -d) %>%
  drop_na(value) %>%
  left_join(model$itembank, by = c("item" = "lex.gcdg")) %>%
  select(study, id, age, item, value, tau, d) %>%
  drop_na(d) %>%
  mutate(p = plogis(d, location = tau, scale = 2.1044),
         psi = exp((d - tau)/2.1044),
         pi = psi / (1 + psi), 
         w = pmax(p^2 * (1 - p) + (1 - p)^2 * p, 0.01),
         c = pmax(p^4 * (1 - p) + (1 - p)^4 * p, 0.01),
         y = value - p,
         z = y / w ^ 0.5,
         z2 = z ^ 2,
         y2 = w * z2,
         w2 = w ^ 2,
         cdivw2 = c / w2,
         cminw2 = c - w2)

# fit statistics per item
item_fit <- res %>%
  group_by(item) %>%
  summarize(
    n = n(),
    outfit = mean(z2),
    qo = sqrt(sum(cdivw2) / n ^ 2 - (1 / n)),
    outfit_z = (outfit^(1/3) - 1) * (3 / qo) + qo / 3,
    infit = sum(y2) / sum(w),
    qi = sqrt(pmax(sum(cminw2) / sum(w) ^ 2, 2)),
    infit_z = (infit^(1/3) - 1) * (3 / qi) + qi / 3)

# fit statistics per person
person_fit <- res %>%
  group_by(study, id, age) %>%
  summarize(
    n = n(),
    outfit = mean(z2),
    qo = sqrt(pmax(sum(cdivw2) / n ^ 2 - (1 / n), 2)),
    outfit_z = (outfit^(1/3) - 1) * (3 / qo) + qo / 3,
    infit = sum(y2) / sum(w),
    qi = sqrt(pmax(sum(cminw2) / sum(w) ^ 2, 2)),
    infit_z = (infit^(1/3) - 1) * (3 / qi) + qi / 3)

