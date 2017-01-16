# fit to Rasch model to selected items

library("dscore")
library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

# take items
# 1) from a registered instrument
# 2) for which we have at least one observation in each category
items <- names(ddata::gcdg)[names(ddata::gcdg) %in% ddata::itemtable$item]
data <- ddata::gcdg %>%
  select_(.dots = items)
# %>%
#   select_if(category_size_exceeds, 1)
# items <- names(data)

# select equategroups among selected items
itemtable_select <- itemtable[which(itemtable$item %in% items), ]
equatelist <- tapply(itemtable_select$item, itemtable_select$equate, list)

# remove troublesome equates 
equatelist$GM1 <- NULL  # ag2 - n4
equatelist$GM2 <- NULL  # ag1 - n3
equatelist$FM2 <- NULL  # af3 - n8
equatelist$EXP5 <- NULL  # b3e5 - dl4 - n10
equatelist$COG18 <- NULL  # b1m45 - b2m34 - b3c18 - n9

# fit model on dutch items to set difficulty parameters
items_nl <- unique(c(item_names("NL"), item_names("NL2")))
items_nl <- items_nl[items_nl %in% names(data)]
data_nl <- ddata::gcdg %>% select_(.dots = items_nl)
fit_nl <- rasch(data_nl, count = gcdg_count)
b_fixed <- get_diff(fit_nl)

# fit the "big model"
fit <- rasch(data, equate = equatelist,
                         count = gcdg_count)
#            b_fixed = b_fixed, count = gcdg_count)

# investigate item difficulties fixed vs estimated
tau <- anchor(get_diff(fit), items = c("n12", "n26"))
tau_est <- tau[names(b_fixed)]
tau_fixed  <- anchor(b_fixed, items = c("n12", "n26"))
plot(x = NULL, y = NULL, ylab = "New tau", xlab = "Fixed tau", 
     xlim = c(0, 80), ylim = c(0, 80))
text(names(b_fixed), x = tau_fixed, y = tau_est)
tau_df <- data.frame(item = names(b_fixed), fixed = tau_fixed, 
                  est = tau_est)
cor(tau_df[,2:3])

# # create itembank
tau_df <- data.frame(item = names(tau), tau = tau, stringsAsFactors = FALSE)
itembank <- left_join(itemtable, tau_df, by = "item")
itembank <- itembank[!is.na(itembank$tau), ]
names(itembank)[4] <- "lex.gcdg"

# calculate d-score
adm <- c("country", "study", "id", "wave", "age")
dscore <- ddata::gcdg %>%
  select_(.dots = c(adm, items)) %>%
  gather(item, score,  -one_of(adm), na.rm = TRUE) %>%
  arrange(country, study, id, age) %>%
  group_by(study, id, age) %>%
  summarise(d = dscore(scores = score, items = item,
                       ages = age / 12, mu = "model",
                       itembank = itembank, lexicon = "gcdg")) %>%
  ungroup()

# joint plot of d-score against age
plot(x = NULL, y = NULL, xlim = c(0, 48), ylim = c(0, 80),
     xaxp = c(0, 48, 8),
     xlab = "Age (in months)", ylab = "D-score")
abline(h = seq(0, 80, 10), v = seq(0, 48, 6), col = "grey90", lty = 3)
xgrid <- seq(1/12, 4, 1/12)
prov_mu <- function(t) {44.35 - 1.8 * t + 28.47 * log(t + 0.25)}
lines(x = xgrid * 12, y = prov_mu(xgrid), lty = 1, lwd = 3,
      col = "olivedrab")
with(dscore, points(x = age, y = d, cex = 0.4, col = "navy"))
mtext(paste(Sys.Date(), length(items), length(b_fixed), length(equatelist)),
      side = 1, line = 3, at = 0)

# calculate residuals
residuals <- ddata::gcdg %>%
  select_(.dots = c("study", "id", "age", items)) %>%
  left_join(dscore, by = c("study", "id", "age")) %>%
  gather(key = item, value = value, -study, -id, -age, -d) %>%
  drop_na(value) %>%
  left_join(itembank, by = c("item" = "lex.gcdg")) %>%
  select(study, id, age, equate, item, value, tau, d) %>%
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
item_fit <- residuals %>%
  group_by(item) %>%
  summarize(
    n = n(),
    outfit = mean(z2),
    qo = sqrt(pmin(sum(cdivw2) / n ^ 2 - (1 / n), 2)),
    outfit_z = (outfit^(1/3) - 1) * (3 / qo) + qo / 3,
    infit = sum(y2) / sum(w),
    qi = sqrt(pmin(sum(cminw2) / sum(w) ^ 2, 2)),
    infit_z = (infit^(1/3) - 1) * (3 / qi) + qi / 3)

# fit statistics per person-age
person_fit <- residuals %>%
  group_by(study, id, age) %>%
  summarize(
    n = n(),
    outfit = mean(z2),
    qo = sqrt(pmin(sum(cdivw2) / n ^ 2 - (1 / n), 2)),
    outfit_z = (outfit^(1/3) - 1) * (3 / qo) + qo / 3,
    infit = sum(y2) / sum(w),
    qi = sqrt(pmin(sum(cminw2) / sum(w) ^ 2, 2)),
    infit_z = (infit^(1/3) - 1) * (3 / qi) + qi / 3)

# fit statistics per equate
equate_fit <- residuals %>%
  group_by(equate) %>%
  summarize(
    n = n(),
    outfit = mean(z2),
    qo = sqrt(pmin(sum(cdivw2) / n ^ 2 - (1 / n), 2)),
    outfit_z = (outfit^(1/3) - 1) * (3 / qo) + qo / 3,
    infit = sum(y2) / sum(w),
    qi = sqrt(pmin(sum(cminw2) / sum(w) ^ 2, 2)),
    infit_z = (infit^(1/3) - 1) * (3 / qi) + qi / 3)

# store model
model_name <- "fr_1310"
# model_name <- "fx_1310"
model <- list(name = model_name, items = items, equatelist = equatelist,
              fit = fit, itembank = itembank, 
              dscore = dscore, 
              item_fit = item_fit, person_fit = person_fit, 
              equate_fit = equate_fit)
fn <- file.path(getwd(), "store", paste(model_name, "RData", sep = "."))
save(model, file = fn, compress = "xz")
