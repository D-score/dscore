library("dscore")
library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

# take items
# 1) from a registered instrument
# 2) for which we have at least 25 observations in each category
items <- names(gcdg)[names(gcdg) %in% itemtable$item]
data <- ddata::gcdg %>%
  select_(.dots = items) %>%
  select_if(category_size_exceeds, 25)
items <- names(data)

# select equategroups among selected items
itemtable_select <- itemtable[which(itemtable$item %in% items), ]
equatelist <- tapply(itemtable_select$item, itemtable_select$equate, list)

# fit model on dutch items to set difficulty parameters
items_nl <- unique(c(item_names("NL"), item_names("NL2")))
items_nl <- items_nl[items_nl %in% names(data)]
data_nl <- ddata::gcdg %>% select_(.dots = items_nl)
fit_nl <- rasch(data_nl, count = gcdg_count)
b_fixed <- get_diff(fit_nl)

# fit the "big model"
system.time(fit <- rasch(data, equate = equatelist,
                          b_fixed = b_fixed, count = gcdg_count))

# # create itembank
tau <- anchor(get_diff(fit), items = c("n12", "n26"))
tau_df <- data.frame(item = names(tau), tau = tau, stringsAsFactors = FALSE)
itembank <- left_join(itemtable, tau_df, by = "item")
itembank <- itembank[!is.na(itembank$tau), ]
names(itembank)[4] <- "lex.gcdg"

# calculate d-score
adm <- c("country", "study", "id", "wave", "age")
system.time(alldscore <- gcdg %>%
  select_(.dots = c(adm, items)) %>%
  gather(item, score,  -one_of(adm), na.rm = TRUE) %>%
  arrange(country, study, id, age) %>%
  group_by(study, id, age) %>%
  summarise(d = dscore(scores = score, items = item,
                       ages = age / 12, mu = "model",
                       itembank = itembank, lexicon = "gcdg")) %>%
  ungroup())

# joint plot of d-score against age
plot(x = NULL, y = NULL, xlim = c(0, 48), ylim = c(0, 80),
     xaxp = c(0, 48, 8),
     xlab = "Age (in months)", ylab = "D-score")
abline(h = seq(0, 80, 10), v = seq(0, 48, 6), col = "grey90", lty = 3)
xgrid <- seq(1/12, 4, 1/12)
prov_mu <- function(t) {44.35 - 1.8 * t + 28.47 * log(t + 0.25)}
lines(x = xgrid * 12, y = prov_mu(xgrid), lty = 1, lwd = 3,
      col = "olivedrab")
with(alldscore, points(x = age, y = d, cex = 0.4, col = "navy"))
mtext(paste(Sys.Date(), length(items), length(b_fixed), length(equatelist)),
      side = 1, line = 3, at = 0)
