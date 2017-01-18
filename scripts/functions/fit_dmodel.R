fit_dmodel <- function(model_name = "unspecified", items = names(data), 
                       equatelist = NULL, free = TRUE, data = ddata::gcdg) {
  
  if (!free) {
    # fit model on dutch items to set difficulty parameters
    items_nl <- unique(c(item_names("NL"), item_names("NL2")))
    items_nl <- items_nl[items_nl %in% names(data)]
    data_nl <- ddata::gcdg %>% select_(.dots = items_nl)
    fit_nl <- rasch(data_nl, count = gcdg_count)
    b_fixed <- get_diff(fit_nl)
  }
  else b_fixed <- NULL
  
  item_data <- select_(data, .dots = items)
  
  # fit the "big model"
  fit <- rasch(item_data, equate = equatelist,
               b_fixed = b_fixed, count = ddata::gcdg_count)
  
  # investigate item difficulties fixed vs estimated
  tau <- anchor(get_diff(fit), items = c("n12", "n26"))

  # # create itembank
  tau_df <- data.frame(item = names(tau), tau = tau, stringsAsFactors = FALSE)
  itemtable$item <- as.character(itemtable$item)
  itembank <- left_join(itemtable, tau_df, by = "item")
  itembank <- itembank[!is.na(itembank$tau), ]
  names(itembank)[match("item", names(itembank))] <- "lex.gcdg"

  # calculate d-score
  adm <- c("country", "study", "id", "wave", "age")
  dscore <- data %>%
    select_(.dots = c(adm, items)) %>%
    gather(item, score,  -one_of(adm), na.rm = TRUE) %>%
    arrange(country, study, id, age) %>%
    group_by(study, id, age) %>%
    summarise(d = dscore(scores = score, items = item,
                         ages = age / 12, mu = "model",
                         itembank = itembank, lexicon = "gcdg")) %>%
    ungroup()
  
  # calculate residuals
  residuals <- data %>%
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
  model <- list(name = model_name, 
                data = data, 
                items = items, equatelist = equatelist,
                fit = fit, itembank = itembank, 
                dscore = dscore, 
                item_fit = item_fit, person_fit = person_fit, 
                equate_fit = equate_fit)
  class(model) <- "dmodel"
  return(model)
}
