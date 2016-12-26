library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("ggplot2")

tab_p <- tbl_df(master) %>%
  select(age, one_of(itemtable$item)) %>%
  # select(1:100) %>%
  gather(key = item, value = value, -age) %>%
  drop_na(value, age) %>%
  mutate(agegp = cut(age, breaks = seq(0, 60, 6))) %>%
  group_by(item, agegp) %>%
  summarise(p = round(mean(value), 3), n = n(),
            max = max(value, na.rm = TRUE)) %>%
  filter(n >= 50 & max <= 1) %>%
  select(item, agegp, p) %>%
  spread(key = agegp, value = p)

tab <- inner_join(itemtable, tab_p, by = "item")

rd <- file.path(getwd(), "results")
write.table(tab, file = file.path(rd, "pass_by_agegrp.txt"),
            sep = "\t", quote = FALSE, row.names = FALSE,
            na = "", dec = ",")
