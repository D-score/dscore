library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("ggplot2")

# source("define_colors.R")

# min and max category number (should either 0 or 1)
mm <- tbl_df(master) %>%
  select(age, one_of(itemtable$item)) %>%
  gather(key = item, value = value, -age) %>%
  drop_na(value, age) %>%
  group_by(item) %>%
  summarise(max = max(value), min = min(value))
table(mm$min, mm$max)
inrange <- mm$item[mm$max <= 1]

# proportion pass per month (p), age per month (a)
# observations per months (n) by study and item
data <- tbl_df(master) %>%
  select(study, age, one_of(inrange)) %>%
  gather(key = item, value = value, -age, -study) %>%
  drop_na(value, age) %>%
  mutate(agegp = cut(age, breaks = seq(0, 60, 1))) %>%
  group_by(study, item, agegp) %>%
  summarise(p = round(mean(value), 3), 
            a = mean(age),
            n = n())

nl <- filter(data, 
             study == "Netherlands 1", n >= 10) %>%
  left_join(itemtable, by = "item")

plt <- ggplot(nl, aes(a, p, group = item, colour = domain)) + 
  scale_colour_manual(values = color_domain, na.value = "grey") +
  geom_line() +
  geom_point()
plt


p <- ggplot(diamonds, aes(carat, price, colour = cut)) + 
  geom_point()
p
