#gs1 domain table
library(openxlsx)
library(dplyr)
library(tidyr)
library(dscore)
#original raw voting file <-- copied from ddomain/data-raw/data 8-12-2025
domaintable_raw <- read.xlsx("data-raw/data/domains/Feasibility and domaining survey output V4.0.xlsx",
                             startRow = 2, sheet= "Domaining Nums")[,c(2,3,6:18)]
colnames(domaintable_raw) <- c("item","modality", "GM", "FM", "REC", "EXP", "Prob_Solv",
                               "Exe_Func", "Pre_ac","Self_reg", "Emo_kn", "Soc_com",
                               "Intern", "Extern", "Life_sk")
#additional votes for new items
domaintable_gsedadds <- read.csv("data-raw/data/domains/GSED Additional Items - Brief Domaining.csv")
colnames(domaintable_gsedadds) <- c("time", "name", "iyolgc005", "cromoc001", "iyomoc002", "crosec004", "by3cgd089", "by3cgd048", "by3cgd053") #note that I am not sure about itemname by3cgd089

domaintable_gsedadds2 <- read.xlsx("data-raw/data/domains/Extra seven items for Iris.xlsx") #NOTE: for this file, manually added a ; between multiple votes in one cell in the excel file.
colnames(domaintable_gsedadds2) <- c("time", "name", "gsdlac001", "gsdcgc002", "gsdfmc003", "gsdlac004", "gsdgmc005", "gsdgmc006", "gsdgmc007")
domaintable_gsedadds2 <- domaintable_gsedadds2 %>% filter(!name=="MJ") %>%
  mutate(name = ifelse(name == "MJ2", "MJ", name)) %>% select(-time)

domaintable_gsedadds <- domaintable_gsedadds %>% left_join(domaintable_gsedadds2)

domaintable_gsedaddst <-
  domaintable_gsedadds %>%
  pivot_longer(cols = c(-time, -name), names_to = "item", values_to = "voteslong") %>%
  mutate(voteslong = gsub("Expressive, ", "Expressive;", voteslong)) %>%
  separate(col = voteslong, into = c("vote1", "vote2", "vote3"), sep = ";", fill = "right") %>%
  pivot_longer(cols = c(vote1, vote2, vote3), names_to = "vote", values_to = "domain") %>%
  mutate(domain = factor(domain, labels = c("Exe_Func", "Pre_ac", "Prob_Solv", "EXP", "REC", "FM", "GM","Emo_kn", "Self_reg", "Soc_com"))) %>%
  drop_na(domain)

#to have one vote per SME
domaintable_gsedaddst2 <- domaintable_gsedaddst %>%
  left_join({domaintable_gsedaddst %>% group_by(name,item) %>% summarise(n = n())}, by = c("name","item")) %>%
  mutate(vote = 1/n) %>%
  group_by(item, domain) %>%
  summarise(votes = sum(vote),
            weight = votes/8,
            .groups = "drop") %>%
  pivot_wider(-votes, names_from = "domain", values_from = "weight", values_fill = 0)


#to count each vote separate (if SME votes on two domains = 2 votes instead of 2* 0.5) - as done in original scoring
domaintable_gsedadd_raw <- domaintable_gsedaddst %>%
  mutate(vote = 1) %>%
  group_by(item, domain) %>%
  summarise(votes = sum(vote), .groups = "drop") %>%
  pivot_wider(names_from = "domain", values_from = "votes", values_fill = 0) %>%
  mutate(Intern = 0,
         Extern = 0,
         Life_sk = 0) |>
  filter(!item %in% domaintable_raw$item)

gs1_subdomain_votes <- bind_rows(domaintable_raw, domaintable_gsedadd_raw) |>
  mutate(item = rename_vector(item, lexin = "gsed", lexout = "gsed3")) |>
  filter(item %in% get_itemnames(instrument = c("gs1")))

gs1_subdomain_p <-
  gs1_subdomain_votes |>
  pivot_longer(GM:Life_sk, names_to = "domain", values_to = "votes") |>
  group_by(item) |>
  mutate(n_votes = sum(votes, na.rm = T))|>
  ungroup() |>
  mutate(p = votes/n_votes) |>
  select(-n_votes, -votes) |>
  pivot_wider(names_from = "domain", values_from = "p")

gs1_domain_p <-
  gs1_subdomain_p |>
  mutate(grossmotor = GM,
         finemotor = FM,
         language = REC + EXP,
         cognitive = Prob_Solv + Exe_Func + Pre_ac,
         social = Self_reg + Emo_kn + Soc_com + Intern + Extern + Life_sk) |>
  select(item, grossmotor, finemotor, language, cognitive, social)


domaintable_gs1 <- gs1_domain_p |>
  pivot_longer(cols = c(grossmotor:social), names_to = "domain", values_to = "weight") |>
  mutate(set = "GFCLS") |>
  select(set, domain, item, weight) |>
  as.data.frame()



builtin_domaintable <- domaintable_gs1

usethis::use_data(builtin_domaintable, overwrite = TRUE)
