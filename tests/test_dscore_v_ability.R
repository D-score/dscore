
#test data
source("R/posterior.R")
source("R/adp.R")
source("R/normalize.R")
source("R/ability.R")

data <- ddata::get_gcdg(study="Netherlands 1", adm = TRUE)
items <- dmetric::prepare_items(study = "Netherlands 1")$items
key <- data.frame(items=items,delta=dscore::gettau(items=items,lex="gcdg"))
data$age <- data$age/12


# test new ability estimator
start <- Sys.time()
d1 <- ability(data=data,items=items, age="age", key=key, pdist="dutch")
timed1 <-Sys.time()-start

# test full posterior output for first row of the data
x <- data[1,]
d11 <- ability(data=x,items=items, age="age", key=key, pdist="dutch", full=TRUE)



# compare with original dscore estimator
adm <- dmetric::prepare_items(study="Netherlands 1")$adm
start <- Sys.time()
d2 <- data %>%
  select(one_of(c(adm, items))) %>%
  gather(item, score, -one_of(adm), na.rm = TRUE) %>%
  arrange(country, study, id, age) %>%
  group_by(study, id, age) %>%
  summarise(d = dscore::dscore(scores = score, items = item,
                               ages = age / 12, mu = "dutch",
                               itembank = itembank, lexicon = "gcdg")) %>%
  ungroup()
timed2 <-Sys.time()-start


#compare calculation times
timed1
timed2
       
#compare estimates
head(d1)
head(d2)
