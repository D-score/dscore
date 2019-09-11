# context("dscore")
# library("dplyr")
# library("tidyr")
# 
# data <- ddata::get_gcdg(study = "Netherlands 1", adm = TRUE)
# adm <- names(data)[1:5]
# items <- ddata::item_names(study = "Netherlands 1")
# key <- data.frame(item = items,
#                   tau = dscore::gettau(items = items, 
#                                          lexicon = "gcdg"))
# data$age <- data$age/12
# 
# # test new ability estimator
# start <- Sys.time()
# d1 <- dscore(data = data, items = items, age = "age", key = key)
# 
# # test full posterior output for first row of the data
# x <- data[1L, ]
# d11 <- dscore(data = x, item = items, age = "age", key = key, full = TRUE)
# 
# # compare with original dscore estimator
# start <- Sys.time()
# d2 <- data %>%
#   select(one_of(c(adm, items))) %>%
#   gather(item, score, -one_of(adm), na.rm = TRUE) %>%
#   arrange(country, study, id, age) %>%
#   group_by(study, id, age) %>%
#   summarise(d = dscore::dscore_vector(scores = score, items = item,
#                                ages = age, mu = "gcdg",
#                                itembank = itembank, lexicon = "gcdg")) %>%
#   ungroup()
# timed2 <-Sys.time()-start
# 
# 
# #compare calculation times
# # timed1
# timed2
#        
# #compare estimates
# head(d1)
# head(d2)
