## ------------------------------------------------------------------------
# Actions taken in this script
# ------------- read data
# ------------- rename to itembank names
# ------------- reorganize to long matrix
# ------------- calculate D-score
# ------------- calculate D-score SDS
# ------------- export to external file

# ------------- preliminaries
library("dscore")
library("haven")
library("dplyr")

pkg <- path.expand("~/SV/SV-036487/DscorePackage en webtool/DscorepackageV7")
project <- path.expand("~/SV/SV-036487/POPS")
datafile <- file.path(project, "data/pops4Dscore.sav")  # Created by PvD 13oct2015

# ------------- read data
pops <- read_sav(datafile)
itembank <- read.delim2(file = file.path(pkg, "itembankVWOv4.txt"))
# repair one duplicate variable name in the itembank
levels(itembank$ID.VWO1996) <- c(levels(itembank$ID.VWO1996), "na10")
itembank[51, "ID.VWO1996"] <- "na10"

# ------------- rename to match itembank variable names ID.VWO1996
pops1 <- pops[, c(1:4, 5:8, 9:26)]
names(pops1) <- c("id", "db", "mb", "yb",
                  "dm", "mm", "ym", "age", 
                  "v1", "v28", "v51", "v52", "v55", "v29", "v2",
                  "v3", "v4", "v30", "v53", "v56", "v5", "v6", 
                  "v54", "na1", "v58", "v57")
pops2 <- pops[, c(1:4, 27:30, 31:48)]
names(pops2) <- c("id", "db", "mb", "yb",
                  "dm", "mm", "ym", "age", 
                  "v3", "v4", "v30", "v53", "v56", "v5", "v6",
                  "v54", "na1", "v58", "v57",
                  "v7", "v8", "v9", "v59", "v60", "v61", "v33")
pops3 <- pops[, c(1:4, 49:51, 52:71)]
names(pops3) <- c("id", "db", "mb", "yb",
                  "dm", "mm", "ym", "age", 
                  "v7", "V8", "V9", "v59", "v60", "v61", "v33",
                  "v62", "v10", "v63", "v64", "v36", "v34", 
                  "v11", "v12", "v65", "v66", "na2", "v37")
pops4 <- pops[, c(1:4, 72:75, 76:95)]
names(pops4) <- c("id", "db", "mb", "yb", 
                  "dm", "mm", "ym", "age", 
                  "v13", "v14", "v39", "na4", 
                  "v67", "v68", "v15", "v16", "na10", "v41", "na5",
                  "v69", "v70", "v17", "v18", "v19", "na6",
                  "v43", "v44", "v71")

# ------------- reorganize to long matrix
long <- bind_rows(pops1, pops2, pops3, pops4)
rm(pops1, pops2, pops3, pops4)
long <- arrange(long, id, age) %>% 
  mutate(dag = age * 365.25)


# ---- save first 97 records 
pops97 <- dplyr::slice(pops, 1:97)
  

# ------------- calculate D-score
source(file = file.path(pkg, "subfuncDDIv7.R"))
source(file = file.path(pkg, "Dscorelistv7.R"))
source(file = file.path(pkg, "subfuncDscorev7.R"))

reference <- read.table(file = file.path(pkg, "Dreference.txt"), header = TRUE)
#result <- Dscore(VWOdata = long, year = "1996", 
#                 itembankVWO = itembank, ref = reference)

# algorithm fails on high values of 'dag'. 
#result <- Dscore(VWOdata = long[long$dag<1016,], year = "1996", 
#                 itembankVWO = itembank, ref = reference)

# ------------- calculate D-score SDS
             
             
             
# ------------- export to external file
             
           

