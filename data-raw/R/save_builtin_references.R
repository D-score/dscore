library(dplyr)

# define project
project <- path.expand("~/Package/dscore/dscore")
path    <- file.path(project, "data-raw/data/references")

f1 <- file.path(path, "dutch.txt")
f2 <- file.path(path, "gcdg.txt")

# ------------- dutch references
ref_dutch <- read.delim(file = f1) %>% 
  mutate(pop = "dutch")

# ------------- gcdg references
ref_gcdg <- read.delim(file = f2) %>% 
  mutate(pop = "gcdg") 
# using the fact that gcdg_reference is a normal model
# add percentiles/SD lines
percentiles <- c(3, 10, 25, 50, 75, 90, 97)
z <- c(qnorm(percentiles/100), -2:2)
mean <- ref_gcdg$mu
sd <- ref_gcdg$sigma * ref_gcdg$mu
m <- matrix(sd, nrow = length(sd), ncol = length(z))
z <- matrix(z, nrow = length(sd), ncol = length(z), byrow = TRUE)
p  <- round(mean + m * z, 2)
colnames(p) <- c(paste0("P", percentiles),
                 "SDM2", "SDM1", "SD0", "SDP1", "SDP2")
ref_gcdg <- bind_cols(ref_gcdg, data.frame(p))

# save to /data
builtin_references <- bind_rows(ref_dutch, ref_gcdg) 
usethis::use_data(builtin_references, overwrite = TRUE)
