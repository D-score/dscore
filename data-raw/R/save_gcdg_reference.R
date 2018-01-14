fn <- path.expand("~/Package/dscore/dscore/data-raw/data/gcdg_reference.txt")
gcdg_reference <- read.delim(fn)

# using the fact that gcdg_reference is a normal model
# add percentiles/SD lines
percentiles <- c(3, 10, 25, 50, 75, 90, 97)
z <- c(qnorm(percentiles/100), -2:2)
mean <- gcdg_reference$mu
sd <- gcdg_reference$sigma * gcdg_reference$mu
m <- matrix(sd, nrow = length(sd), ncol = length(z))
z <- matrix(z, nrow = length(sd), ncol = length(z), byrow = TRUE)
p  <- round(mean + m * z, 2)
colnames(p) <- c(paste0("P", percentiles),
                 "SDM2", "SDM1", "SD0", "SDP1", "SDP2")
gcdg_reference <- cbind(gcdg_reference, p)

devtools::use_data(gcdg_reference, overwrite = TRUE)
