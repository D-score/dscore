# test new equating facility of rasch() function
# SvB 14dec2016

library("sirt")

# Simulate data from the Rasch model

base::set.seed(9765)
N <- 50000    # number of persons
I <- 11       # number of items
b <- base::seq(-2, 2, length = I)
dat <- sirt::sim.raschtype(stats::rnorm(N), b)
base::colnames(dat) <- base::paste0("I", 1:I)

# Do the conventional analysis
fit1 <- rasch(dat)
fit1$item

# split items I6 and I9 into two parts
dat2 <- dat[, -c(6, 9)]
dat2 <- cbind(dat2,
              c(dat[1:(N/2),"I6"], rep(NA, N/2)),
              c(rep(NA, N/2), dat[(N/2+1):N,"I6"]),
              c(dat[1:(N/4),"I9"], rep(NA, 3*N/4)),
              c(rep(NA, N/4), dat[(N/4+1):N,"I9"])
              )
colnames(dat2)[10:13] <- c("I6a", "I6b","I9a","I9b")

# no equating, parameters close but not identical
fit2 <- rasch(dat2)
fit2$item

# equate I6a and I6b, and I9a and I9b
# parameters identical AND close to conventional analysis
fit3 <- rasch(dat2,
              equate = list(cube = c("I6a", "I6b"), wave = c("I9a", "I9b")))
fit3$item
