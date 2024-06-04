library(dplyr)

# define project
path <- file.path("data-raw/data/references")

f1 <- file.path(path, "dutch.txt")
f2 <- file.path(path, "gcdg.txt")
f3 <- file.path(path, "phase1.txt")
f4 <- file.path(path, "Dutch_gsed2212.txt")
f5 <- file.path(path, "phase1_healthy.txt")

# ------------- dutch references
ref_dutch <- read.delim(file = f1) |>
  mutate(pop = "dutch")

# ------------- gcdg references
ref_gcdg <- read.delim(file = f2) |>
  mutate(pop = "gcdg")
# using the fact that gcdg_reference is a normal model
# add percentiles/SD lines
percentiles <- c(3, 10, 25, 50, 75, 90, 97)
z <- c(qnorm(percentiles / 100), -2:2)
mean <- ref_gcdg$mu
sd <- ref_gcdg$sigma * ref_gcdg$mu
m <- matrix(sd, nrow = length(sd), ncol = length(z))
z <- matrix(z, nrow = length(sd), ncol = length(z), byrow = TRUE)
p <- round(mean + m * z, 2)
colnames(p) <- c(
  paste0("P", percentiles),
  "SDM2", "SDM1", "SD0", "SDP1", "SDP2"
)
ref_gcdg <- bind_cols(ref_gcdg, data.frame(p))

# ------------- phase1 references
ref_phase1 <- read.delim(file = f3) |>
  mutate(
    pop = "phase1",
    P3 = dscore:::qBCT(0.03, mu, sigma, nu, tau),
    P10 = dscore:::qBCT(0.10, mu, sigma, nu, tau),
    P25 = dscore:::qBCT(0.25, mu, sigma, nu, tau),
    P50 = dscore:::qBCT(0.50, mu, sigma, nu, tau),
    P75 = dscore:::qBCT(0.75, mu, sigma, nu, tau),
    P90 = dscore:::qBCT(0.90, mu, sigma, nu, tau),
    P97 = dscore:::qBCT(0.97, mu, sigma, nu, tau),
    SDM2 = dscore:::qBCT(pnorm(-2), mu, sigma, nu, tau),
    SDM1 = dscore:::qBCT(pnorm(-1), mu, sigma, nu, tau),
    SD0 = dscore:::qBCT(pnorm(-0), mu, sigma, nu, tau),
    SDP1 = dscore:::qBCT(pnorm(+1), mu, sigma, nu, tau),
    SDP2 = dscore:::qBCT(pnorm(+2), mu, sigma, nu, tau)
  )

# ------------- Dutch references for gsed2212 key
ref_dutchgsed <- read.delim(file = f4) |>
  mutate(
    pop = "dutch_gsed2212",
    P3 = dscore:::qBCT(0.03, mu, sigma, nu, tau),
    P10 = dscore:::qBCT(0.10, mu, sigma, nu, tau),
    P25 = dscore:::qBCT(0.25, mu, sigma, nu, tau),
    P50 = dscore:::qBCT(0.50, mu, sigma, nu, tau),
    P75 = dscore:::qBCT(0.75, mu, sigma, nu, tau),
    P90 = dscore:::qBCT(0.90, mu, sigma, nu, tau),
    P97 = dscore:::qBCT(0.97, mu, sigma, nu, tau),
    SDM2 = dscore:::qBCT(pnorm(-2), mu, sigma, nu, tau),
    SDM1 = dscore:::qBCT(pnorm(-1), mu, sigma, nu, tau),
    SD0 = dscore:::qBCT(pnorm(-0), mu, sigma, nu, tau),
    SDP1 = dscore:::qBCT(pnorm(+1), mu, sigma, nu, tau),
    SDP2 = dscore:::qBCT(pnorm(+2), mu, sigma, nu, tau)
  )
# ------------- phase1 references for subset of healthy participants
ref_phase1_healthy <- read.delim(file = f5) |>
  mutate(
    pop = "phase1_healthy",
    P3 = dscore:::qBCT(0.03, mu, sigma, nu, tau),
    P10 = dscore:::qBCT(0.10, mu, sigma, nu, tau),
    P25 = dscore:::qBCT(0.25, mu, sigma, nu, tau),
    P50 = dscore:::qBCT(0.50, mu, sigma, nu, tau),
    P75 = dscore:::qBCT(0.75, mu, sigma, nu, tau),
    P90 = dscore:::qBCT(0.90, mu, sigma, nu, tau),
    P97 = dscore:::qBCT(0.97, mu, sigma, nu, tau),
    SDM2 = dscore:::qBCT(pnorm(-2), mu, sigma, nu, tau),
    SDM1 = dscore:::qBCT(pnorm(-1), mu, sigma, nu, tau),
    SD0 = dscore:::qBCT(pnorm(-0), mu, sigma, nu, tau),
    SDP1 = dscore:::qBCT(pnorm(+1), mu, sigma, nu, tau),
    SDP2 = dscore:::qBCT(pnorm(+2), mu, sigma, nu, tau)
  )
# save to /data
builtin_references <- bind_rows(ref_dutch, ref_gcdg, ref_phase1, ref_phase1_healthy, ref_dutchgsed) |>
  rename(age = year) |>
  dplyr::select(
    pop, age, mu, sigma, nu, tau,
    P3, P10, P25, P50, P75, P90, P97,
    SDM2, SDM1, SD0, SDP1, SDP2
  )

usethis::use_data(builtin_references, overwrite = TRUE)
