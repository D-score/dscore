library(dplyr)

# define project
path <- file.path("data-raw/data/references")

f1 <- file.path(path, "dutch.txt")
f2 <- file.path(path, "gcdg.txt")
f3 <- file.path(path, "phase1.txt")
f4 <- file.path(path, "Dutch_gsed2212.txt")
f5 <- file.path(path, "phase1_healthy.txt")

# ------------- dutch references
dutch_dutch <- read.delim(file = f1) |>
  mutate(population = "dutch",
         key = "dutch",
         distribution = "LMS")

# ------------- gcdg references
gcdg_gcdg <- read.delim(file = f2) |>
  mutate(population = "gcdg",
         key = "gcdg",
         distribution = "LMS")
# using the fact that gcdg_reference is a normal model
# add percentiles/SD lines
percentiles <- c(3, 10, 25, 50, 75, 90, 97)
z <- c(qnorm(percentiles / 100), -2:2)
mean <- gcdg_gcdg$mu
sd <- gcdg_gcdg$sigma * gcdg_gcdg$mu
m <- matrix(sd, nrow = length(sd), ncol = length(z))
z <- matrix(z, nrow = length(sd), ncol = length(z), byrow = TRUE)
p <- round(mean + m * z, 2)
colnames(p) <- c(
  paste0("P", percentiles),
  "SDM2", "SDM1", "SD0", "SDP1", "SDP2"
)
gcdg_gcdg <- bind_cols(gcdg_gcdg, data.frame(p))

# create copy for gsed1912
gcdg_gsed1912 <- gcdg_gcdg |>
  mutate(key = "gsed1912")

# ------------- phase1 references
phase1_gsed2212 <- read.delim(file = f3) |>
  mutate(
    population = "phase1",
    key = "gsed2212",
    distribution = "BCT",
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
dutch_gsed2212 <- read.delim(file = f4) |>
  mutate(
    population = "dutch",
    key = "gsed2212",
    distribution = "BCT",
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

# create copy for 293_0
phase1_293_0 <- phase1_gsed2212 |>
  mutate(key = "293_0")

# ------------- preliminiary_standards based on coarse covariate selection
preliminary_standards_gsed2406 <- read.delim(file = f5) |>
  mutate(
    population = "preliminary_standards",
    key = "gsed2406",
    distribution = "BCT",
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

# create copies for gsed2406
phase1_gsed2406 <- phase1_gsed2212 |>
  mutate(key = "gsed2406")
dutch_gsed2406 <- dutch_gsed2212 |>
  mutate(key = "gsed2406")

# save to /data
builtin_references <- bind_rows(
  dutch_dutch,
  gcdg_gcdg,
  gcdg_gsed1912,
  phase1_293_0,
  phase1_gsed2212,
  dutch_gsed2212,
  preliminary_standards_gsed2406,
  phase1_gsed2406,
  dutch_gsed2406) |>
  rename(age = year) |>
  dplyr::select(
    population, key, distribution,
    age, mu, sigma, nu, tau,
    P3, P10, P25, P50, P75, P90, P97,
    SDM2, SDM1, SD0, SDP1, SDP2
  )

usethis::use_data(builtin_references, overwrite = TRUE)
