# do not forget to update the keys table
builtin_keys <- data.frame(
  key = c("dutch", "gcdg", "gsed1912", "sf2206", "lf2206",
          "gsed2206", "294_0", "293_0", "gsed2208", "gsed2212",
          "gsed2406"),
  base_population = c("dutch", "gcdg", "gcdg", "gcdg", "gcdg",
                      "gcdg", "gcdg", "phase1", "phase1", "phase1",
                      "preliminary_standards"),
  n_items = c(76, 565, 945, 278, 310,
              1126, 606, 293, 1407, 1478,
              1478),
  n_instruments = c(1, 13, 21, 7, 13,
                    22, 20, 2, 25, 27,
                    27),
  intercept = c(38.906, 66.483757, 66.174355, 66.174355, 66.174355,
                66.174355, 55, 54.939147, 54.939147, 54.939147,
                54.939147),
  slope = c(2.1044, 2.075044, 2.073871, 2.073871, 2.073871,
            2.073871, 4, 4.064264, 4.064264, 4.064264,
            4.064264),
  from = rep(-10, 11),
  to = c(80, rep(100, 10)),
  by = rep(1, 11),
  retired = c(FALSE, FALSE, FALSE, TRUE, TRUE,
              TRUE, TRUE, FALSE, TRUE, FALSE,
              FALSE),
  stringsAsFactors = FALSE
)

# remove retired keys
rem <- c("sf2206", "lf2206", "294_0", "gsed2206", "gsed2208")
builtin_keys <- builtin_keys[!builtin_keys$key %in% rem, ]

usethis::use_data(builtin_keys, overwrite = TRUE)
