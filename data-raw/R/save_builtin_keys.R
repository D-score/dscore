# do not forget to update the keys table
builtin_keys <- data.frame(
  key = c("dutch", "gcdg", "gsed1912", "sf2206", "lf2206",
          "gsed2206", "294_0", "293_0", "gsed2208", "gsed2212"),
  n_items = c(76, 565, 945, 278, 310,
              1126, 606, 293, 1407, 1478),
  n_instruments = c(1, 13, 21, 7, 13,
                    22, 20, 2, 25, 27),
  intercept = c(38.906, 66.174355, 66.174355, 66.174355, 66.174355,
                66.174355, 55, 54.939147, 54.939147, 54.939147),
  slope = c(2.1044, 2.073871, 2.073871, 2.073871, 2.073871,
            2.073871, 4, 4.064264, 4.064264, 4.064264),
  deprecated = c(FALSE, FALSE, FALSE, TRUE, TRUE,
                 TRUE, TRUE, FALSE, TRUE, FALSE),
  stringsAsFactors = FALSE
)

usethis::use_data(builtin_keys, overwrite = TRUE)
