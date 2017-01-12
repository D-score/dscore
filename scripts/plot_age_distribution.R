# Creates "Age distribution - range.pdf", "Age distribution - relative.pdf",
# and "Age distribution - absolute.pdf"
library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("ggplot2")

vars_adm <- c("country", "study", "id",
              "wave", "male",
              "age", "agedays")
adm <- select(tbl_df(gcdg), one_of(vars_adm))

rd <- file.path(getwd(), "results")

pdf(file = file.path(rd, "Age distribution - range.pdf"),
    width = 5, height = 5)
qplot(age, study, data = adm, geom = "boxplot")
dev.off()

# age distribution per study
pdf(file = file.path(rd, "Age distribution - relative.pdf"),
    width = 5, height = 20)
qplot(age, data = adm, geom = "histogram", binwidth = 0.25,
      xlab = "Age (months)", ylab = "Count") +
  scale_x_continuous(breaks = seq(0, 72, 12), limits = c(-1, 73)) +
  facet_grid(study ~ ., scales = "free")
dev.off()

# age distribution per study - scaled by study size
pdf(file = file.path(rd, "Age distribution - absolute.pdf"),
    width = 5, height = 20)
qplot(age, data = adm, geom = "histogram", binwidth = 0.25,
      xlab = "Age (months)", ylab = "Count") +
  scale_x_continuous(breaks = seq(0, 72, 12), limits = c(-1, 73)) +
  facet_grid(study ~ ., scales = "free", space = "free")
dev.off()
