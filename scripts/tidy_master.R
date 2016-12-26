library("ddata")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")

dim(master)

vars_adm <- c("country", "study", "id",
              "wave", "male",
              "age", "agedays")
vars_cov <- c("edumo", "edumocat", "feduc", "meduc", "edumocat_other",
              "edumocat1", "edum",
              "ses", "ses6y",
              "gestage", "birthweight", "BLENGTH7", "birthlength",
              "agem",
              "residence",
              "ahaz", "haz", "height", "haz6y",
              "awaz", "waz", "weight", "waz6y", "waz18",
              "awhz", "whz", "whz18",
              "baz", "baz_18", "bmi6y")
vars_other <- c("agemth", "agemth1", "hdc",
                "hgtm", "hgtf",
                "ASSST7", "BMI_enrl", "bsiactdt",
                "X_merge", "dv",
                "v9005", "v9014", "v8",
                "ageappropiate_tepsi", "ageappropiate_tvip",
                "ageappropiate_cbcl1", "ageappropiate_tadi",
                "ageappropiate_bdi_st2", "tramo_BATTELLE",
                "PrenatalGroup_num", "postnatalgroup_num", "study.1",
                "group",
                "Ctviptot4", "CWM2tot4", "Crctot4", "CWM18Btot4",
                "CWM6tot4", "CWM5tot4", "CWM10tot4",
                "ppvt6y", "verbaliq", "performiq", "processiq",
                "fulliq", "devquot15", "sdqemotion", "sdqconduct",
                "sdqhyper", "sdqpeer", "sdqprosocial", "sdqtotaldif",
                "devquo24", "sescont", "age6y",
                "nutritiongroup", "ppvt7y", "spelling7y", "reading7y",
                "arithmetic7y", "ravens7y",
                "read_score", "math_score", "schoolage",
                "locn", "lft", "rec", "nrec", "src",
                "sexc",
                "bttid",
                "childdob", "childgender", "gestationage",
                "mateducation", "matschooling", "momage",
                "sa7yassessge", "sa7ydapinterviewage", "y2dateassess")

adm <- select(tbl_df(master), one_of(vars_adm))


library("ggplot2")

pl <- qplot(age, study, data = adm, geom = "boxplot")
pl


pl <- qplot(agedays, study, data = adm, geom = "boxplot")
pl

# age distribution per study
rd <- file.path(getwd(), "results")
pdf(file = file.path(rd, "Age distribution - relative.pdf"),
    width = 5, height = 10)
qplot(age, data = adm, geom = "histogram", binwidth = 0.25,
      xlab = "Age (months)", ylab = "Count") +
  scale_x_continuous(breaks = seq(0, 72, 12), limits = c(-1, 73)) +
  facet_grid(study ~ ., scales = "free")
dev.off()

# age distribution per study - scaled by study size
pdf(file = file.path(rd, "Age distribution - absolute.pdf"),
    width = 5, height = 10)
qplot(age, data = adm, geom = "histogram", binwidth = 0.25,
      xlab = "Age (months)", ylab = "Count") +
  scale_x_continuous(breaks = seq(0, 72, 12), limits = c(-1, 73)) +
  facet_grid(study ~ ., scales = "free", space = "free")
dev.off()

tidy <- master %>%
  select(1:239) %>%
  gather(key = item, value = value) %>%
  drop_na(value)

