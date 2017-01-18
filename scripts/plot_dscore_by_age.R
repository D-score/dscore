# creates "d_a_study_model.pdf"

library("dscore")
library("dplyr", warn.conflicts = FALSE)
library("tidyr")
library("ggplot2")

# model_name <- "fx_1310"
# model_name <- "fr_1310"
model_name <- "d_1221"
fn <- file.path(getwd(), "store", paste(model_name, "RData", sep = "."))
load(file = fn)

# select data for reference layer
reference <- select(dscore::Dreference, month, SDM2:SDP2) %>%
  filter(month <= 30) %>%
  gather(key = centile, value = d, -month)

theme_set(theme_light())
plot <- ggplot(reference, aes(x = month, y = d, group = centile)) +
  scale_colour_manual(values = get_palette("study"), na.value = "grey") +
  scale_x_continuous("Age (in months)", limits = c(0, 60),
                     breaks = seq(0, 60, 12)) +
  scale_y_continuous(paste0("D-Score (", model_name, ")"), breaks = seq(0, 80, 20), 
                     limits = c(0, 80)) +
  geom_line(colour = "grey") +
  geom_point(mapping = aes(x = age, y = d, group = study, colour = study), 
             data = model$dscore, size = 0.5, shape = 1) +
  facet_wrap(~study) + 
  theme(legend.position = "none")

pdf_file <- file.path(getwd(), "results", paste0("d_a_study_overconstrained", model_name,".pdf"))
pdf(pdf_file, onefile = TRUE, width = 7, height = 7)
print(plot)
dev.off()



