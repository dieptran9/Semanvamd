# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("gridExtra")
# install.packages("patchwork")
# install.packages("ggtext")
# install.packages("Rcpp")
# install.packages("stringr")

library(ggplot2)
library(ggtext)
library(dplyr)
library(gridExtra)
library(patchwork)
library(grid)
library(stringr)
library(xlsx)
library(magick)
library(pdftools)

# CM Results
cm_results <- read.xlsx("D:/Data/R/Semanvamd/semanvamd-results-cm-sensitivity-analysis-1-series3.xlsx",1) #SemaNVAMD

# Convert relevant columns to numeric (handling '<5' as NA)
cm_results$hr <- as.numeric(as.character(cm_results$hr))
cm_results$hr_lb <- as.numeric(as.character(cm_results$hr_lb))
cm_results$hr_ub <- as.numeric(as.character(cm_results$hr_ub))

# Filter out rows where hr_lb or hr_ub is missing
cm_results <- cm_results %>% filter(!is.na(hr_lb) & !is.na(hr_ub))

# Fix copyright symbol unicode
cm_results$database_name <- stringr::str_replace_all(cm_results$database_name,"\xae","")
cm_results$database_name <- stringr::str_replace_all(cm_results$database_name,"®","")
cm_results$database_name <- stringr::str_replace_all(cm_results$database_name,"Bayesian Synthesis","Meta-analysis")

# Filter only Semaglutide comparisons
cm_results <- cm_results %>% 
  filter(target_cohort_id==301) # for Specific NVAMD

# Sum N by comparison
cm_results <- cm_results %>% 
  group_by(comparison_name, outcome_name) %>% 
  mutate(comparison_n = 2*max(group_n)) %>% 
  ungroup()

# Concatenate label for comparison with N
cm_results <- cm_results %>% 
  mutate(
    target_label = case_when(
      str_detect(target_name, "semaglutide")   ~ "Semaglutide",
      str_detect(target_name, "dulaglutide")   ~ "Dulaglutide",
      str_detect(target_name, "empagliflozin") ~ "Empagliflozin",
      str_detect(target_name, "sitagliptin")   ~ "Sitagliptin",
      str_detect(target_name, "glipizide")     ~ "Glipizide",
      TRUE ~ target_name
    ),
    comparator_label = case_when(
      str_detect(comparator_name, "semaglutide")   ~ "Semaglutide",
      str_detect(comparator_name, "dulaglutide")   ~ "Dulaglutide",
      str_detect(comparator_name, "empagliflozin") ~ "Empagliflozin",
      str_detect(comparator_name, "sitagliptin")   ~ "Sitagliptin",
      str_detect(comparator_name, "glipizide")     ~ "Glipizide",
      TRUE ~ comparator_name
    ),
    comparison_label = paste0(target_label, " v. ", comparator_label, " (N=", comparison_n, ")")
  )

# Order the bars
cm_results_ordered <- cm_results %>%
#  arrange(comparison_name) %>%  # Sort by your custom column
#  arrange(target_name) %>%  # Sort by your custom column
  mutate(database_name = factor(database_name,levels = rev(c("Meta-analysis", sort(setdiff(database_name, "Meta-analysis"))))))

cm_results_ordered <- cm_results_ordered %>%
  arrange(comparator_cohort_id)
#   arrange(comparison_label) %>%  # Sort by your custom column
#   mutate(comparison_name = factor(comparison_name, levels = unique(comparison_name))) %>% 
#   mutate(comparison_label = factor(comparison_label, levels = unique(comparison_label)))


#cm_results_ordered <- cm_results_ordered[ ,!(names(cm_results_ordered) %in% "X")]
cm_results_sens <- cm_results_ordered %>% filter(outcome_id==2) #Sensitive NVAMD
cm_results_spec <- cm_results_ordered %>% filter(outcome_id==1) #Specific NVAMD

# Create the forest plot for Sensitive NVAMD
CM_sens <- ggplot(cm_results_sens, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                            color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
                  geom_point(size = 2.8) +
                  geom_errorbarh(height = 0.3, linewidth=1.1) +
                  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
# Choose facet_wrap to order the subtitle as needed: (1) alphabet order (2) customize order
                  facet_wrap(~ comparison_label, scales = "free_y",ncol = 1) +
                  # facet_wrap(~factor(comparison_label, c("Semaglutide v. Dulaglutide (N=27228)",
                  #                                       "Semaglutide v. Empagliflozin (N=139832)",
                  #                                       "Semaglutide v. Sitagliptin (N=20352)",
                  #                                       "Semaglutide v. Glipizide (N=80710)")), scales = "free_y",ncol = 1) +
                  scale_x_log10(limits = c(0.1,500), breaks = c(0.1, 0.2, 0.5, 1, 2, 5, 10,100), labels = c("0.1","0.2", "0.5", "1", "2", "5", "10","100")) +
                  scale_color_identity() +
                  labs(title = "Sensitive NVAMD",
                       x = "Hazard Ratio (95% CI)", y = "") +
                  theme_minimal() +
                  theme(panel.grid.minor = element_blank(),
                        plot.title = element_text(size = 24, hjust = 0.5),
                        axis.text.y = element_text(size = 14),
                        axis.text.x = element_text(size = 14),
                        axis.title.x = element_text(size = 16, margin = margin(t=15)),
                        strip.text = element_text(size = 15),
                        plot.margin = unit(c(1, 2, 1, 1), "lines")) +
  geom_richtext(aes(x = max(hr_ub),
                label = ifelse(p < 0.001, 
                               paste0(sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "),  <i>P</i><0.001"),
                               paste0(sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "),  <i>P</i>=",
                                      sub("^0+", "", ifelse(p >= 0.01, format(round(p, 2)), format(round(p, 3)))))),
                label.color = NA,   # Removes border
                fontface = ifelse(p < 0.05, "bold", "plain")),
            hjust = -0.1, size = 4.5, color = "black", check_overlap = TRUE) #Adjust the horizontal of the text
print(CM_sens)
# Create the forest plot for Specific NVAMD
CM_spec <- ggplot(cm_results_spec, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                            color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
                  geom_point(size = 2.8) +
                  geom_errorbarh(height = 0.3, linewidth=1.1) +
                  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
# Choose facet_wrap to order the subtitle as needed: (1) alphabet order (2) customize order
                  facet_wrap(~ comparison_label, scales = "free_y",ncol = 1) +
                  # facet_wrap(~factor(comparison_label, c("Semaglutide v. Dulaglutide (N=27270)",
                  #                                       "Semaglutide v. Empagliflozin (N=83816)",
                  #                                       "Semaglutide v. Sitagliptin (N=20410)",
                  #                                       "Semaglutide v. Glipizide (N=41372)")), scales = "free_y",ncol = 1) +
  
                  scale_x_log10(limits = c(0.05,600), breaks = c(0.05,0.1, 0.2, 0.5, 1, 2, 10,100), labels = c("0.05","0.1","0.2", "0.5", "1", "2", "10","100")) +  #main model 300
                  scale_color_identity() +
                  labs(title = "Specific NVAMD",
                       x = "Hazard Ratio (95% CI)", y = "") +
                  theme_minimal() +
                  theme(panel.grid.minor = element_blank(),
                        plot.title = element_text(size = 24, hjust = 0.5),
                        axis.text.y = element_text(size = 14),
                        axis.text.x = element_text(size = 14),
                        axis.title.x = element_text(size = 16, margin = margin(t=15)),
                        strip.text = element_text(size = 16),
                        plot.margin = unit(c(1, 2, 1, 1), "lines")) +
  geom_richtext(aes(x = max(hr_ub),
                label = ifelse(p < 0.001, 
                               paste0(sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "), <i>P</i><0.001"),
                               paste0(sub("^0+", "", ifelse(hr >= 0.01, format(round(hr, 2)), format(round(hr, 3)))), 
                                      " (", 
                                      sub("^0+", "", ifelse(hr_lb >= 0.01, format(round(hr_lb, 2)), format(round(hr_lb, 3)))), 
                                      "-", 
                                      sub("^0+", "", ifelse(hr_ub >= 0.01, format(round(hr_ub, 2)), format(round(hr_ub, 3)))), 
                                      "), <i>P</i>=", 
                                      sub("^0+", "", ifelse(p >= 0.01, format(round(p, 2)), format(round(p, 3)))))),
                #fill = NA,          # Removes background
                label.color = NA,   # Removes border
                fontface = ifelse(p < 0.05, "bold", "plain")),
            hjust = -0.05, size = 4.5, color = "black", check_overlap = TRUE) #Adjust the horizontal of the text
# geom_richtext(aes(x = max(hr_ub), label = paste0("<b>P</b> and <i>P</i> text"),
#                   hjust = -0.1, size = 4.5, color = "black", check_overlap = TRUE))
# Convert ggplot objects to grobs using ggplotGrob()
CM_sens_grob <- ggplotGrob(CM_sens)
CM_spec_grob <- ggplotGrob(CM_spec)



CM_combined_grobs <- grid.arrange(
                        grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), CM_sens_grob),  # Add border around plot1
                        grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), CM_spec_grob),  # Add border around plot2
                             ncol = 2)


# ggsave(CM_combined_grobs,file = "C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/CM_plot_100224.eps", device = "eps", width = 70, height = 30, units = "cm")
# Note sensitivity 1 (with: 2100, heigh: 1080)

################################################################################
##                       REGULARLY WE STOP HERE                               ##
################################################################################


# This trick to convert pdf to jpg with 600 dpi
# library(magick)
# library(pdftools)
# Step 1: plot in pdf
# pdf("Sensitive_1_series2_v2.pdf",
#     width = 20,   # in inches, adjust to match layout
#     height = 8.5)   # in inches, adjust to match layout
# 
# grid.draw(CM_combined_grobs)   # draw the grob
# 
# dev.off()

# Step 2: convert from pdf to jpg
# pdf_file <- "D:/Data/R/Semanvamd/Sensitive_1_series2.pdf"
# img <- image_read_pdf(pdf_file, density = 600)  # density = dpi

# Save as JPG
# image_write(img, path = "Sensitive_1_series2_600dpi.jpg", format = "jpg")

# Helper function to save PDF

save_pdf <- function(grob, pdf_file, width = width_in, height = height_in) {
  pdf(pdf_file, width = width, height = height)
  grid.draw(grob)
  dev.off()
  pdf_file  # return pdf path for chaining
}

# Chain PDF creation -> JPG conversion
width_in <- 20; # DN: <- adjust for each images
height_in <- 12 # DN: <- adjust for each images
file_name <- "Sensitive_1_series3" # DN: <- adjust for file name
CM_combined_grobs |>
  save_pdf(paste0(file_name,".pdf"),
           width = width_in,
           height = height_in) |>
  (\(pdf_file) pdf_convert(pdf_file, 
                           format = "jpeg", 
                           dpi = 600,
                           filenames = paste0(file_name,".jpg")))()

