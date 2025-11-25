
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("gridExtra")
#install.packages("patchwork")

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


# SCSS Results
scss_results <- read.xlsx("D:/Data/R/Semanvamd/semanvamd-results-scss.xlsx",sheetName = "Sheet1") #NVAMD
# scss_results <- read.xlsx("D:/Data/R/semaglutide-dr-results-main/results-scss-DT.xlsx",sheetName = "Sheet1") #NAION

# Convert relevant columns to numeric (handling '<5' as NA)
scss_results$hr <- as.numeric(as.character(scss_results$hr))
scss_results$hr_lb <- as.numeric(as.character(scss_results$hr_lb))
scss_results$hr_ub <- as.numeric(as.character(scss_results$hr_ub))

# Filter out rows where hr_lb or hr_ub is missing
scss_results <- scss_results %>% filter(!is.na(hr_lb) & !is.na(hr_ub))

# Fix copyright symbol unicode
scss_results$database_name <- stringr::str_replace_all(scss_results$database_name,"\xae","")
scss_results$database_name <- stringr::str_replace_all(scss_results$database_name,"Bayesian Synthesis","Meta-analysis")
scss_results$database_name <- stringr::str_replace_all(scss_results$database_name,"WU","WashU")
scss_results$database_name <- stringr::str_replace_all(scss_results$database_name,"®","")

# Sum N by exposure
scss_results <- scss_results %>% 
  group_by(exposure_name, outcome_name) %>% 
  mutate(exposure_n = max(outcome_subjects)) %>% 
  ungroup()

# Concatenate scss_results for exposure with N
scss_results <- scss_results %>% 
  mutate(exposure_label = paste0(exposure_name, " (N=", exposure_n, ")"))

# Order the bars
scss_results_ordered <- scss_results %>%
  # arrange(database_order) %>%  # Sort by your custom column
  # mutate(database_name = factor(database_name, levels = rev(unique(database_name)))) 
  arrange(outcome_subjects) %>%  # Sort by your custom column
  mutate(database_name = factor(database_name,levels = rev(c("Meta-analysis", sort(setdiff(database_name, "Meta-analysis"))))))
  


scss_results_ordered <- scss_results_ordered %>%
  #arrange(database_order) %>%  # Sort by your custom column
  #arrange(outcome_name) %>%  # Sort by your custom column
  mutate(outcome_subjects = factor(outcome_subjects, levels = unique(outcome_subjects))) %>% 
  mutate(exposure_label = factor(exposure_label, levels = unique(exposure_label)))



# Separate Sensitivity and Specificity HR calculations
scss_results_sens <- scss_results_ordered %>% filter(outcome_id==2) #17760
scss_results_spec <- scss_results_ordered %>% filter(outcome_id==1) #17761



# Create the forest plot for Sensitive NAION
SCSS_sens <- ggplot(scss_results_sens, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                                       color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3"))) +
  geom_point(size = 2.8) +
  geom_errorbarh(height = 0.5, linewidth=1.1) +
  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
  facet_wrap(~ exposure_label, scales = "free_y",ncol = 1) +
  scale_x_log10(limits = c(0.009,25), breaks = c(0.01, 0.05, 0.2, 0.5, 1, 2, 5), labels = c("0.01", "0.05","0.2", "0.5", "1", "2", "5")) +
  scale_color_identity() +
  labs(title = "Sensitive NVAMD",
       x = "Incidence Rate Ratio (95% CI)", y = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 13, margin = margin(t=13)),
        strip.text = element_text(size = 13),
        plot.margin = unit(c(1, 1.5, 0, 1), "lines")) + # without the bottom legend for ARVO paper
        # plot.margin = unit(c(1, 1.5, 1.5, 1), "lines")) + # with the bottom legend for presentation purpose
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
                label.color = NA,   # Removes border
                fontface = ifelse(p < 0.05, "bold", "plain")),
            hjust = -0.1, size = 3.5, color = "black", check_overlap = TRUE)
print(SCSS_sens)
# Create the forest plot for Specific NAION
SCSS_spec <- ggplot(scss_results_spec, aes(x = hr, y = database_name, xmin = hr_lb, xmax = hr_ub,
                                       color = ifelse(database_name == "Meta-analysis", "firebrick3", "dodgerblue3")),) +
  geom_point(size = 2.8) +
  geom_errorbarh(height = 0.5, linewidth=1.1) +
  geom_vline(xintercept = 1, linetype = "solid", color = "black") +
  facet_wrap(~ exposure_label, scales = "free_y",ncol = 1) +
  scale_x_log10(limits = c(0.01,100), breaks = c(0.01, 0.05, 0.2, 0.5, 1, 2, 5,20), labels = c("0.01", "0.05","0.2", "0.5", "1", "2", "5", "20")) +
  scale_color_identity() +
  labs(title = "Specific NVAMD",
       x = "Incidence Rate Ratio (95% CI)", y = "") +
  theme_minimal() +
  theme(panel.grid.minor = element_blank(),
        plot.title = element_text(size = 15, hjust = 0.5),
        axis.text.y = element_text(size = 11),
        axis.text.x = element_text(size = 11),
        axis.title.x = element_text(size = 13, margin = margin(t=13)),
        strip.text = element_text(size = 13),
        plot.margin = unit(c(1, 1.5, 0, 1), "lines")) + # without the bottom legend for ARVO paper
        # plot.margin = unit(c(1, 1.5, 1.5, 1), "lines")) + # with the bottom legend for presentation purpose
        #plot.margin = margin(10, 100, 10, 1)) +  # Reduce left & right margins
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
                label.color = NA,   # Removes border
                fontface = ifelse(p < 0.05, "bold", "plain")),
            hjust = -0.1, size = 3.5, color = "black", check_overlap = TRUE)
# print(SCSS_spec)

# Convert ggplot objects to grobs using ggplotGrob()
SCSS_sens_grob <- ggplotGrob(SCSS_sens)
SCSS_spec_grob <- ggplotGrob(SCSS_spec)

SCSS_combined_grobs <- grid.arrange(
  grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), SCSS_sens_grob),  # Add border around plot1
  grobTree(rectGrob(gp = gpar(lwd = 2, col = "black")), SCSS_spec_grob),  # Add border around plot2
  ncol = 2)

# Suggest: w-2500, H:1900
#ggsave(SCSS_combined_grobs,file = "C:/Users/bmart/OneDrive - Johns Hopkins/JHU Post-Doc/SemaglutideNAION/SCCS_plot_100224.eps", device = "eps", width = 70, height = 40, units = "cm")

#ggsave(SCSS_combined_grobs,file = "SemaNVAMD_SCCS_plot_110925.png", width = 2500, height = 1900, units = "px", dpi = 300)
#ggsave(SCSS_combined_grobs,file = "D:/Data/R/Semanvamd/SemaNVAMD_SCCS_plot_110925.eps", device = "eps", width = 70, height = 40, units = "cm")

# Export to 600 dpi resolution
# Helper function to save PDF
save_pdf <- function(grob, pdf_file, width = width_in, height = height_in) {
  pdf(pdf_file, width = width, height = height)
  grid.draw(grob)
  dev.off()
  pdf_file  # return pdf path for chaining
}

# Chain PDF creation -> JPG conversion
width_in <- 25; # DN: <- adjust for each images
height_in <- 15 # DN: <- adjust for each images
file_name <- "eFig3_SCCS" # DN: <- adjust for file name

SCSS_combined_grobs |>
  save_pdf(paste0(file_name,".pdf"),
           width = width_in,
           height = height_in) |>
  (\(pdf_file) pdf_convert(pdf_file, 
                           format = "jpeg", 
                           dpi = 600,
                           filenames = paste0(file_name,".jpg")))()


# AVRO figure
# The Legend added
# Ctrl + Shift + C for toggle the multiple lines comment

# This version using annotate
# text_plot <- ggplot() +
#   annotate(
#     "richtext",
#     x = 0, y = 1,
#     label = "<b>Abbreviations:</b> NVAMD = neovascular age-related macular degeneration, CI = confidence interval<br>
#              <b>Database Abbreviations:</b><br>
#              CCAE = Merative MarketScan Commercial Claims and Encounters Database<br>
#              Clinformatics = Optum Clinformatics Data Mart<br>
#              CUMC = Columbia University Medical Center<br>
#              JHME = Johns Hopkins Medical Enterprise<br>
#              MDCD = Merative MarketScan Multi-State Medicaid Database<br>
#              MDCR = Merative MarketScan Medicare Supplemental Database<br>
#              OHSU = Oregon Health & Science University<br>
#              USC = Keck Medical Center of USC<br>
#              VA = Department of Veterans Affairs<br>
#              WashU = Washington University in St. Louis<br>",
#     hjust = 0, vjust = 1,
#     fill = NA, label.color = NA,
#     size = 4
#   ) +
#   xlim(0, 1) + ylim(0, 1) +
#   theme_void()

# This version using geom_richtext
df_text <- data.frame(
  x = 0.01,
  y = 0.01,
  label = "<b>Abbreviations:</b> NVAMD = neovascular age-related macular degeneration, CI = confidence interval<br>
             <b>Database Abbreviations:</b><br>
             CCAE = Merative MarketScan Commercial Claims and Encounters Database<br>
             Clinformatics = Optum Clinformatics Data Mart<br>
             CUMC = Columbia University Medical Center<br>
             JHME = Johns Hopkins Medical Enterprise<br>
             MDCD = Merative MarketScan Multi-State Medicaid Database<br>
             MDCR = Merative MarketScan Medicare Supplemental Database<br>
             OHSU = Oregon Health & Science University<br>
             USC = Keck Medical Center of USC<br>
             VA = Department of Veterans Affairs<br>
             WashU = Washington University in St. Louis<br>"
)

text_plot <- ggplot(df_text) +
  geom_richtext(
    aes(x = x, y = y, label = label),
    hjust = 0,      # left align
    vjust = 0,      # top align
    fill = NA,
    label.color = NA,
    size = 4
  ) +
  xlim(0, 3) +
  ylim(0, 0.5) +
  theme_void()

g_text <- ggplotGrob(text_plot)

# Arrange layout
SCSS_combined_grobs <- grid.arrange(
  SCSS_sens_grob, SCSS_spec_grob,
  g_text,
  ncol = 2,
  nrow = 2,
  heights = c(4, 0.75),
  layout_matrix = rbind(
    c(1, 2),
    c(3, 3)
  )
)

# Export to 600 dpi resolution
# Helper function to save PDF
save_pdf <- function(grob, pdf_file, width = width_in, height = height_in) {
  pdf(pdf_file, width = width, height = height)
  grid.draw(grob)
  dev.off()
  pdf_file  # return pdf path for chaining
}

# Chain PDF creation -> JPG conversion
width_in <- 25; # DN: <- adjust for each images
height_in <- 17.8 # DN: <- adjust for each images
file_name <- "eFig3_SCCS_ARVO" # DN: <- adjust for file name

SCSS_combined_grobs |>
  save_pdf(paste0(file_name,".pdf"),
           width = width_in,
           height = height_in) |>
  (\(pdf_file) pdf_convert(pdf_file, 
                           format = "jpeg", 
                           dpi = 600,
                           filenames = paste0(file_name,".jpg")))()
