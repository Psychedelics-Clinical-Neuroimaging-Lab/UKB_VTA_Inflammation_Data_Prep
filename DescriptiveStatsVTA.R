# -----------------------------------
# Script Purpose and Overview
# -----------------------------------
# This script performs data visualization and descriptive statistics for a dataset
# containing neuroimaging biomarkers and clinical measures related to depression severity.
#
# Main Objectives:
# 1. Generate descriptive statistics for the entire dataset and a subset of participants with a history of depression.
# 2. Visualize the distribution of recent depressive symptoms (RDS) scores:
#    - Overall distribution
#    - Distribution by ICD Depression status (Yes/No).
# 3. Create bar plots and interactive visualizations to explore the relationship between RDS scores and ICD history.
# 4. Calculate frequencies, proportions, and NA counts for key clinical and biomarker variables.
# 5. Save visualizations (e.g., bar plots) as images and interactive plots as HTML files for detailed analysis.
#
# Notes:
# - Ensure the required libraries are installed and file paths are correct before running the script.
# - Descriptive statistics provide insights into missing data, value distributions, and group-level differences.

# Author : Sarah Khalife
# Version: 1
# Date   : Dec 2024

# -----------------------------------

# -----------------------------------
# Load Required Libraries
# -----------------------------------
# Load libraries for data manipulation, visualization, and interactive plots
library(dplyr)     # Data manipulation
library(ggplot2)   # Data visualization
library(ggpubr)    # Advanced ggplot2 visualizations
library(skimr)     # Descriptive statistics
library(plotly)    # Interactive visualizations
library(htmlwidgets) # Save interactive plots as HTML
library(MatchIt)
library(gghalves)
library(psych)
library(GGally)
library(tidyverse)
library(car)
library(kableExtra)
library(officer)   # Exporting tables to word 
library(QuantPsyc)
library(ggExtra)



# -----------------------------------

# -----------------------------------
# As a continuation of the DataPreparationVTA.R script, we assume that merged_data was computed and saved in the environment
# -----------------------------------
str(merged_data)
summary(merged_data)

# -----------------------------------
# Descriptive Statistics
# -----------------------------------
# Select the relevant columns and get descriptive statistics

skim(merged_data %>% select(
  Participant_ID, Sex, Age_2, BMI_2, Depressed_2, Unenthusiasm_2, Restlessness_2, Lethargy_2, ICVUKB, CRP_1, CRP_0,
  WBC_0, WBC_1, WBC_2, Date_Assess_Center_0, Date_Assess_Center_1, Date_Assess_Center_2, Date_Assess_Center_3,
  Age_first_depression_2016, Age_last_depression_2016, Age_first_depression_2022, Age_last_depression_2022,
  Date_F32, Date_F33, Depressed_0, Unenthusiasm_0, Restlessness_0, Lethargy_0, Depressed_1, Unenthusiasm_1,
  Restlessness_1, Lethargy_1, Depressed_3, Unenthusiasm_3, Restlessness_3, Lethargy_3, PHQ_2016_Recent_Depression,
  PHQ_2016_Recent_Inadequacy, PHQ_2016_Recent_Tiredness, PHQ_2016_Recent_Lack_Interest, PHQ_2016_Recent_Appetite_change,
  PHQ_2016_Recent_Self_Harm, PHQ_2016_Recent_Trouble_Concentrating, PHQ_2016_Trouble_Falling_Asleep, PHQ_2016_Any_Sleep_Issues,
  Date_PHQ_2016, PHQ_2022_Recent_Depression, PHQ_2022_Recent_Inadequacy, PHQ_2022_Recent_Tiredness, PHQ_2022_Recent_Lack_Interest,
  PHQ_2022_Recent_Appetite_change, PHQ_2022_Recent_Self_Harm, PHQ_2022_Recent_Trouble_Concentrating, PHQ_2022_Trouble_Falling_Asleep,
  PHQ_2022_Any_Sleep_Issues, Date_PHQ_2022, COVID_2022_Depression, COVID_2022_Recent_Lack_Interest, Date_COVID_Ques_2022,
  Nb_Depression_Episodes_2, Longest_period_of_depression_2, Nb_Disinterest_Episodes_2, Longest_period_of_Disinterest_2,
  Seen_GP_Nerves_Depression_2, Seen_Psychiatrist_Nerves_Depression_2,Anxiety_2022_Restlessness,Anxiety_2016_Restlessness
))

positive_ICD_history <- merged_data[merged_data$ICD_Depression == "Yes", ]

# -----------------------------------
# Visualizations
# -----------------------------------

################ (Supplementary Figure 1) Frequency Distribution Figure: Visualizing Participants with varying Recent Depressive Symptoms (RDS) Score ################ 
# Calculate value counts and proportions
value_counts_hist <- table(merged_data$RDS_2)
Proportion_Dep <- round(prop.table(value_counts_hist) * 100, 2)

jpeg("All_Participants_Barplot.jpeg", width = 800, height = 600)

# Create the barplot
barplot_heights <- barplot(value_counts_hist, 
                           main = "Distribution of Participants by Recent Depressive Symptoms (RDS) Score", 
                           xlab = "RDS Score", 
                           ylab = "Number of Participants", 
                           col = "lightblue", 
                           ylim = c(0, max(value_counts_hist) + 3000))

# Add text labels for each bar with counts and percentages
text(barplot_heights, value_counts_hist, 
     labels = paste(value_counts_hist, "\n", "(", Proportion_Dep, "%)", sep = ""), 
     pos = 3, col = "black", cex = 0.8)

# Close the PNG device and save the file
dev.off()

################ (NOT reported in paper) Frequency Distribution Figure: Recreating the same figure but splitting Dataset by Depression Diagnosis ################ 

# Calculate the frequency (value) counts for RDS_2 stratifying them by ICD_Depression status
value_counts_split_RDS_ICD <- table(merged_data$ICD_Depression, merged_data$RDS_2)
Proportion_Dep_RDS_ICD <- round(prop.table(value_counts_split_RDS_ICD) * 100, 2)

print (value_counts_hist)
print (value_counts_split_RDS_ICD)
print (Proportion_Dep)
print (Proportion_Dep_RDS_ICD)

# Convert your table to a dataframe for plotting
value_counts_split_RDS_ICD_df <- as.data.frame(value_counts_split_RDS_ICD)
colnames(value_counts_split_RDS_ICD_df) <- c("ICD_Depression", "RDS_2", "Count")

# Create the base ggplot
Distribution_Depression_Severity <- ggplot(value_counts_split_RDS_ICD_df, aes(x = as.factor(RDS_2), y = Count, fill = ICD_Depression, text = paste("Count:", Count))) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("No" = "#56B4E9", "Yes" = "#E69F00")) +
  labs(title = "Distribution of Depression Severity (RDS_2) by ICD Depression Status",
       x = "Depression Severity (RDS_2)",
       y = "Count",
       fill = "ICD Depression") +
  theme_minimal()

# Use ggplotly to make the plot interactive and give it a useful name
interactive_RDS_ICD_plot <- ggplotly(Distribution_Depression_Severity, tooltip = "text")

# Display the renamed interactive plot
interactive_RDS_ICD_plot

# Save the interactive plot to an HTML file
saveWidget(interactive_RDS_ICD_plot, "Percentage_ICD_Depression_To_HC.html")

################ (NOT reported in paper) Frequency Distribution Figure: Visualising the RDS frequency of only the depression diagnosed sample  ################ 
# Calculate value counts and proportions
value_counts_hist_ICD_history <- table(positive_ICD_history$RDS_2)
Proportion_Dep_ICD_history <- round(prop.table(value_counts_hist_ICD_history) * 100, 2)

jpeg("Depression_Participants_Barplot.jpeg", width = 800, height = 600)

# Create the barplot
barplot_heights_ICD_history <- barplot(value_counts_hist_ICD_history, 
                           main = "Nb of Depressed Participants at each scale", 
                           xlab = "Values", 
                           ylab = "Frequency", 
                           col = "lightpink", 
                           ylim = c(0, max(value_counts_hist_ICD_history) + 100))

# Add text labels for each bar with counts and percentages
text(barplot_heights_ICD_history, value_counts_hist_ICD_history, 
     labels = paste(value_counts_hist_ICD_history, "\n", "(", Proportion_Dep_ICD_history, "%)", sep = ""), 
     pos = 3, col = "black", cex = 0.8)

print (value_counts_hist_ICD_history)
print (Proportion_Dep_ICD_history)

# Close the PNG device and save the file
dev.off()

#######################

# -----------------------------------
# (NOT reported in paper) Descriptive Statistics of the depressed sample
# -----------------------------------
# Get descriptive statistics for ICD positive sample

skim(positive_ICD_history %>% select(
  Age_2, BMI_2, RDS_0, RDS_1, RDS_2, RDS_3, RDS_4, RDS_5, ICVUKB, CRP_1, CRP_0,
  WBC_0, WBC_1, WBC_2,
  Date_PHQ_2016, 
  Date_PHQ_2022, 
  Nb_Depression_Episodes_2, Longest_period_of_depression_2, Nb_Disinterest_Episodes_2, Longest_period_of_Disinterest_2,
  Seen_GP_Nerves_Depression_2, Seen_Psychiatrist_Nerves_Depression_2
))

# Function to calculate NA counts, percentages, and value counts for each variable with rounding

calculate_na_and_counts <- function(positive_ICD_history) {
  results <- lapply(positive_ICD_history, function(column) {
    na_count <- sum(is.na(column))
    na_percent <- round((na_count / length(column)) * 100, 2)
    value_counts <- table(column, useNA = "ifany")
    value_percent <- round(prop.table(value_counts) * 100, 2)
    
    list(
      "NA_count" = na_count,
      "NA_percent" = na_percent,
      "Value_counts" = value_counts,
      "Value_percent" = value_percent
    )
  })
  
  results_df <- do.call(rbind, lapply(names(results), function(name) {
    data.frame(
      Variable = name,
      NA_count = results[[name]]$NA_count,
      NA_percent = results[[name]]$NA_percent,
      Value_counts = I(list(results[[name]]$Value_counts)),
      Value_percent = I(list(results[[name]]$Value_percent))
    )
  }))
  
  return(results_df)
}

# Select the relevant columns
selected_columns <- c( "Sex", "Depressed_2", "Unenthusiasm_2", "Restlessness_2", "Lethargy_2", "CRP_1", "CRP_0", "WBC_0", "WBC_1", "WBC_2", 
                       "Age_first_depression_2016", 
                       "Age_last_depression_2016", "Age_first_depression_2022", "Age_last_depression_2022", 
                       "Depressed_0", "Unenthusiasm_0", "Restlessness_0", "Lethargy_0", "Depressed_1", 
                       "Unenthusiasm_1", "Restlessness_1", "Lethargy_1", "Depressed_3", "Unenthusiasm_3", "Restlessness_3", 
                       "Lethargy_3", "PHQ_2016_Recent_Depression", "PHQ_2016_Recent_Inadequacy", "PHQ_2016_Recent_Tiredness", 
                       "PHQ_2016_Recent_Lack_Interest", "PHQ_2016_Recent_Appetite_change", "PHQ_2016_Recent_Self_Harm", 
                       "PHQ_2016_Recent_Trouble_Concentrating", "PHQ_2016_Trouble_Falling_Asleep", "PHQ_2016_Any_Sleep_Issues", 
                       "PHQ_2022_Recent_Depression", "PHQ_2022_Recent_Inadequacy", "PHQ_2022_Recent_Tiredness", 
                       "PHQ_2022_Recent_Lack_Interest", "PHQ_2022_Recent_Appetite_change", "PHQ_2022_Recent_Self_Harm", 
                       "PHQ_2022_Recent_Trouble_Concentrating", "PHQ_2022_Trouble_Falling_Asleep", "PHQ_2022_Any_Sleep_Issues", 
                       "COVID_2022_Depression", "COVID_2022_Recent_Lack_Interest",
                       "Nb_Depression_Episodes_2", "Longest_period_of_depression_2", "Nb_Disinterest_Episodes_2", 
                       "Longest_period_of_Disinterest_2", "Seen_GP_Nerves_Depression_2", "Seen_Psychiatrist_Nerves_Depression_2")

# Apply the function to the selected columns
frequency_summary <- calculate_na_and_counts(positive_ICD_history[selected_columns])

# View the summary
print(frequency_summary)

for (i in 1:nrow(frequency_summary)) {
  variable_name <- frequency_summary$Variable[i]
  value_counts <- frequency_summary$Value_counts[[i]]
  value_percent <- frequency_summary$Value_percent[[i]]
  
  cat("\nVariable:", variable_name, "\n")
  print(value_counts)
  print(value_percent)
}

# Descriptive stats were computed for the depressed group. This was done to see if we needed to do any follow up analyses other than our hypothesis

# -----------------------------------
# Analysis 1: Group Comparisons: ANOVAs-- Resampling of HC based on MDD group
# -----------------------------------

# Matched HC with MDD history group to see effect of chronic inflammation on biomarkers 

# -----------------------------------

##Splitting the data into MDD and HC 

## we defined current symptoms of depressions as having depression at least sometimes within the past week (if Depressed_2>1)
merged_data$ICD_Depression <- ifelse(merged_data$ICD_Depression == "Yes", 1, 0)

Total_HC <- subset(merged_data, ICD_Depression == 0)
table(Total_HC$Depressed_2)
##     1     2     3     4 
## 24689  3515   343   141 

Total_MDD <- subset(merged_data, ICD_Depression == 1)
table(Total_MDD$Depressed_2)
##    1    2    3    4
## 2215 1244  177  171 

################# Preparing Data: From this we will extract the Total_HC_matched
# Combine both datasets into one for matching
total_data <- rbind(Total_HC, Total_MDD)

# Perform propensity score matching on sex, age, and BMI
match_total <- matchit(ICD_Depression ~ Age_2 + Sex + BMI_2 + AgeSquared, 
                       data = total_data, 
                       method = "nearest", 
                       ratio = 1,  # 1:1 matching
                       replace = FALSE)  # No replacement, meaning each HC can only be used once

# Extract the matched data
matched_data <- match.data(match_total)

# Create the subset of matched HC
matched_HC <- subset(matched_data, ICD_Depression == 0)
matched_MDD <- subset(matched_data, ICD_Depression == 1)

####################################################################################Ran ANOVA for the 2 groups
ancova_VTA_ICVF_2Groups <- aov(VTA_ICVF ~ ICD_Depression, data = matched_data)
ancova_VTA_FW_2Groups <- aov(VTA_FW ~ ICD_Depression,  data = matched_data)
ancova_VTA_ISOVF_2Groups <- aov(VTA_ISOVF ~ ICD_Depression, data = matched_data)
ancova_VTA_OD_2Groups <- aov(VTA_OD ~ ICD_Depression, data = matched_data)
ancova_VTA_QSM_normalized_2Groups <- aov(VTA_QSM_normalized ~ ICD_Depression, data = matched_data)
ancova_VTA_Volume_Normalized_2Groups <- aov(VTA_Volume_Normalized ~ ICD_Depression, data = matched_data)

summary(ancova_VTA_ICVF_2Groups)
summary(ancova_VTA_FW_2Groups)
summary(ancova_VTA_ISOVF_2Groups)
summary(ancova_VTA_OD_2Groups)
summary(ancova_VTA_QSM_normalized_2Groups)
summary(ancova_VTA_Volume_Normalized_2Groups)

#remember to compare the alphas to 0.05/6 (bonferroni correction)

############################## Descriptives
# Calculate the mean and standard deviation of the variables for each group
matched_data %>%
  group_by(ICD_Depression) %>%
  summarise(
    mean_VTA_FW = mean(VTA_FW, na.rm = TRUE), sd_VTA_FW = sd(VTA_FW, na.rm = TRUE),
    mean_VTA_ISOVF = mean(VTA_ISOVF, na.rm = TRUE), sd_VTA_ISOVF = sd(VTA_ISOVF, na.rm = TRUE),
    mean_VTA_ICVF = mean(VTA_ICVF, na.rm = TRUE), sd_VTA_ICVF = sd(VTA_ICVF, na.rm = TRUE),
    mean_VTA_OD = mean(VTA_OD, na.rm = TRUE), sd_VTA_OD = sd(VTA_OD, na.rm = TRUE),
    mean_VTA_QSM = mean(VTA_QSM_normalized, na.rm = TRUE), sd_VTA_QSM = sd(VTA_QSM_normalized, na.rm = TRUE),
    mean_VTA_Volume = mean(VTA_Volume_Normalized, na.rm = TRUE), sd_VTA_Volume = sd(VTA_Volume_Normalized, na.rm = TRUE)
  )
# -----------------------------------
# Visualization of Analysis 1: Violin Plots - HC vs MDD for different biomarkers
# -----------------------------------

matched_data$ICD_Depression <- factor(matched_data$ICD_Depression, 
                                      levels = c(0, 1), 
                                      labels = c("HC", "With Depression history"))
p_corrected_threshold <- 0.05 / 6

#################Created a function for plotting violin Plots  


#ICVF
violinPlot_VTA_ICVF <- ggplot(matched_data, aes(x = "ICD Depression History", y = VTA_ICVF)) +
  
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), 
                   aes(x = "ICD Depression History", y = VTA_ICVF),
                   trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", 
                   color = NA, side = "l") +  # Left half for HC
  
  # Add right half violin for ICD Depression group (colored light blue)
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), 
                   aes(x = "ICD Depression History", y = VTA_ICVF),
                   trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", 
                   color = NA, side = "r") + # Right half for ICD Depression
  
  # Add jittered data points for HC (positioned at x = 0.75) behind other plots
  geom_jitter(data = matched_data %>% filter(ICD_Depression == "HC"), 
              aes(x = 0.75, y = VTA_ICVF), 
              shape = 16, position = position_jitter(width = 0.05), color = "grey60", size = 1.5, alpha = 0.7) +
  
  # Add jittered data points for ICD Depression History (positioned at x = 1.25) behind other plots
  geom_jitter(data = matched_data %>% filter(ICD_Depression != "HC"), 
              aes(x = 1.25, y = VTA_ICVF), 
              shape = 16, position = position_jitter(width = 0.05), color = "#6b95c4", size = 1.5, alpha = 0.5) +
  
  # Minimal theme for aesthetics
  theme_minimal(base_size = 15) +
  
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_ICVF),trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  
  # Add right half violin for ICD Depression group (colored light blue)
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_ICVF),trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  
  # Add half boxplot for HC group (aligned with the left half)
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression == "HC"), 
                    aes(x = "ICD Depression History", y = VTA_ICVF), 
                    width = 0.1, fill = "#f0f0f0", side = "l", 
                    color ="grey40", outlier.shape = NA) +
  
  # Add half boxplot for ICD Depression group (aligned with the right half)
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression != "HC"), 
                    aes(x = "ICD Depression History", y = VTA_ICVF), 
                    width = 0.1, fill = "#bad2f4", side = "r", 
                    color = "grey40", outlier.shape = NA) +
  
  # Axis labels and plot title
  labs(title = "Distribution of VTA_ICVF by ICD Depression History",
       x = "ICD Depression History", y = "VTA_ICVF") +
  
  # Adjust gridlines, axis lines, and set the background to white
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.8),
    panel.grid.minor = element_blank(),  # Remove minor grid lines
    panel.border = element_blank(),      # Remove panel border
    axis.line = element_blank(),         # Remove all axis lines
    panel.background = element_rect(fill = "white", color = NA),  # Set panel background to white
    plot.background = element_rect(fill = "white", color = NA),   # Set plot background to white
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    legend.position = "none"
  )

summary_ancova_VTA_ICVF <- summary(ancova_VTA_ICVF_2Groups)
p_value_ICVF <- summary_ancova_VTA_ICVF[[1]][["Pr(>F)"]][1]


# Add a line between the two groups just below the significance symbol
violinPlot_VTA_ICVF <- violinPlot_VTA_ICVF +
  geom_segment(aes(x = 0.75, xend = 1.25, 
                   y = max(matched_data$VTA_ICVF, na.rm = TRUE) + 0.015, 
                   yend = max(matched_data$VTA_ICVF, na.rm = TRUE) + 0.015), 
               color = "grey40", size = 0.8)

# Check significance and add symbols (after Bonferroni correction or before)
if (p_value_ICVF < p_corrected_threshold) {
  # Add * if significant after Bonferroni correction
  violinPlot_VTA_ICVF <- violinPlot_VTA_ICVF +
    annotate("text", x = 1, y = max(matched_data$VTA_ICVF, na.rm = TRUE) + 0.02, 
             label = "*", size = 6, color = "black")
} else if (p_value_ICVF < 0.05) {
  # Add # if significant before correction but not after
  violinPlot_VTA_ICVF <- violinPlot_VTA_ICVF +
    annotate("text", x = 1, y = max(matched_data$VTA_ICVF, na.rm = TRUE) + 0.02, 
             label = "#", size = 6, color = "black")
}

# Display the plot
print(violinPlot_VTA_ICVF)

##############################
## VTA_FW

violinPlot_VTA_FW <- ggplot(matched_data, aes(x = "ICD Depression History", y = VTA_FW)) +
  
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_FW), trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_FW), trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_jitter(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = 0.75, y = VTA_FW), shape = 16, position = position_jitter(width = 0.05), color = "grey60", size = 1.5, alpha = 0.7) +
  geom_jitter(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = 1.25, y = VTA_FW), shape = 16, position = position_jitter(width = 0.05), color = "#6b95c4", size = 1.5, alpha = 0.5) +
  theme_minimal(base_size = 15) +
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_FW),trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_FW),trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_FW), width = 0.1, fill = "#f0f0f0", side = "l", color ="grey40", outlier.shape = NA) +
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_FW), width = 0.1, fill = "#bad2f4", side = "r", color = "grey40", outlier.shape = NA) +
  labs(title = "Distribution of VTA_FW by ICD Depression History", x = "ICD Depression History", y = "VTA_FW") +
  theme(panel.grid.major = element_line(color = "grey80", size = 0.8), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),axis.title.x = element_text(size = 14, face = "bold"),axis.title.y = element_text(size = 14, face = "bold"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "none"
  )

summary_ancova_VTA_FW <- summary(ancova_VTA_FW_2Groups)
p_value_FW <- summary_ancova_VTA_FW[[1]][["Pr(>F)"]][1]

# Add a line between the two groups just below the significance symbol
violinPlot_VTA_FW <- violinPlot_VTA_FW + geom_segment(aes(x = 0.75, xend = 1.25, y = max(matched_data$VTA_FW, na.rm = TRUE) + 0.015, yend = max(matched_data$VTA_FW, na.rm = TRUE) + 0.015), color = "grey40", size = 0.8)

# Check significance and add symbols (after Bonferroni correction or before)
if (p_value_FW < p_corrected_threshold) {
  # Add * if significant after Bonferroni correction
  violinPlot_VTA_FW <- violinPlot_VTA_FW + annotate("text", x = 1, y = max(matched_data$VTA_FW, na.rm = TRUE) + 0.02, label = "*", size = 8, color = "black")
} else if (p_value_FM < 0.05) {
  # Add # if significant before correction but not after
  violinPlot_VTA_FW <- violinPlot_VTA_FW + annotate("text", x = 1, y = max(matched_data$VTA_FW, na.rm = TRUE) + 0.02, label = "#", size = 6, color = "black")
}

# Display the plot
print(violinPlot_VTA_FW)

##############################################
violinPlot_VTA_OD <- ggplot(matched_data, aes(x = "ICD Depression History", y = VTA_OD)) +
  
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_OD), trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_OD), trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_jitter(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = 0.75, y = VTA_OD), shape = 16, position = position_jitter(width = 0.05), color = "grey60", size = 1.5, alpha = 0.7) +
  geom_jitter(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = 1.25, y = VTA_OD), shape = 16, position = position_jitter(width = 0.05), color = "#6b95c4", size = 1.5, alpha = 0.5) +
  theme_minimal(base_size = 15) +
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_OD),trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_OD),trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_OD), width = 0.1, fill = "#f0f0f0", side = "l", color ="grey40", outlier.shape = NA) +
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_OD), width = 0.1, fill = "#bad2f4", side = "r", color = "grey40", outlier.shape = NA) +
  labs(title = "Distribution of VTA_OD by ICD Depression History", x = "ICD Depression History", y = "VTA_OD") +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.8), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14, face = "bold"), axis.title.y = element_text(size = 14, face = "bold"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "none"
  )

summary_ancova_VTA_OD <- summary(ancova_VTA_OD_2Groups)
p_value_OD <- summary_ancova_VTA_OD[[1]][["Pr(>F)"]][1]

# P values 
violinPlot_VTA_OD <- violinPlot_VTA_OD +
  geom_segment(aes(x = 0.75, xend = 1.25, y = max(matched_data$VTA_OD, na.rm = TRUE) + 0.015, yend = max(matched_data$VTA_OD, na.rm = TRUE) + 0.015), color = "grey40", size = 0.8)

# Check significance and add symbols (after Bonferroni correction or before)
if (p_value_OD < p_corrected_threshold) {
  # Add * if significant after Bonferroni correction
  violinPlot_VTA_OD <- violinPlot_VTA_OD + annotate("text", x = 1, y = max(matched_data$VTA_OD, na.rm = TRUE) + 0.02, label = "*", size = 8, color = "black")
} else if (p_value_OD < 0.05) {
  # Add # if significant before correction but not after
  violinPlot_VTA_OD <- violinPlot_VTA_OD + annotate("text", x = 1, y = max(matched_data$VTA_OD, na.rm = TRUE) + 0.02, label = "#", size = 6, color = "black")
}

# Display the plot
print(violinPlot_VTA_OD)

###################################################
## VTA_ISOVF
violinPlot_VTA_ISOVF <- ggplot(matched_data, aes(x = "ICD Depression History", y = VTA_ISOVF)) +
  
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_ISOVF), trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_ISOVF), trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_jitter(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = 0.75, y = VTA_ISOVF), shape = 16, position = position_jitter(width = 0.05), color = "grey60", size = 1.5, alpha = 0.7) +
  geom_jitter(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = 1.25, y = VTA_ISOVF), shape = 16, position = position_jitter(width = 0.05), color = "#6b95c4", size = 1.5, alpha = 0.5) +
  theme_minimal(base_size = 15) +
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_ISOVF),trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_ISOVF),trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_ISOVF), width = 0.1, fill = "#f0f0f0", side = "l", color ="grey40", outlier.shape = NA) +
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_ISOVF), width = 0.1, fill = "#bad2f4", side = "r", color = "grey40", outlier.shape = NA) +
  labs(title = "Distribution of VTA_ISOVF by ICD Depression History", x = "ICD Depression History", y = "VTA_ISOVF") +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.8), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14, face = "bold"), axis.title.y = element_text(size = 14, face = "bold"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "none"
  )

summary_ancova_VTA_ISOVF <- summary(ancova_VTA_ISOVF_2Groups)
p_value_ISOVF <- summary_ancova_VTA_ISOVF[[1]][["Pr(>F)"]][1]

# P values 
violinPlot_VTA_ISOVF <- violinPlot_VTA_ISOVF +
  geom_segment(aes(x = 0.75, xend = 1.25, y = max(matched_data$VTA_ISOVF, na.rm = TRUE) + 0.015, yend = max(matched_data$VTA_ISOVF, na.rm = TRUE) + 0.015), color = "grey40", size = 0.8)

# Check significance and add symbols (after Bonferroni correction or before)
if (p_value_ISOVF < p_corrected_threshold) {
  # Add * if significant after Bonferroni correction
  violinPlot_VTA_ISOVF <- violinPlot_VTA_ISOVF + annotate("text", x = 1, y = max(matched_data$VTA_ISOVF, na.rm = TRUE) + 0.02, label = "*", size = 8, color = "black")
} else if (p_value_ISOVF < 0.05) {
  # Add # if significant before correction but not after
  violinPlot_VTA_ISOVF <- violinPlot_VTA_ISOVF + annotate("text", x = 1, y = max(matched_data$VTA_ISOVF, na.rm = TRUE) + 0.02, label = "#", size = 6, color = "black")
}

# Display the plot
print(violinPlot_VTA_ISOVF)
###################################################
## VTA_QSM
violinPlot_VTA_QSM_normalized <- ggplot(matched_data, aes(x = "ICD Depression History", y = VTA_QSM_normalized)) +
  
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_QSM_normalized), trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_QSM_normalized), trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_jitter(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = 0.75, y = VTA_QSM_normalized), shape = 16, position = position_jitter(width = 0.05), color = "grey60", size = 1.5, alpha = 0.7) +
  geom_jitter(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = 1.25, y = VTA_QSM_normalized), shape = 16, position = position_jitter(width = 0.05), color = "#6b95c4", size = 1.5, alpha = 0.5) +
  theme_minimal(base_size = 15) +
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_QSM_normalized),trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_QSM_normalized),trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_QSM_normalized), width = 0.1, fill = "#f0f0f0", side = "l", color ="grey40", outlier.shape = NA) +
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_QSM_normalized), width = 0.1, fill = "#bad2f4", side = "r", color = "grey40", outlier.shape = NA) +
  labs(title = "Distribution of VTA_QSM_normalized by ICD Depression History", x = "ICD Depression History", y = "VTA_QSM_normalized") +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.8), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14, face = "bold"), axis.title.y = element_text(size = 14, face = "bold"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "none"
  )

summary_ancova_VTA_QSM_normalized <- summary(ancova_VTA_QSM_normalized_2Groups)
p_value_QSM_normalized <- summary_ancova_VTA_QSM_normalized[[1]][["Pr(>F)"]][1]

# P values 
violinPlot_VTA_QSM_normalized <- violinPlot_VTA_QSM_normalized +
  geom_segment(aes(x = 0.75, xend = 1.25, y = max(matched_data$VTA_QSM_normalized, na.rm = TRUE) + 0.015, yend = max(matched_data$VTA_QSM_normalized, na.rm = TRUE) + 0.015), color = "grey40", size = 0.8)

# Check significance and add symbols (after Bonferroni correction or before)
if (p_value_QSM_normalized < p_corrected_threshold) {
  # Add * if significant after Bonferroni correction
  violinPlot_VTA_QSM_normalized <- violinPlot_VTA_QSM_normalized + annotate("text", x = 1, y = max(matched_data$VTA_QSM_normalized, na.rm = TRUE) + 0.02, label = "*", size = 8, color = "black")
} else if (p_value_QSM_normalized < 0.05) {
  # Add # if significant before correction but not after
  violinPlot_VTA_QSM_normalized <- violinPlot_VTA_QSM_normalized + annotate("text", x = 1, y = max(matched_data$VTA_QSM_normalized, na.rm = TRUE) + 0.02, label = "#", size = 6, color = "black")
}

# Display the plot
print(violinPlot_VTA_QSM_normalized)

###########################################################
## VTA_Volume
violinPlot_VTA_Volume_Normalized <- ggplot(matched_data, aes(x = "ICD Depression History", y = VTA_Volume_Normalized)) +
  
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_Volume_Normalized), trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_Volume_Normalized), trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_jitter(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = 0.75, y = VTA_Volume_Normalized), shape = 16, position = position_jitter(width = 0.05), color = "grey60", size = 1.5, alpha = 0.7) +
  geom_jitter(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = 1.25, y = VTA_Volume_Normalized), shape = 16, position = position_jitter(width = 0.05), color = "#6b95c4", size = 1.5, alpha = 0.5) +
  theme_minimal(base_size = 15) +
  geom_half_violin(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_Volume_Normalized),trim = FALSE, scale = "width", alpha = 0.7, fill = "#f0f0f0", color = NA, side = "l") +  # Left half for HC
  geom_half_violin(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_Volume_Normalized),trim = FALSE, scale = "width", alpha = 0.7, fill = "#bad2f4", color = NA, side = "r") + # Right half for ICD Depression
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression == "HC"), aes(x = "ICD Depression History", y = VTA_Volume_Normalized), width = 0.1, fill = "#f0f0f0", side = "l", color ="grey40", outlier.shape = NA) +
  geom_half_boxplot(data = matched_data %>% filter(ICD_Depression != "HC"), aes(x = "ICD Depression History", y = VTA_Volume_Normalized), width = 0.1, fill = "#bad2f4", side = "r", color = "grey40", outlier.shape = NA) +
  labs(title = "Distribution of VTA_Volume_Normalized by ICD Depression History", x = "ICD Depression History", y = "VTA_Volume_Normalized") +
  theme(
    panel.grid.major = element_line(color = "grey80", size = 0.8), panel.grid.minor = element_blank(), panel.border = element_blank(), axis.line = element_blank(), panel.background = element_rect(fill = "white", color = NA), plot.background = element_rect(fill = "white", color = NA), plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), axis.title.x = element_text(size = 14, face = "bold"), axis.title.y = element_text(size = 14, face = "bold"), axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.position = "none"
  )

summary_ancova_VTA_Volume_Normalized <- summary(ancova_VTA_Volume_Normalized_2Groups)
p_value_Volume_Normalized <- summary_ancova_VTA_Volume_Normalized[[1]][["Pr(>F)"]][1]

# P values 
violinPlot_VTA_Volume_Normalized <- violinPlot_VTA_Volume_Normalized +
  geom_segment(aes(x = 0.75, xend = 1.25, y = max(matched_data$VTA_Volume_Normalized, na.rm = TRUE) + 0.00015, yend = max(matched_data$VTA_Volume_Normalized, na.rm = TRUE) + 0.00015), color = "grey40", size = 0.8)

# Check significance and add symbols (after Bonferroni correction or before)
if (p_value_Volume_Normalized < p_corrected_threshold) {
  # Add * if significant after Bonferroni correction
  violinPlot_VTA_Volume_Normalized <- violinPlot_VTA_Volume_Normalized + annotate("text", x = 1, y = max(matched_data$VTA_Volume_Normalized, na.rm = TRUE) + 0.02, label = "*", size = 8, color = "black")
} else if (p_value_Volume_Normalized < 0.05) {
  # Add # if significant before correction but not after
  violinPlot_VTA_Volume_Normalized <- violinPlot_VTA_Volume_Normalized + annotate("text", x = 1, y = max(matched_data$VTA_Volume_Normalized, na.rm = TRUE) + 0.02, label = "#", size = 6, color = "black")
}

# Display the plot
print(violinPlot_VTA_Volume_Normalized)
########################################################### 

###############
# Install and load the 'effectsize' package if you haven't already
install.packages("effectsize")
library(effectsize)

# Assuming your data is in a dataframe and you have run ANOVAs for each model

# For ancova_VTA_ICVF_2Groups
partial_eta_ICVF <- eta_squared(ancova_VTA_ICVF_2Groups)
print(partial_eta_ICVF)

# For ancova_VTA_FW_2Groups
partial_eta_FW <- eta_squared(ancova_VTA_FW_2Groups)
print(partial_eta_FW)

# For ancova_VTA_ISOVF_2Groups
partial_eta_ISOVF <- eta_squared(ancova_VTA_ISOVF_2Groups)
print(partial_eta_ISOVF)

# For ancova_VTA_OD_2Groups
partial_eta_OD <- eta_squared(ancova_VTA_OD_2Groups)
print(partial_eta_OD)

# For ancova_VTA_QSM_normalized_2Groups
partial_eta_QSM <- eta_squared(ancova_VTA_QSM_normalized_2Groups)
print(partial_eta_QSM)

# For ancova_VTA_Volume_Normalized_2Groups
partial_eta_Volume <- eta_squared(ancova_VTA_Volume_Normalized_2Groups)
print(partial_eta_Volume)

####################
# -----------------------------------
# Analysis 2: Linear Regression: ANOVAs-- Resampling of HC based on MDD group
# -----------------------------------

#Because we saw that HC were represented way more in the dataset, we wanted to make sure that they are not driving the effect by over representing HC soo we tried to balance the sampling by selecting HC based on sex and age of the MDD group 
# -----------------------------------

###### Fitting residuals vs fitted values - did not end up reported in the paper but is useful to visualize
# model <- lm(RDS_2 ~ VTA_ICVF + VTA_ISOVF + VTA_OD + VTA_QSM_normalized + VTA_Volume_Normalized + Sex + BMI_2 + Age_2 + AgeSquared, data = merged_data)
# 
# # Create a residuals vs. fitted plot
# plot(fitted(model), resid(model), main = "Residuals vs Fitted Values")
# abline(h = 0, col = "red")
# 
# # Install and load the lmtest package
# library(lmtest)
# 
# # Run the Breusch-Pagan test
# bptest(model)
################### This indicated heteroscedasticity (most ppl have RDS_2 (4))


## Now for the actual Linear Regression and Data Matching

########## Step 1: Prepare Data for Regression
# Remove IDs to anonymize data
LinearRegressionMDD <- Total_MDD[, -1]

# Select relevant columns (excluding volume data initially)
LinearRegressionMDD <- LinearRegressionMDD[, c("Sex", "Age_2", "AgeSquared", "BMI_2", "RDS_2", 
                                               "VTA_ICVF", "VTA_FW", "VTA_ISOVF", 
                                               "VTA_OD", "VTA_QSM_normalized", 
                                               "VTA_Volume_Normalized")]

########## Step 2: Conduct Initial Regression and Check Assumptions
# Perform multiple linear regression
LinearRegressionMDDStep1 <- lm(RDS_2 ~ ., data = LinearRegressionMDD)
summary(LinearRegressionMDDStep1)

# Check multicollinearity using Variance Inflation Factor (VIF)
vif_values_LinearRegressionMDDStep1 <- vif(LinearRegressionMDDStep1)
print(vif_values_LinearRegressionMDDStep1)

########## Step 3: Address Multicollinearity by Removing Highly Correlated Variables
# Remove "VTA_FW" due to high collinearity
LinearRegressionMDD <- LinearRegressionMDD[, -which(names(LinearRegressionMDD) == "VTA_FW")]

# Rerun regression after removing "VTA_FW"
LinearRegressionMDDStep2 <- lm(RDS_2 ~ ., data = LinearRegressionMDD)
summary(LinearRegressionMDDStep2)

# Recalculate VIF values
vif_values_LinearRegressionMDDStep2 <- vif(LinearRegressionMDDStep2)
print(vif_values_LinearRegressionMDDStep2)

########## Step 4: Match Subjects by RDS Scores and Demographics


# Set random seed for reproducibility
set.seed(196)

# Define reference group (RDS > 12) and calculate demographic averages
reference_group <- merged_data %>% filter(RDS_2 > 12)
reference_age_mean <- mean(reference_group$Age_2)
reference_sex_ratio <- mean(reference_group$Sex == "Male")

# Optimize subject matching across RDS groups
num_iterations <- 50000
best_match <- NULL
best_age_diff <- Inf
best_sex_diff <- Inf

# Perform multiple iterations to find the best match
for (i in 1:num_iterations) {
  selected_subjects <- data.frame()
  
  # For each unique RDS score, sample up to 100 subjects
  for (rds in unique(merged_data$RDS_2)) {
    rds_subset <- merged_data %>% filter(RDS_2 == rds)
    if (nrow(rds_subset) > 100) {
      rds_subset <- rds_subset %>% sample_n(100)
    }
    selected_subjects <- bind_rows(selected_subjects, rds_subset)
  }
  
  # Calculate demographic differences between selected and reference groups
  selected_age_mean <- mean(selected_subjects$Age_2)
  selected_sex_ratio <- mean(selected_subjects$Sex == "Male")
  age_diff <- abs(selected_age_mean - reference_age_mean)
  sex_diff <- abs(selected_sex_ratio - reference_sex_ratio)
  
  if (age_diff + sex_diff < best_age_diff + best_sex_diff) {
    best_match <- selected_subjects
    best_age_diff <- age_diff
    best_sex_diff <- sex_diff
  }
}

# Save matched subject IDs
selected_ids <- best_match$ID
id_dataframe <- data.frame(ID = selected_ids)
write.csv(id_dataframe, "best_match_ids.csv", row.names = FALSE)
cat("ID numbers saved to 'best_match_ids.csv'\n")

# Report demographics of the best match
best_age_mean <- mean(best_match$Age_2)
best_sex_ratio <- mean(best_match$Sex == "Male")
cat("Best Age Mean:", best_age_mean, "\n")
cat("Best Sex Ratio (Male):", best_sex_ratio, "\n")

###########################################################
# Step 5: Analyze Matched Dataset
###########################################################

# Perform regression on matched dataset
LinearRegressionMDDbest_match_df <- best_match[, c("Sex", "Age_2", "AgeSquared", "BMI_2", "RDS_2", 
                                                   "VTA_ICVF", "VTA_ISOVF", "VTA_OD", 
                                                   "VTA_QSM_normalized", "VTA_Volume_Normalized")]

LinearRegressionMDDbest_match_df$Sex <- as.numeric(LinearRegressionMDDbest_match_df$Sex)  # Converts factor to numeric levels

LinearRegressionMDDbest_match <- lm(RDS_2 ~ ., data = LinearRegressionMDDbest_match_df)
summary(LinearRegressionMDDbest_match)

# Run lm.beta to get standardized beta coefficients
standardized_betas <- lm.beta(LinearRegressionMDDbest_match)

# Calculate VIF and report
vif_values_LinearRegressionMDDbest_match <- vif(LinearRegressionMDDbest_match)
print(vif_values_LinearRegressionMDDbest_match)

# Compute 95% confidence intervals
confint(LinearRegressionMDDbest_match, level = 0.95)

###########################################################
# Step 6: Predicted vs Observed Plot
###########################################################

# Load required libraries
library(ggplot2)
library(ggExtra)

# Get predicted values from the model
LinearRegressionMDDbest_match_df$Predicted_RDS_2 <- predict(LinearRegressionMDDbest_match, newdata = LinearRegressionMDDbest_match_df)

# Extract numeric vectors for plotting
X <- LinearRegressionMDDbest_match_df$Predicted_RDS_2
Y <- LinearRegressionMDDbest_match_df$RDS_2

# Compute correlation and standard deviations
r_value <- cor(X, Y)   # Pearson correlation
sdX <- sd(X)           # Standard deviation of Predicted RDS
sdY <- sd(Y)           # Standard deviation of Observed RDS

# Create annotation text with correlation and standard deviations
annotation_text <- paste0(
  "r = ", round(r_value, 3), "\n",
  "σ(Predicted) = ", round(sdX, 3), "\n",
  "σ(Observed) = ", round(sdY, 3)
)

# Base scatter plot
p <- ggplot(LinearRegressionMDDbest_match_df, 
            aes(x = Predicted_RDS_2, y = RDS_2)) +
  geom_point(color = "#6b95c4", alpha = 0.6) +            # Scatter plot
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +  # Identity line (y = x)
  #geom_smooth(method = "lm", formula = y ~ x, color = "red", se = TRUE) +  # Best-fit regression line
  coord_cartesian(xlim = c(4, 16), ylim = c(4, 16)) +  # Zoom without cropping data
  labs(
    title = "Predicted vs. Observed Depression Severity 
    (Old Model)",
    x = "Predicted RDS",
    y = "Observed RDS"
  ) +
  theme_minimal(base_size = 13) +  # Apply a clean theme
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  # Centered title
    axis.title = element_text(size = 16),  # Axis labels
    axis.title.x = element_text(size = 14, face = "bold"),  # Increase x-axis title size
    axis.title.y = element_text(size = 14, face = "bold"),  # Increase y-axis title size
    plot.margin = margin(t = 40, r = 10, b = 10, l = 10)  # Add extra spacing at the top
    
  ) +
  annotate(
    "text",
    x = 3.5,    # Move further left
    y = 16.25, # Upper part of the graph
    label = annotation_text,
    hjust = 0,  # Left-align text
    vjust = 1,  # Top-align text
    size = 4.5,
    color = "black"
  )

# Add marginal density or histogram plots
p_with_marginals <- ggMarginal(
  p,
  type = "density",  # Change to "histogram" for histogram instead of density
  fill = "#6b95c4", 
  alpha = 1
)

# Render the plot with marginal distributions
print(p_with_marginals)


###########################################################
# Step 7: Beta Estimates 
###########################################################
# Load necessary libraries
library(ggplot2)
library(broom)
library(dplyr)

# Step 1: Remove Predicted_RDS_2 if it exists
LinearRegressionMDDbest_match_df <- LinearRegressionMDDbest_match_df %>% 
  select(-Predicted_RDS_2)

# Step 2: Standardize predictors (excluding RDS_2)
predictors <- LinearRegressionMDDbest_match_df %>% select(-RDS_2)
predictors_standardized <- as.data.frame(scale(predictors))

# Keep dependent variable (RDS_2) in original scale
LinearRegressionMDDbest_match_standardized <- cbind(
  RDS_2 = LinearRegressionMDDbest_match_df$RDS_2, 
  predictors_standardized
)

# Step 3: Fit standardized regression model
model_standardized <- lm(RDS_2 ~ ., data = LinearRegressionMDDbest_match_standardized)

# Step 4: Extract coefficients & confidence intervals
coef_df_standardized <- broom::tidy(model_standardized, conf.int = TRUE) %>%
  rename(Term = term, Beta = estimate, CI_low = conf.low, CI_high = conf.high, p_value = p.value) %>%
  filter(Term != "(Intercept)") %>%
  arrange(desc(Beta))  # Sort by effect size

# Step 5: Rename variables for the y-axis
coef_df_standardized$Term <- factor(
  coef_df_standardized$Term,
  levels = coef_df_standardized$Term,  # Maintain order
  labels = c("ODI", "BMI", "ICVF", "Normalized QSM", "Age²", 
             "Volume Ratio", "ISOVF", "Sex", "Age")
)

# Step 6: Create the improved Beta Coefficient Plot
ggplot(coef_df_standardized, aes(x = reorder(Term, Beta), y = Beta, fill = Beta > 0)) +
  
  # Bar plot for beta coefficients
  geom_col(show.legend = FALSE, width = 0.6) +
  
  # Confidence intervals (error bars)
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "black", linewidth = 0.8) +
  
  # Dashed zero-reference line
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  
  # Improved labels & theme
  labs(
    title = "Standardized Beta Coefficients for Depression Severity\n",
    subtitle = "Predictors with 95% Confidence Intervals",
    x = "Predictors",
    y = "Standardized Beta Coefficient"
  ) +
  
  # Updated color scheme: Grey (negative), Blue (positive)
  scale_fill_manual(values = c("#808080", "#377EB8")) +  # Grey for negative, Blue for positive
  
  # Theme for a clean, publication-ready look with adjusted font sizes
  theme_minimal(base_size = 16) +  # Increases the overall text size slightly
  
  # Customize text & axis elements
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 1),  # Shift title to the left
    plot.subtitle = element_text(size = 14, hjust = 0.7),  # Keep subtitle aligned with title
    axis.title.x = element_text(size = 14, face = "bold"),  # Increase x-axis title size
    axis.title.y = element_text(size = 15, face = "bold"),  # Increase y-axis title size
    axis.text.x = element_text(size = 16),  # Increase x-axis text
    axis.text.y = element_text(size = 12),  # Increase y-axis text (slightly smaller than x-axis)
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10),  # Add extra spacing at the top
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) +
  
  # Flip coordinates for readability
  coord_flip()

# Step 7: Display extracted values in descending order
print(coef_df_standardized)

####################
# -----------------------------------
# Supplementary Figure 2: Correlations and variability 
# -----------------------------------

# Subset inflammatory variables for correlation analysis
InflammationVariables_best_match <- best_match[, c("VTA_FW", "VTA_ICVF", "VTA_ISOVF", "VTA_OD",
                                                   "VTA_QSM_normalized", "VTA_Volume_Normalized", 
                                                   "BMI_2", "Age_2", "AgeSquared")]

# Calculate correlation matrix with p-values
correlation_matrix_best_match <- corr.test(InflammationVariables_best_match)
print(correlation_matrix_best_match)

# Visualize correlations
ggcorr(InflammationVariables_best_match, palette = "RdBu", label = TRUE)

# Create scatter plot matrix for visualizing relationships
ggpairs(InflammationVariables_best_match, columns = 1:9, 
        lower = list(continuous = wrap("smooth", colour = "pink", alpha = 0.1)),
        upper = list(continuous = wrap("cor", method = "pearson", size = 2.5))) +
  ggtitle("Scatter Plot Matrix") +
  theme(axis.text = element_text(size = 5),
        axis.title = element_text(size = 5),
        plot.title = element_text(size = 10, face = "bold", hjust = 0.5))

# Not used in paper
#-----------------------------------
# Computing the average duration between ICD diagnosis and scan
# -----------------------------------
# Filter data to exclude rows with NA in Date_F32, Date_F33, or Date_Assess_Center_2
filtered_data_ICD_Dates <- best_match[!is.na(best_match$Date_F32) &
                               !is.na(best_match$Date_F33) &
                               !is.na(best_match$Date_Assess_Center_2), ]

# Calculate the difference in years between Date_F32 or Date_F33 and Date_Assess_Center_2
filtered_data_ICD_Dates$Years_Diff_F32 <- as.numeric(difftime(filtered_data_ICD_Dates$Date_Assess_Center_2,
                                                    filtered_data_ICD_Dates$Date_F32,
                                                    units = "days")) / 365.25

filtered_data_ICD_Dates$Years_Diff_F33 <- as.numeric(difftime(filtered_data_ICD_Dates$Date_Assess_Center_2,
                                                    filtered_data_ICD_Dates$Date_F33,
                                                    units = "days")) / 365.25

# Take the maximum difference for each row (farthest diagnosis from the assessment date)
filtered_data_ICD_Dates$Farthest_Years_Diff <- pmax(filtered_data_ICD_Dates$Years_Diff_F32,
                                          filtered_data_ICD_Dates$Years_Diff_F33,
                                          na.rm = TRUE)

# Calculate the average and standard deviation of the maximum differences
average_years_diff <- mean(filtered_data_ICD_Dates$Farthest_Years_Diff, na.rm = TRUE)
std_years_diff <- sd(filtered_data_ICD_Dates$Farthest_Years_Diff, na.rm = TRUE)

# Print the results
average_years_diff
std_years_diff
# -----------------------------------

# ################################################### Table 1
# Install and load necessary packages if not already done
library(kableExtra)
library(dplyr)

# Function to format scientific notation with readable exponents
scientific_format <- function(x, digits = 3) {
  exponent <- floor(log10(abs(x)))
  base <- round(x / 10^exponent, digits = digits)
  # Replace exponent with superscript formatting
  exponent_str <- gsub("-", "\u207B", as.character(exponent)) # Negative superscript
  exponent_str <- chartr("0123456789", "\u2070\u00B9\u00B2\u00B3\u2074\u2075\u2076\u2077\u2078\u2079", exponent_str)
  return(paste0(base, " × 10", exponent_str))
}

# Function to calculate p-values
calculate_p_value <- function(variable) {
  hc_values <- matched_HC[[variable]]
  depression_values <- matched_MDD[[variable]]
  if (is.numeric(hc_values) && is.numeric(depression_values)) {
    # Using t-test if data is normally distributed; Wilcoxon otherwise
    test <- try(t.test(hc_values, depression_values, na.rm = TRUE)$p.value, silent = TRUE)
    if (inherits(test, "try-error")) {
      test <- wilcox.test(hc_values, depression_values, na.rm = TRUE)$p.value
    }
  } else {
    test <- NA  # NA for categorical data or missing values
  }
  return(test)
}

# Function to add significance code to p-value
significance_code <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else if (p < 0.1) return(".")
  else return(" ")
}

# Calculating p-values and adding significance codes
p_values <- c(
  NA, # "N" count does not have a p-value
  NA, # "Female n (%)" is categorical; no p-value computed here
  calculate_p_value("Age_2"),
  calculate_p_value("BMI_2"),
  calculate_p_value("RDS_2"),
  NA, # Empty row for "MRI Derived Metrics"
  calculate_p_value("VTA_ICVF"),
  calculate_p_value("VTA_OD"),
  calculate_p_value("VTA_ISOVF"),
  calculate_p_value("VTA_FW"),
  calculate_p_value("VTA_QSM_normalized"),
  calculate_p_value("VTA_Volume_Normalized")
)

# Corrected format_p_value function
format_p_value <- function(p) {
  if (is.na(p)) {
    return("")  # Leave NA as an empty string
  } else if (p < 0.001) {
    return(paste0("p&lt;0.001", significance_code(p)))  # Use &lt; for < symbol
    } else {
    formatted_p <- formatC(p, format = "f", digits = 3)  # Format other p-values with 3 decimal places
    return(paste0(formatted_p, significance_code(p)))  # Append significance code if applicable
  }
}
# Apply conditional formatting to p-values
p_value_display <- sapply(p_values, format_p_value)


# Create a summarized table with only the 4 desired columns (excluding Best_Match)
summary_table1 <- data.frame(
  Variable = c("N", "Female n (%)", "Age (Years)", "BMI (kg/m²)", "RDS",
               "MRI Derived Metrics", "ICVF", "ODI", "ISOVF", "FW", "Magnetic Susceptibility", "Volume Ratio"),

  Total = c(
    formatC(nrow(merged_data), format = "f", big.mark = ",", digits = 0),
    paste0(formatC(sum(merged_data$Sex == "Female"), format = "f", big.mark = ",", digits = 0),
           " (", round(mean(merged_data$Sex == "Female") * 100, 1), "%)"),
    paste0(round(mean(merged_data$Age_2, na.rm = TRUE), 1), " (", round(sd(merged_data$Age_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(merged_data$BMI_2, na.rm = TRUE), 1), " (", round(sd(merged_data$BMI_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(merged_data$RDS_2, na.rm = TRUE), 1), " (", round(sd(merged_data$RDS_2, na.rm = TRUE), 1), ")"),
    "",  # Empty row for title
    paste0(round(mean(merged_data$VTA_ICVF, na.rm = TRUE), 3), " (", round(sd(merged_data$VTA_ICVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(merged_data$VTA_OD, na.rm = TRUE), 3), " (", round(sd(merged_data$VTA_OD, na.rm = TRUE), 3), ")"),
    paste0(round(mean(merged_data$VTA_ISOVF, na.rm = TRUE), 3), " (", round(sd(merged_data$VTA_ISOVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(merged_data$VTA_FW, na.rm = TRUE), 3), " (", round(sd(merged_data$VTA_FW, na.rm = TRUE), 3), ")"),
    paste0(round(mean(merged_data$VTA_QSM_normalized, na.rm = TRUE), 3), " (", round(sd(merged_data$VTA_QSM_normalized, na.rm = TRUE), 3), ")"),
    paste0(scientific_format(mean(merged_data$VTA_Volume_Normalized, na.rm = TRUE), digits = 3),
           " (", scientific_format(sd(merged_data$VTA_Volume_Normalized, na.rm = TRUE), digits = 3), ")")
  ),

  HC = c(
    formatC(nrow(matched_HC), format = "f", big.mark = ",", digits = 0),
    paste0(formatC(sum(matched_HC$Sex == "Female"), format = "f", big.mark = ",", digits = 0),
           " (", round(mean(matched_HC$Sex == "Female") * 100, 1), "%)"),
    paste0(round(mean(matched_HC$Age_2, na.rm = TRUE), 1), " (", round(sd(matched_HC$Age_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(matched_HC$BMI_2, na.rm = TRUE), 1), " (", round(sd(matched_HC$BMI_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(matched_HC$RDS_2, na.rm = TRUE), 1), " (", round(sd(matched_HC$RDS_2, na.rm = TRUE), 1), ")"),
    "",  # Empty row for title
    paste0(round(mean(matched_HC$VTA_ICVF, na.rm = TRUE), 3), " (", round(sd(matched_HC$VTA_ICVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(matched_HC$VTA_OD, na.rm = TRUE), 3), " (", round(sd(matched_HC$VTA_OD, na.rm = TRUE), 3), ")"),
    paste0(round(mean(matched_HC$VTA_ISOVF, na.rm = TRUE), 3), " (", round(sd(matched_HC$VTA_ISOVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(matched_HC$VTA_FW, na.rm = TRUE), 3), " (", round(sd(matched_HC$VTA_FW, na.rm = TRUE), 3), ")"),
    paste0(round(mean(matched_HC$VTA_QSM_normalized, na.rm = TRUE), 3), " (", round(sd(matched_HC$VTA_QSM_normalized, na.rm = TRUE), 3), ")"),
    paste0(scientific_format(mean(matched_HC$VTA_Volume_Normalized, na.rm = TRUE), digits = 3),
           " (", scientific_format(sd(matched_HC$VTA_Volume_Normalized, na.rm = TRUE), digits = 3), ")")
  ),

  `Depression History` = c(
    formatC(nrow(matched_MDD), format = "f", big.mark = ",", digits = 0),
    paste0(formatC(sum(matched_MDD$Sex == "Female"), format = "f", big.mark = ",", digits = 0),
           " (", round(mean(matched_MDD$Sex == "Female") * 100, 1), "%)"),
    paste0(round(mean(matched_MDD$Age_2, na.rm = TRUE), 1), " (", round(sd(matched_MDD$Age_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(matched_MDD$BMI_2, na.rm = TRUE), 1), " (", round(sd(matched_MDD$BMI_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(matched_MDD$RDS_2, na.rm = TRUE), 1), " (", round(sd(matched_MDD$RDS_2, na.rm = TRUE), 1), ")"),
    "",  # Empty row for title
    paste0(round(mean(matched_MDD$VTA_ICVF, na.rm = TRUE), 3), " (", round(sd(matched_MDD$VTA_ICVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(matched_MDD$VTA_OD, na.rm = TRUE), 3), " (", round(sd(matched_MDD$VTA_OD, na.rm = TRUE), 3), ")"),
    paste0(round(mean(matched_MDD$VTA_ISOVF, na.rm = TRUE), 3), " (", round(sd(matched_MDD$VTA_ISOVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(matched_MDD$VTA_FW, na.rm = TRUE), 3), " (", round(sd(matched_MDD$VTA_FW, na.rm = TRUE), 3), ")"),
    paste0(round(mean(matched_MDD$VTA_QSM_normalized, na.rm = TRUE), 3), " (", round(sd(matched_MDD$VTA_QSM_normalized, na.rm = TRUE), 3), ")"),
    paste0(scientific_format(mean(matched_MDD$VTA_Volume_Normalized, na.rm = TRUE), digits = 3),
           " (", scientific_format(sd(matched_MDD$VTA_Volume_Normalized, na.rm = TRUE), digits = 3), ")")
  ),
  # Adding the calculated p-values with significance codes as a new column
  `p-value` = p_value_display
  )

# Set column names without conversion
summary_table1 <- setNames(summary_table1, c("Variable", "Total", "HC", "Depression History", "p-value"))

# Create a publication-ready table
kable(summary_table1, format = "html", row.names = FALSE, escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(6, bold = TRUE, italic = TRUE, background = "#f0f0f0")  # Custom formatting for the title row "MRI Derived Metrics"

#### Save as Word 
library(officer)

# Assuming summary_table is your data.frame
# Create a Word document
doc <- read_docx()

# Add a title
doc <- body_add_par(doc, "Summary Table", style = "heading 1")

# Add the table without specifying a style (use default style)
doc <- body_add_table(doc, value = summary_table1, style = "Normal")

# Save the Word document
print(doc, target = "~/Desktop/dataVTA/Table_1.docx")

######################################################## Table 2
########################################################
# Load necessary packages
library(kableExtra)
library(dplyr)

# Define reference group (RDS < 13) 
group_lowRDSS <- best_match %>% filter(RDS_2 < 13)

# Define scientific format function for readable exponents
scientific_format <- function(x, digits = 3) {
  exponent <- floor(log10(abs(x)))
  base <- round(x / 10^exponent, digits = digits)
  exponent_str <- gsub("-", "\u207B", as.character(exponent))  # For negative superscripts
  exponent_str <- chartr("0123456789", "\u2070\u00B9\u00B2\u00B3\u2074\u2075\u2076\u2077\u2078\u2079", exponent_str)
  return(paste0(base, " × 10", exponent_str))
}

# Function to format p-values conditionally
format_p_value <- function(p) {
  if (is.na(p)) return("")
  if (p < 0.001 || p > 1e6) { formatC(p, format = "e", digits = 2) }
  else { formatC(p, format = "f", digits = 3) }
}

# Define a sample dataset for the table with p-values
summary_table2 <- data.frame(
  Variable = c("N", "Female n (%)", "Age (Years)", "BMI (kg/m²)", "RDS", "MRI Derived Metrics", "ICVF", "ODI",
               "ISOVF", "FW", "Magnetic Susceptibility", "Volume Ratio"),

  `Total Subsample` = c(
    formatC(nrow(best_match), format = "f", big.mark = ",", digits = 0),
    paste0(formatC(sum(best_match$Sex == "Female"), format = "f", big.mark = ",", digits = 0),
           " (", round(mean(best_match$Sex == "Female") * 100, 1), "%)"),
    paste0(round(mean(best_match$Age_2, na.rm = TRUE), 1), " (", round(sd(best_match$Age_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(best_match$BMI_2, na.rm = TRUE), 1), " (", round(sd(best_match$BMI_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(best_match$RDS_2, na.rm = TRUE), 1), " (", round(sd(best_match$RDS_2, na.rm = TRUE), 1), ")"),
    "",  # Empty row for title
    paste0(round(mean(best_match$VTA_ICVF, na.rm = TRUE), 3), " (", round(sd(best_match$VTA_ICVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(best_match$VTA_OD, na.rm = TRUE), 3), " (", round(sd(best_match$VTA_OD, na.rm = TRUE), 3), ")"),
    paste0(round(mean(best_match$VTA_ISOVF, na.rm = TRUE), 3), " (", round(sd(best_match$VTA_ISOVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(best_match$VTA_FW, na.rm = TRUE), 3), " (", round(sd(best_match$VTA_FW, na.rm = TRUE), 3), ")"),
    paste0(round(mean(best_match$VTA_QSM_normalized, na.rm = TRUE), 3), " (", round(sd(best_match$VTA_QSM_normalized, na.rm = TRUE), 3), ")"),
    paste0(scientific_format(mean(best_match$VTA_Volume_Normalized, na.rm = TRUE), digits = 3),
           " (", scientific_format(sd(best_match$VTA_Volume_Normalized, na.rm = TRUE), digits = 3), ")")

  ),
  `(RDS < 13)` = c(
    formatC(nrow(group_lowRDSS), format = "f", big.mark = ",", digits = 0),
    paste0(formatC(sum(group_lowRDSS$Sex == "Female"), format = "f", big.mark = ",", digits = 0),
           " (", round(mean(group_lowRDSS$Sex == "Female") * 100, 1), "%)"),
    paste0(round(mean(group_lowRDSS$Age_2, na.rm = TRUE), 1), " (", round(sd(group_lowRDSS$Age_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(group_lowRDSS$BMI_2, na.rm = TRUE), 1), " (", round(sd(group_lowRDSS$BMI_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(group_lowRDSS$RDS_2, na.rm = TRUE), 1), " (", round(sd(group_lowRDSS$RDS_2, na.rm = TRUE), 1), ")"),
    "",  # Empty row for title
    paste0(round(mean(group_lowRDSS$VTA_ICVF, na.rm = TRUE), 3), " (", round(sd(group_lowRDSS$VTA_ICVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(group_lowRDSS$VTA_OD, na.rm = TRUE), 3), " (", round(sd(group_lowRDSS$VTA_OD, na.rm = TRUE), 3), ")"),
    paste0(round(mean(group_lowRDSS$VTA_ISOVF, na.rm = TRUE), 3), " (", round(sd(group_lowRDSS$VTA_ISOVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(group_lowRDSS$VTA_FW, na.rm = TRUE), 3), " (", round(sd(group_lowRDSS$VTA_FW, na.rm = TRUE), 3), ")"),
    paste0(round(mean(group_lowRDSS$VTA_QSM_normalized, na.rm = TRUE), 3), " (", round(sd(group_lowRDSS$VTA_QSM_normalized, na.rm = TRUE), 3), ")"),
    paste0(scientific_format(mean(group_lowRDSS$VTA_Volume_Normalized, na.rm = TRUE), digits = 3),
           " (", scientific_format(sd(group_lowRDSS$VTA_Volume_Normalized, na.rm = TRUE), digits = 3), ")")

  ),
  `(RDS ≥ 13)` = c(
    formatC(nrow(reference_group), format = "f", big.mark = ",", digits = 0),
    paste0(formatC(sum(reference_group$Sex == "Female"), format = "f", big.mark = ",", digits = 0),
           " (", round(mean(reference_group$Sex == "Female") * 100, 1), "%)"),
    paste0(round(mean(reference_group$Age_2, na.rm = TRUE), 1), " (", round(sd(reference_group$Age_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(reference_group$BMI_2, na.rm = TRUE), 1), " (", round(sd(reference_group$BMI_2, na.rm = TRUE), 1), ")"),
    paste0(round(mean(reference_group$RDS_2, na.rm = TRUE), 1), " (", round(sd(reference_group$RDS_2, na.rm = TRUE), 1), ")"),
    "",  # Empty row for title
    paste0(round(mean(reference_group$VTA_ICVF, na.rm = TRUE), 3), " (", round(sd(reference_group$VTA_ICVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(reference_group$VTA_OD, na.rm = TRUE), 3), " (", round(sd(reference_group$VTA_OD, na.rm = TRUE), 3), ")"),
    paste0(round(mean(reference_group$VTA_ISOVF, na.rm = TRUE), 3), " (", round(sd(reference_group$VTA_ISOVF, na.rm = TRUE), 3), ")"),
    paste0(round(mean(reference_group$VTA_FW, na.rm = TRUE), 3), " (", round(sd(reference_group$VTA_FW, na.rm = TRUE), 3), ")"),
    paste0(round(mean(reference_group$VTA_QSM_normalized, na.rm = TRUE), 3), " (", round(sd(reference_group$VTA_QSM_normalized, na.rm = TRUE), 3), ")"),
    paste0(scientific_format(mean(reference_group$VTA_Volume_Normalized, na.rm = TRUE), digits = 3),
           " (", scientific_format(sd(reference_group$VTA_Volume_Normalized, na.rm = TRUE), digits = 3), ")")

  )
  )

colnames(summary_table2) <- c(
  "Variable<br>&#8203",  # Add an empty line
  "Total Subsample<br>&#8203",  # Add an empty line
  "Best matched<br>(RDS&lt;13)",  # Multi-line header
  "Reference<br>(RDS &ge; 13)"    # Multi-line header
)

# Render the table with kable
kable(summary_table2, format = "html", row.names = FALSE, escape = FALSE) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  row_spec(6, bold = TRUE, italic = TRUE, background = "#f0f0f0")  # Customize the "MRI Derived Metrics" row

#### Save as Word 
library(officer)

# Assuming summary_table is your data.frame
# Create a Word document
doc <- read_docx()

# Add a title
doc <- body_add_par(doc, "Summary Table", style = "heading 1")

# Add the table without specifying a style (use default style)
doc <- body_add_table(doc, value = summary_table2, style = "Normal")

# Save the Word document
print(doc, target = "~/Desktop/dataVTA/Table_2.docx")

# ################################################### Supplementary Table 1
# Load necessary libraries
library(dplyr)
library(kableExtra)

# Ensure p-values are formatted with scientific notation where needed
coef_df_standardized <- coef_df_standardized %>%
  mutate(p_value = ifelse(p_value < 0.001, formatC(p_value, format = "e", digits = 2), round(p_value, 4))) %>%
  mutate(p_value = ifelse(as.numeric(gsub("\\*","",p_value)) < 0.05, paste0("**", p_value, "**"), p_value)) # Bold p-values < 0.05

# Create a publication-ready table
kable(coef_df_standardized, format = "html", digits = 3,
      col.names = c("Predictor", "Beta Coeff.", "Std. Error", "t-Statistic", "p-Value", "95% CI (Lower)", "95% CI (Upper)")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                full_width = FALSE, font_size = 14) %>%
  column_spec(2:7, width = "10em") %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general = "Standardized beta coefficients predicting depression severity (RDS_2). Significant predictors (p < 0.05) are highlighted.")
##################################################

#Reviewer requested analyses

###########################################################
# 0. Load Required Libraries
###########################################################
library(ggplot2)
library(broom)
library(dplyr)
library(ggExtra)

###########################################################
# 1. Data Preparation: Add Depression Variables
###########################################################
# Replace NAs with 0 and log-transform chronicity
best_match$Nb_Depression_Episodes_2[is.na(best_match$Nb_Depression_Episodes_2)] <- 0
best_match$Longest_period_of_depression_2[is.na(best_match$Longest_period_of_depression_2)] <- 0

# Cap values at 100
best_match$Nb_Depression_Episodes_2 <- pmin(best_match$Nb_Depression_Episodes_2, 100)
best_match$Longest_period_of_depression_2 <- pmin(best_match$Longest_period_of_depression_2, 100)

best_match$log_Nb_Depression_Episodes <- log1p(best_match$Nb_Depression_Episodes_2)
best_match$log_Longest_Depression <- log1p(best_match$Longest_period_of_depression_2)

# Create regression dataset
LinearRegressionMDD_extended_df <- best_match[, c(
  "Sex", "Age_2", "AgeSquared", "BMI_2", "RDS_2",
  "VTA_ICVF", "VTA_ISOVF", "VTA_OD", 
  "VTA_QSM_normalized",
  "log_Nb_Depression_Episodes", "log_Longest_Depression"
)]
# Create regression dataset
LinearRegressionMDD_extended_df_Volume <- best_match[, c(
  "Sex", "Age_2", "AgeSquared", "BMI_2", "RDS_2",
  "VTA_ICVF", "VTA_ISOVF", "VTA_OD", "VTA_Volume_Normalized",
  "VTA_QSM_normalized",
  "log_Nb_Depression_Episodes", "log_Longest_Depression"
)]
LinearRegressionMDD_extended_df$Sex <- as.numeric(LinearRegressionMDD_extended_df$Sex)
LinearRegressionMDD_extended_df_Volume$Sex <- as.numeric(LinearRegressionMDD_extended_df_Volume$Sex)

###########################################################
# 2. Linear Model: Full Regression
###########################################################
model_extended <- lm(RDS_2 ~ ., data = LinearRegressionMDD_extended_df)
summary(model_extended)

model_extended_volume <- lm(RDS_2 ~ ., data = LinearRegressionMDD_extended_df_Volume)
summary(model_extended_volume)
###########################################################
# 3. Predicted vs Observed Plot (Extended Model)
###########################################################
# Get predictions
LinearRegressionMDD_extended_df$Predicted_RDS_2 <- predict(model_extended, newdata = LinearRegressionMDD_extended_df)

# Compute correlation and SDs
X <- LinearRegressionMDD_extended_df$Predicted_RDS_2
Y <- LinearRegressionMDD_extended_df$RDS_2
r_value <- cor(X, Y)
sdX <- sd(X)
sdY <- sd(Y)

# Create annotation
annotation_text <- paste0(
  "r = ", round(r_value, 3), "\n",
  "σ(Predicted) = ", round(sdX, 3), "\n",
  "σ(Observed) = ", round(sdY, 3)
)

# Base plot
p_updated <- ggplot(LinearRegressionMDD_extended_df, 
            aes(x = Predicted_RDS_2, y = RDS_2)) +
  geom_point(color = "#6b95c4", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "black") +
  coord_cartesian(xlim = c(4, 16), ylim = c(4, 16)) +
  labs(
    title = "     Predicted vs. Observed Depression Severity    ",
    x = "Predicted RDS",
    y = "Observed RDS"
  ) +
  theme_minimal(base_size = 20) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),
    axis.title = element_text(size = 16),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    plot.margin = margin(t = 40, r = 10, b = 10, l = 10)
  ) +
  annotate(
    "text", x = 3.5, y = 16.25, label = annotation_text,
    hjust = 0, vjust = 1, size = 4.75, color = "black"
  )

# Add marginal distributions
p_with_marginals_updated <- ggMarginal(
  p_updated, type = "density", fill = "#6b95c4", alpha = 1
)
print(p_with_marginals_updated)

###########################################################
# 4. Standardized Beta Coefficient Plot
###########################################################

# Standardize predictors (excluding RDS_2)
predictors <- dplyr::select(LinearRegressionMDD_extended_df, -RDS_2)
predictors <- dplyr::select(LinearRegressionMDD_extended_df, -Predicted_RDS_2)
predictors_standardized <- as.data.frame(scale(predictors))

# Recombine with outcome
LinearRegressionMDD_standardized <- cbind(
  RDS_2 = LinearRegressionMDD_extended_df$RDS_2,
  predictors_standardized
)

# Fit standardized model
model_standardized <- lm(RDS_2 ~ ., data = LinearRegressionMDD_standardized)
summary(model_standardized)

# Step 1: After extracting and sorting coefficients
coef_df_standardized_updated <- broom::tidy(model_standardized, conf.int = TRUE) %>%
  rename(Term = term, Beta = estimate, CI_low = conf.low, CI_high = conf.high, p_value = p.value) %>%
  filter(Term != "(Intercept)") %>%
  arrange(desc(Beta))  # or use arrange(abs(Beta), .by_group = TRUE) for abs sorting

# Step 2: Assign correct factor levels for plotting
# First, ensure the actual variable names match the order in `coef_df_standardized$Term`
# This vector MUST match the actual variable names from the model:
coef_df_standardized_updated$Term <- factor(
  coef_df_standardized_updated$Term,
  levels = c(
    "log_Nb_Depression_Episodes",
    "log_Longest_Depression",
    "VTA_OD",
    "VTA_ICVF",
    "BMI_2",
    "VTA_ISOVF",
    "Sex",
    "Age_2",
    "AgeSquared",
    "VTA_QSM_normalized"
  ),
  labels = c(
    "Number of Depressive 
    Episodes (Lifetime)",
    "Longest Depressive 
    Episode (Weeks)",
    "ODI",
    "ICVF",
    "BMI",
    "ISOVF",
    "Sex",
    "Age",
    "Age²",
    "Normalized QSM"
  )
)

# Plot
ggplot(coef_df_standardized_updated, aes(x = reorder(Term, Beta), y = Beta, fill = Beta > 0)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_errorbar(aes(ymin = CI_low, ymax = CI_high), width = 0.2, color = "black", linewidth = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.8) +
  labs(
    title = "Standardized Beta Coefficients\nPredicting Depression Severity",
    subtitle = "Predictors with \n 95% Confidence Intervals",
    x = "Predictors",
    y = "Standardized Beta Coefficients"
  ) +
  scale_fill_manual(values = c("#808080", "#377EB8")) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 1),
    plot.subtitle = element_text(size = 14, hjust = 0.7),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 15, face = "bold"),
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 12),
    plot.margin = margin(t = 20, r = 10, b = 10, l = 10),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank()
  ) +
  coord_flip()

####Print extracted standardized coefficients
print(coef_df_standardized_updated)
#####################################
## MDD hisotry + current symptom severity
library(ggplot2)

# Step 1: Make sure ICD_Depression is a factor with readable labels
best_match$ICD_Depression <- factor(
  best_match$ICD_Depression,
  levels = c(0, 1),
  labels = c("Healthy Controls", "MDD History")
)

# Step 2: Create side-by-side histogram
ggplot(best_match, aes(x = RDS_2, fill = ICD_Depression)) +
  geom_histogram(position = "dodge", bins = 20, color = "white", alpha = 0.8) +
  scale_fill_manual(values = c("Healthy Controls" = "seagreen3", "MDD History" = "steelblue")) +
  labs(
    title = "Distribution of RDS_2 Scores by Depression History",
    x = "RDS_2 Score",
    y = "Number of Participants",
    fill = "Group"
  ) +
  theme_minimal()

###"Severity of Depression"
#Prepare the variables (replace NA with 0)
best_match$Nb_Depression_Episodes_2[is.na(best_match$Nb_Depression_Episodes_2)] <- 0
best_match$Longest_period_of_depression_2[is.na(best_match$Longest_period_of_depression_2)] <- 0

#Step 2: Create frequency tables
episodes_table <- as.data.frame(table(best_match$Nb_Depression_Episodes_2))
colnames(episodes_table) <- c("Number of Episodes", "Frequency")

duration_table <- as.data.frame(table(best_match$Longest_period_of_depression_2))
colnames(duration_table) <- c("Longest Period (weeks)", "Frequency")

# Format tables for publishing (e.g., gt or knitr::kable)

library(knitr)

kable(episodes_table, caption = "Frequency of Depression Episodes")
kable(duration_table, caption = "Frequency of Longest Depressive Period (Months)")

library(ggplot2)

# Replace NA with 0 as before
best_match$Nb_Depression_Episodes_2[is.na(best_match$Nb_Depression_Episodes_2)] <- 0
best_match$Longest_period_of_depression_2[is.na(best_match$Longest_period_of_depression_2)] <- 0

# Create frequency tables
episodes_freq <- as.data.frame(table(best_match$Nb_Depression_Episodes_2))
colnames(episodes_freq) <- c("Episodes", "Frequency")
episodes_freq$Episodes <- as.numeric(as.character(episodes_freq$Episodes))


duration_freq <- as.data.frame(table(best_match$Longest_period_of_depression_2))
colnames(duration_freq) <- c("DurationMonths", "Frequency")
duration_freq$DurationMonths <- as.numeric(as.character(duration_freq$DurationMonths))

####################################
######################## Model with Volume for reviewer - computing the CI and such
#####################################
library(dplyr)
library(broom)

# Select predictors and standardize them (excluding RDS_2)
predictors_volume <- LinearRegressionMDD_extended_df_Volume %>% select(-RDS_2)
predictors_volume_scaled <- as.data.frame(scale(predictors_volume))

# Recombine with the outcome variable
LinearRegressionMDD_standardized_volume <- cbind(
  RDS_2 = LinearRegressionMDD_extended_df_Volume$RDS_2,
  predictors_volume_scaled
)

# Fit standardized model
model_standardized_volume <- lm(RDS_2 ~ ., data = LinearRegressionMDD_standardized_volume)

coef_df_standardized_volume <- broom::tidy(model_standardized_volume, conf.int = TRUE) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    `β (Standardized Coefficient)` = round(estimate, 3),
    `95% CI` = paste0("[", round(conf.low, 3), ", ", round(conf.high, 3), "]"),
    `p-value` = case_when(
      p.value < 0.001 ~ "<0.001***",
      p.value < 0.01  ~ paste0(round(p.value, 3), "**"),
      p.value < 0.05  ~ paste0(round(p.value, 3), "*"),
      TRUE            ~ as.character(round(p.value, 3))
    )
  ) %>%
  select(Predictor = term, 
         `β (Standardized Coefficient)`, 
         `95% CI`, 
         `p-value`) %>%
  arrange(desc(`β (Standardized Coefficient)`))

### Recode and clean up the variables
coef_df_standardized_volume$Predictor <- recode(coef_df_standardized_volume$Predictor,
                                                "log_Nb_Depression_Episodes" = "Number of Depressive Episodes (Lifetime)",
                                                "log_Longest_Depression" = "Longest Depressive Episode (Weeks)",
                                                "VTA_OD" = "ODI",
                                                "VTA_ICVF" = "ICVF",
                                                "VTA_ISOVF" = "ISOVF",
                                                "VTA_QSM_normalized" = "Normalized QSM",
                                                "VTA_Volume_Normalized" = "VTA Volume Ratio",
                                                "BMI_2" = "BMI",
                                                "Age_2" = "Age",
                                                "AgeSquared" = "Age²",
                                                "Sex" = "Sex"
)


### make into a table 
library(knitr)
library(dplyr) 

kable(coef_df_standardized_volume, digits = 3,
      caption = "Table S1. Standardized Beta Coefficients from Original Linear Regression Model Including VTA Volume")

#

################
#########
#################

library(officer)
library(flextable)

# Convert to flextable (prettier than base Word table)
ft <- flextable(coef_df_standardized_volume)
ft <- set_caption(ft, caption = "Table S1. Standardized Beta Coefficients from Original Linear Regression Model Including VTA Volume")
ft <- autofit(ft)

# Create Word doc and add the table
doc <- read_docx()
doc <- body_add_par(doc, "Table S1", style = "heading 1")
doc <- body_add_par(doc, "Standardized Beta Coefficients from Original Linear Regression Model Including VTA Volume", style = "Normal")
doc <- body_add_flextable(doc, ft)

# Save
print(doc, target = "~/Desktop/dataVTA/Standardized_Betas_TableS1.docx")

##################
#########Exporting it as an image
#################
##################Adding linear regression stats
# Get model summary
model_summary <- summary(model_standardized_volume)

# Extract values
r_squared      <- round(model_summary$r.squared, 3)
adj_r_squared  <- round(model_summary$adj.r.squared, 3)
f_statistic    <- round(model_summary$fstatistic[1], 2)
df1            <- model_summary$fstatistic[2]  # numerator df
df2            <- model_summary$fstatistic[3]  # denominator df

# reporting these values in figure 
note_line <- paste0("Note: Model R² = ", r_squared,
", Adjusted R² = ", adj_r_squared,
", F(", df1, ", ", df2, ") = ", f_statistic, ", p < 0.001.")


library(flextable)
library(webshot2)

# Define the alternating background color
zebra_grey <- "#F7F7F7"

# Build formatted flextable
ft <- flextable(coef_df_standardized_volume) %>%
  autofit() %>%
  fontsize(size = 11, part = "all") %>%
  
  # Clean and minimal theme
  theme_booktabs() %>%
  
  # Align columns
  align(j = "Predictor", align = "left", part = "all") %>%
  align(j = "β (Standardized Coefficient)", align = "center", part = "all") %>%
  align(j = "95% CI", align = "center", part = "all") %>%
  align(j = "p-value", align = "left", part = "all") %>%
  
  # Italicize β and p in headers only
  compose(part = "header", j = "β (Standardized Coefficient)",
          value = as_paragraph(as_i("β"), " (Standardized Coefficient)")) %>%
  compose(part = "header", j = "p-value",
          value = as_paragraph(as_i("p"), "-value")) %>%
  
  # Apply alternating grey background to even-numbered rows
  bg(i = seq(2, nrow(coef_df_standardized_volume), by = 2),
     bg = zebra_grey, part = "body")
  
# Show it
ft

##### adding the captions and so on 
ft <- ft %>%
  add_footer_lines(values = c(
    "Table S4. Standardized beta coefficients from original linear regression model including VTA volume. VTA volume was later removed from the final model due to concerns about atlas-based volume estimation (see main text for revised model).The outcome variable was recent depressive symptom severity (RDS) and significant predictors are indicated with asterisks: ***p < .001, **p < .01, *p < .05.",
    "",
    note_line  )) %>%
  fontsize(size = 10, part = "footer") %>%
  align(align = "left", part = "footer")

ft
# Save as image
save_as_image(ft, path = "~/Desktop/dataVTA/Standardized_Betas_Table_Volume_screenshot.png", zoom = 2)
