
# -----------------------------------
# Script Purpose and Overview
# -----------------------------------
# This script performs data visualization and descriptive statistics for the extra analyses asked by the reviewers 
# for the revise and resubmit.
#
# Main Objectives:
# 1. Update the model (no volume + more variables) + compute new figures + data cleaning
# 2. Descriptives for the additional ROIs
#
# Author : Sarah Khalife
# Version: 1
# Date   : Jul 2025
# -----------------------------------

# -----------------------------------
# Load Required Libraries
# -----------------------------------
library(dplyr)
library(readr)
library(tidyr)
library(officer)
library(flextable)

# Define file paths
base_path <- "~/Desktop/dataVTA"

# -----------------------------------
# Helper Function to Load and Clean Metrics
# -----------------------------------
# This function loads a whitespace-separated data table from file_path,
# replaces "NaN" strings with actual NA values,
# converts all columns (except the first, assumed to be Participant_ID) to numeric,
# and appends the metric_name as a prefix to each ROI column.
load_and_clean_metric <- function(file_path, metric_name) {
  # Read table using flexible whitespace separator
  df_raw <- read.table(file_path, header = TRUE, sep = "", fill = TRUE)
  
  # Replace "NaN" with NA
  df_raw[df_raw == "NaN"] <- NA
  
  # Rename the first column to Participant_ID
  colnames(df_raw)[1] <- "Participant_ID"
  
  # Convert all non-ID columns to numeric
  df_clean <- df_raw %>%
    mutate(across(-Participant_ID, as.numeric))
  
  # Rename ROI columns to include metric prefix
  colnames(df_clean)[-1] <- paste0(metric_name, "_", colnames(df_clean)[-1])
  
  return(df_clean)
}

# -----------------------------------
# Load Supplementary Metric Files
# -----------------------------------
# Each call loads a specific MRI-derived metric from a cleaned text file
# and prepares it for merging based on Participant_ID.
qsm_df   <- load_and_clean_metric("~/Desktop/dataVTA/Final_QSM_Combined_Cleaned.txt", "QSM")
icvf_df  <- load_and_clean_metric("~/Desktop/dataVTA/Final_ICVF_Combined_Cleaned.txt", "ICVF")
isovf_df <- load_and_clean_metric("~/Desktop/dataVTA/Final_ISOVF_Combined_Cleaned.txt", "ISOVF")
od_df    <- load_and_clean_metric("~/Desktop/dataVTA/Final_OD_Combined_Cleaned.txt", "OD")
fw_df    <- load_and_clean_metric("~/Desktop/dataVTA/Final_FW_Combined_Cleaned.txt", "FW")

# -----------------------------------
# Remove Duplicate Rows (Keep Most Complete per Subject)
# -----------------------------------
# Sort by number of missing values, keeping the row with the fewest NAs per Participant_ID.
# AND Apply duplicate cleaning for each metric

## For QSM
qsm_df_clean <- qsm_df[order(qsm_df$Participant_ID, rowSums(is.na(qsm_df))), ]
qsm_df_clean <- qsm_df_clean[!duplicated(qsm_df_clean$Participant_ID), ]

# For ICVF
icvf_df_clean <- icvf_df[order(icvf_df$Participant_ID, rowSums(is.na(icvf_df))), ]
icvf_df_clean <- icvf_df_clean[!duplicated(icvf_df_clean$Participant_ID), ]

# For ICVF
isovf_df_clean <- isovf_df[order(isovf_df$Participant_ID, rowSums(is.na(isovf_df))), ]
isovf_df_clean <- isovf_df_clean[!duplicated(isovf_df_clean$Participant_ID), ]

## For QSM
od_df_clean <- od_df[order(od_df$Participant_ID, rowSums(is.na(od_df))), ]
od_df_clean <- od_df_clean[!duplicated(od_df_clean$Participant_ID), ]

# For ICVF
fw_df_clean <- fw_df[order(fw_df$Participant_ID, rowSums(is.na(fw_df))), ]
fw_df_clean <- fw_df_clean[!duplicated(fw_df_clean$Participant_ID), ]

# -----------------------------------
# Merge All Metrics by Participant_ID
# -----------------------------------
# Merge all cleaned metric dataframes using Participant_ID as the key.
supplementary_metrics_with_NA <- Reduce(function(x, y) merge(x, y, by = "Participant_ID", all = FALSE),
                                        list(qsm_df_clean, icvf_df_clean, isovf_df_clean, od_df_clean, fw_df_clean))
# -----------------------------------
# Merge Supplementary Metrics with Main Dataset
# -----------------------------------
# Merge matched_data with the supplementary metrics. Assumes matched_data is already loaded in the environment.
matched_supp_with_NA <- merge(matched_data, supplementary_metrics_with_NA, by = "Participant_ID", all.x = FALSE)

# -----------------------------------
# Save Combined Dataset to CSV
# -----------------------------------
# Output file contains matched clinical + imaging features for later statistical analysis.
write.csv(matched_supp_with_NA, "matched_data_with_supplementary_ROIs.csv", row.names = FALSE)

# -----------------------------------
# Data Cleaning and Preprocessing
# -----------------------------------

# Identify columns for each metric type using naming patterns
qsm_cols   <- grep("^QSM_", names(matched_supp_with_NA), value = TRUE)  # QSM columns
icvf_cols  <- grep("ICVF", names(matched_supp_with_NA), value = TRUE)   # ICVF columns
isovf_cols <- grep("ISOVF", names(matched_supp_with_NA), value = TRUE)  # ISOVF columns
od_cols    <- grep("OD", names(matched_supp_with_NA), value = TRUE)     # OD columns
fw_cols    <- grep("^FW", names(matched_supp_with_NA), value = TRUE)    # FW columns

# Combine all relevant columns into one vector for later filtering or NA handling
all_supp_cols <- c(qsm_cols, icvf_cols, isovf_cols, od_cols, fw_cols)

#matched_supp<- matched_supp_with_NA[complete.cases(matched_supp_with_NA[, all_supp_cols]), ]

# View distribution of ICD_Depression groups (HC vs With Depression History)
prop.table(table(matched_supp_with_NA$ICD_Depression))
# HC With Depression history 
# 0.493807                0.506193

table(matched_supp_with_NA$ICD_Depression)
# HC With Depression history 
#. 3030                    3106 

# Save the number of subjects before cleaning
NbofSubjB4Processing <- nrow(matched_supp_with_NA)

NbofSubjB4Processing
#########
# -----------------------------------
# Step 1: Rename Metric Columns to ROI_METRIC Format
# -----------------------------------
metrics <- c("QSM", "ICVF", "ISOVF", "OD", "FW")
current_names <- colnames(matched_supp_with_NA)

# Helper function to reformat metric columns: e.g., QSM_VTA → VTA_QSM
rename_metric_roi <- function(name) {
  for (m in metrics) {
    if (startsWith(name, paste0(m, "_"))) {
      roi <- sub(paste0(m, "_"), "", name)
      roi <- ifelse(roi == "NA.", "NA", roi)  # Fix NA.
      return(paste0(roi, "_", m))
    }
  }
  return(name) # Leave other names unchanged
}

new_names <- sapply(current_names, rename_metric_roi)
colnames(matched_supp_with_NA) <- new_names

# -----------------------------------
# Step 2: Normalize QSM Columns (Min-Max Scaling)
# -----------------------------------

# Identify QSM columns (including VTA_QSM, OFC_QSM, etc.)
qsm_columns <- grep("_QSM$", colnames(matched_supp_with_NA), value = TRUE)

# Loop over each QSM column and apply min-max normalization
for (col in qsm_columns) {
  min_val <- min(matched_supp_with_NA[[col]], na.rm = TRUE)
  max_val <- max(matched_supp_with_NA[[col]], na.rm = TRUE)
  
  # Normalize
  normalized <- scale(matched_supp_with_NA[[col]], center = min_val, scale = max_val - min_val)
  
  # Create new column: original name + _normalized
  matched_supp_with_NA[[paste0(col, "_normalized")]] <- normalized[, 1]
}

# -----------------------------
# Create positive/negative label for each QSM column (if meaningful)
# -----------------------------
# Example: just for VTA_QSM (as you had before)
matched_supp_with_NA$label_column <- ifelse(matched_supp_with_NA$VTA_QSM >= 0, "positive", "negative")
matched_supp_with_NA$label_column <- as.factor(matched_supp_with_NA$label_column)

# -----------------------------
# Summary by label and RDS_2 for VTA_QSM
# -----------------------------
QSM_summary_byGroup <- matched_supp_with_NA %>%
  group_by(label_column, RDS_2) %>%
  summarize(
    mean_value = mean(VTA_QSM, na.rm = TRUE),
    sd_value = sd(VTA_QSM, na.rm = TRUE),
    .groups = "drop"
  )

# -----------------------------
# Remove rows with NA in any QSM column (before or after normalization)
# -----------------------------
matched_supp_with_NA <- matched_supp_with_NA[complete.cases(matched_supp_with_NA[, qsm_columns]), ]

# -----------------------------
# Step 3: Remove Biologically Implausible Values for FW, ICVF, ISOVF, OD (Should be in [0, 1])
# -----------------------------
for (col in columns_to_check) {
  if (grepl("FW|ICVF|ISOVF|OD", col)) {
    matched_supp_with_NA <- matched_supp_with_NA[matched_supp_with_NA[[col]] >= 0 & matched_supp_with_NA[[col]] <= 1, ]
  }
}

NbofSubjB4OutlierRemoval <- nrow(matched_supp_with_NA)
ImplausibleNbsRemoved <- NbofSubjB4OutlierRemoval - NbofSubjB4Processing
cat("Number of subjects removed due to implausible values:", ImplausibleNbsRemoved, "\n")

# -----------------------------------
# Step 4: Outlier Detection (Per ROI-Metric, >3 SD)
# -----------------------------------
ROIs <- c("OFC", "INS", "AMY", "HIP", "vACC", "mPFC", "PCC", "PCUN", "ANG",
          "DLPFC", "dACC", "PFC", "CAU", "NA")

columns_to_check <- unlist(lapply(ROIs, function(roi) {
  paste0(roi, "_", metrics)
}))
columns_to_check <- columns_to_check[columns_to_check %in% colnames(matched_supp_with_NA)]

#matched_supp_with_NA <- matched_supp_with_NA[complete.cases(matched_supp_with_NA[, columns_to_check]), ]

# For each column, set values > 3 SD from the mean to NA (but keep the subject)
for (col in columns_to_check) {
  m <- mean(matched_supp_with_NA[[col]], na.rm = TRUE)
  s <- sd(matched_supp_with_NA[[col]], na.rm = TRUE)
  
  # Identify outliers
  is_outlier <- abs(matched_supp_with_NA[[col]] - m) > 3 * s
  
  # Set only the outlier values to NA
  matched_supp_with_NA[[col]][is_outlier] <- NA
}

outliers_per_column <- sapply(columns_to_check, function(col) {
  m <- mean(matched_supp_with_NA[[col]], na.rm = TRUE)
  s <- sd(matched_supp_with_NA[[col]], na.rm = TRUE)
  sum(abs(matched_supp_with_NA[[col]] - m) > 3 * s, na.rm = TRUE)
})

print(outliers_per_column)

total_outliers <- sum(outliers_per_column)
cat("Total number of ROI outlier values set to NA:", total_outliers, "\n")

# -----------------------------
# Step 5: Count ROI outliers per ICD_Depression group
# -----------------------------
groups <- unique(matched_supp_with_NA$ICD_Depression)

outlier_table <- data.frame(
  ROI = character(),
  Metric = character(),
  Group = character(),
  Outliers_Removed = integer(),
  stringsAsFactors = FALSE
)

for (col in columns_to_check) {
  m <- mean(matched_supp_with_NA[[col]], na.rm = TRUE)
  s <- sd(matched_supp_with_NA[[col]], na.rm = TRUE)
  
  for (g in groups) {
    group_data <- matched_supp_with_NA[matched_supp_with_NA$ICD_Depression == g, ]
    
    group_outliers <- abs(group_data[[col]] - m) > 3 * s
    n_outliers <- sum(group_outliers, na.rm = TRUE)
    
    outlier_table <- rbind(
      outlier_table,
      data.frame(
        ROI = sub("_[^_]+$", "", col),
        Metric = sub(".*_", "", col),
        Group = g,
        Outliers_Removed = n_outliers
      )
    )
  }
}

# Create pivot table of outliers by group
outlier_summary <- outlier_table %>%
  pivot_wider(names_from = Group, values_from = Outliers_Removed, values_fill = 0)

print(outlier_summary)

# Drop "NA" column if present in the outlier_summary
outlier_summary_clean <- outlier_summary[, colnames(outlier_summary) != "NA"]

# -----------------------------
# Step 6: Final report
# -----------------------------
OutliersRemoved <- NbofSubjB4OutlierRemoval - nrow(matched_supp_with_NA)
cat("Total number of outliers removed:", OutliersRemoved, "\n")

# -----------------------------
# Step 7: Count Complete ROI Entries Post-Cleaning
# -----------------------------
# This section counts how many participants have complete (non-NA) data for all metrics within each ROI.
# Initialize a named vector with zeros to store counts per ROI
complete_counts <- setNames(numeric(length(ROIs)), ROIs)

# Loop through each ROI
for (roi in ROIs) {
  roi_cols <- paste0(roi, "_", metrics)
  roi_cols <- roi_cols[roi_cols %in% colnames(matched_supp_with_NA)]  # Make sure columns exist
  
  # Count rows where all ROI-specific metrics are not NA
  complete_counts[roi] <- sum(complete.cases(matched_supp_with_NA[, roi_cols]))
}

# View the counts
print(complete_counts)

# -----------------------------
# Step 8: Descriptive Table of Demographics and ROI Metrics
# -----------------------------
# Create subsets for each group
matched_HC_supp <- matched_supp_with_NA %>% filter(ICD_Depression == "HC")
matched_MDD_supp <- matched_supp_with_NA %>% filter(ICD_Depression == "With Depression history")

# Helper functions
calculate_p_value <- function(variable) {
  hc_values <- matched_HC_supp[[variable]]
  mdd_values <- matched_MDD_supp[[variable]]
  if (is.numeric(hc_values) && is.numeric(mdd_values)) {
    test <- try(t.test(hc_values, mdd_values)$p.value, silent = TRUE)
    if (inherits(test, "try-error")) {
      test <- wilcox.test(hc_values, mdd_values)$p.value
    }
  } else {
    test <- NA
  }
  return(test)
}

# Significance formatting functions
significance_code <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return("***")
  else if (p < 0.01) return("**")
  else if (p < 0.05) return("*")
  else if (p < 0.1) return(".")
  else return(" ")
}

format_p_value <- function(p) {
  if (is.na(p)) return("")
  else if (p < 0.001) return(paste0("p<0.001", significance_code(p)))
  else return(paste0(formatC(p, format = "f", digits = 3), significance_code(p)))
}

# Create summary table structure
summary_table <- data.frame(
  Variable = character(),
  HC = character(),
  Depression_History = character(),
  `p-value` = character(),
  stringsAsFactors = FALSE
)

# === 1. Demographics and clinical vars ===
summary_table <- rbind(
  summary_table,
  data.frame(
    Variable = "N",
    HC = formatC(nrow(matched_HC_supp), format = "f", big.mark = ",", digits = 0),
    Depression_History = formatC(nrow(matched_MDD_supp), format = "f", big.mark = ",", digits = 0),
    `p-value` = ""
  ),
  data.frame(
    Variable = "Female n (%)",
    HC = paste0(sum(matched_HC_supp$Sex == "Female"), " (", round(mean(matched_HC_supp$Sex == "Female") * 100, 1), "%)"),
    Depression_History = paste0(sum(matched_MDD_supp$Sex == "Female"), " (", round(mean(matched_MDD_supp$Sex == "Female") * 100, 1), "%)"),
    `p-value` = ""
  ),
  data.frame(
    Variable = "Age (Years)",
    HC = sprintf("%.1f (%.1f)", mean(matched_HC_supp$Age_2, na.rm = TRUE), sd(matched_HC_supp$Age_2, na.rm = TRUE)),
    Depression_History = sprintf("%.1f (%.1f)", mean(matched_MDD_supp$Age_2, na.rm = TRUE), sd(matched_MDD_supp$Age_2, na.rm = TRUE)),
    `p-value` = format_p_value(calculate_p_value("Age_2"))
  ),
  data.frame(
    Variable = "BMI (kg/m²)",
    HC = sprintf("%.1f (%.1f)", mean(matched_HC_supp$BMI_2, na.rm = TRUE), sd(matched_HC_supp$BMI_2, na.rm = TRUE)),
    Depression_History = sprintf("%.1f (%.1f)", mean(matched_MDD_supp$BMI_2, na.rm = TRUE), sd(matched_MDD_supp$BMI_2, na.rm = TRUE)),
    `p-value` = format_p_value(calculate_p_value("BMI_2"))
  ),
  data.frame(
    Variable = "RDS",
    HC = sprintf("%.1f (%.1f)", mean(matched_HC_supp$RDS_2, na.rm = TRUE), sd(matched_HC_supp$RDS_2, na.rm = TRUE)),
    Depression_History = sprintf("%.1f (%.1f)", mean(matched_MDD_supp$RDS_2, na.rm = TRUE), sd(matched_MDD_supp$RDS_2, na.rm = TRUE)),
    `p-value` = format_p_value(calculate_p_value("RDS_2"))
  ),
  data.frame(
    Variable = "MRI Derived Metrics", HC = "", Depression_History = "", `p-value` = ""
  )
)

# === 2. Add MRI metrics per ROI ===
for (roi in ROIs) {
  for (metric in metrics) {
    var_name <- paste0(roi, "_", metric)
    if (!var_name %in% names(matched_supp_with_NA)) next
    
    mean_hc <- mean(matched_HC_supp[[var_name]], na.rm = TRUE)
    sd_hc <- sd(matched_HC_supp[[var_name]], na.rm = TRUE)
    
    mean_mdd <- mean(matched_MDD_supp[[var_name]], na.rm = TRUE)
    sd_mdd <- sd(matched_MDD_supp[[var_name]], na.rm = TRUE)
    
    p <- calculate_p_value(var_name)
    
    summary_table <- rbind(summary_table, data.frame(
      Variable = var_name,
      HC = sprintf("%.3f (%.3f)", mean_hc, sd_hc),
      Depression_History = sprintf("%.3f (%.3f)", mean_mdd, sd_mdd),
      `p-value` = format_p_value(p),
      stringsAsFactors = FALSE
    ))
  }
}

# -----------------------------
# Step 9: ROI-Wise Completeness Table (for Word export)
# -----------------------------
# Count number of subjects with complete data per ROI in each group
complete_counts_HC <- setNames(numeric(length(ROIs)), ROIs)
complete_counts_MDD <- setNames(numeric(length(ROIs)), ROIs)

for (roi in ROIs) {
  roi_cols <- paste0(roi, "_", metrics)
  roi_cols <- roi_cols[roi_cols %in% names(matched_supp_with_NA)]
  
  complete_counts_HC[roi] <- sum(complete.cases(matched_HC_supp[, roi_cols]))
  complete_counts_MDD[roi] <- sum(complete.cases(matched_MDD_supp[, roi_cols]))
}

roi_completeness_table <- data.frame(
  ROI = ROIs,
  HC_Complete_Cases = formatC(as.integer(complete_counts_HC), big.mark = ",", format = "f", digits = 0),
  MDD_Complete_Cases = formatC(as.integer(complete_counts_MDD), big.mark = ",", format = "f", digits = 0),
  stringsAsFactors = FALSE
)

# -----------------------------
# Step 10: Save Output Tables to Word Document
# -----------------------------
doc <- read_docx()

# Add Table 4 to Word doc
doc <- body_add_break(doc)
doc <- body_add_par(doc, "Table 2: ROI-Metric Outlier Counts by ICD Group", style = "heading 1")
doc <- body_add_table(doc, value = outlier_summary_clean)

# Page break and ROI Completeness Table
doc <- body_add_break(doc)
doc <- body_add_par(doc, "ROI-Wise Count of Subjects with Complete Data (Post-Cleaning)", style = "heading 1")
doc <- body_add_table(doc, value = roi_completeness_table)

# Add Summary Table
doc <- body_add_par(doc, "Full Sample Characteristics and MRI-Derived Metrics", style = "heading 1")
doc <- body_add_table(doc, value = summary_table)

# Save the Word document
print(doc, target = "~/Desktop/dataVTA/Summary_Table_ROI_and_Clinical.docx")

###################
#formatted_p <- ifelse(chi_p_value < 0.001, "<0.001", sprintf("%.3f", chi_p_value))

# Add the p-value to the Female row
#summary_table$`p-value`[summary_table$Variable == "Female n (%)"] <- formatted_p