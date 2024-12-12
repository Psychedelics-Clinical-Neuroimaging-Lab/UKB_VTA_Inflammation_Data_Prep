# -----------------------------------
# Script Purpose and Overview
# -----------------------------------
# This script is designed to preprocess and analyze neuroimaging and clinical data
# from the UK Biobank to investigate the relationship between biomarkers in the 
# ventral tegmental area (VTA) and depression severity.
#
# Main Objectives:
# 1. Load and merge neuroimaging biomarker datasets - that were derived from the MRI images (e.g., free-water, isotropic volume fraction).
# 2. Integrate neuroimaging data with clinical variables from the UKB (e.g., depression scores, demographics).
# 3. Perform data cleaning and transformation:
#    - Handle missing values and invalid responses.
#    - Normalize VTA volume by intracranial volume (ICV) to account for brain size.
#    - Remove outliers to ensure data integrity.
# 4. Create composite depression and anxiety scores from questionnaire responses.
# 5. Label participants with and without a history of depression based on ICD codes.
# 6. Visualize key data distributions to validate preprocessing steps.
# 7. Prepare the cleaned and structured dataset for statistical analysis.
#
# Notes:
# - The script includes error-handling functions for debugging.
# - Ensure file paths and libraries are correctly configured before running the script.
#
# Author : Sarah Khalife
# Version: 1
# Date   : Dec 2024

# -----------------------------------
# Library Imports
# -----------------------------------
# Load necessary libraries for the script
library(tcltk)     # For error pop-up handling
library(dplyr)     # For data manipulation and transformation
library(ggplot2)   # For data visualization
library(scales)    # For normalization and scaling values
# -----------------------------------

# -----------------------------------
# Error Handling Function
# -----------------------------------
# Function to display an error message in a GUI pop-up window.
# Useful for debugging issues like unexpected values or logic errors.


# Function to create and display an error message pop-up
show_error <- function(message) {
# Create a tcl/tk window
  error_popup <- tktoplevel()
  
# Set the window title
  tkwm.title(error_popup, "Error")
  
  # Create a label widget to display the error message
  label <- tklabel(error_popup, text = message)
  
  # Add padding around the label
  tkpack(label, padx = 20, pady = 20)
  
  # Add a button to close the window
  close_button <- tkbutton(error_popup, text = "OK", command = function() tkdestroy(error_popup))
  tkpack(close_button, padx = 10, pady = 10)
}
# -----------------------------------

# -----------------------------------
# Loading and Merging Imaging Datasets
# -----------------------------------
# Load individual imaging biomarkers from text files into separate dataframes
VTA_FW <- read.table("~/Desktop/dataVTA/ROI_biomarker_data/VTA_FW_filtered.txt",
                     quote="\"", comment.char="",header=FALSE, 
                     col.names = c("Participant_ID", "VTA_FW"))
# (Repeat for other biomarkers: ICVF, ISOVF, QSM, OD, volume)
VTA_ICVF <- read.table("~/Desktop/dataVTA/ROI_biomarker_data/VTA_ICVF_filtered.txt", 
                       quote="\"", comment.char="",header=FALSE,
                       col.names = c("Participant_ID", "VTA_ICVF"))
VTA_ISOVF <- read.table("~/Desktop/dataVTA/ROI_biomarker_data/VTA_ISOVF_filtered.txt",
                        quote="\"", comment.char="",header=FALSE, 
                        col.names = c("Participant_ID", "VTA_ISOVF"))
VTA_QSM <- read.table("~/Desktop/dataVTA/ROI_biomarker_data/VTA_QSM_filtered.txt", 
                      quote="\"", comment.char="",header=FALSE, 
                      col.names = c("Participant_ID", "VTA_QSM"))
VTA_OD <- read.table("~/Desktop/dataVTA/ROI_biomarker_data/VTA_OD_filtered.txt",
                     quote="\"", comment.char="",header=FALSE,
                     col.names = c("Participant_ID", "VTA_OD"))
VTA_volume <- read.table("~/Desktop/dataVTA/ROI_biomarker_data/VTA_volume_filtered.txt", 
                         quote="\"", comment.char="",header=FALSE, 
                         col.names = c("Participant_ID", "VTA_volume","VTA_Voxel_Count"))

# Combine all biomarker datasets into a single dataframe
# This ensures we have a unified dataset for further processing
df_list <- list(VTA_FW, VTA_ICVF, VTA_ISOVF, VTA_QSM, VTA_OD, VTA_volume)      
dataset_variables<-Reduce(function(x, y) merge(x, y, all=TRUE), df_list)  

# At this point, `dataset_variables` contains all VTA imaging biomarkers for each participant.
# -----------------------------------

# -----------------------------------
# Loading and Preparing UK Biobank Variables
# -----------------------------------
# Load the dataset with additional UK Biobank variables
fieldsTable <- read.csv("~/Desktop/dataVTA/DepressionVariables2024.csv", header=TRUE)

# Subset columns of interest to focus only on relevant data for analysis
# Include required columns
fieldsVTA <- fieldsTable[, c("f.eid", "f.31.0.0","f.21003.2.0", "f.21001.2.0", "f.2050.2.0", "f.2060.2.0","f.2070.2.0","f.2080.2.0",
                             "f.26521.2.0","f.30710.1.0","f.30710.0.0","f.30000.0.0","f.30000.1.0","f.30000.2.0","f.53.0.0","f.53.1.0","f.53.2.0","f.53.3.0",
                             "f.20433.0.0","f.20434.0.0","f.29034.0.0","f.29036.0.0","f.130894.0.0","f.130896.0.0",
                             "f.2050.0.0","f.2060.0.0","f.2070.0.0","f.2080.0.0","f.2050.1.0","f.2060.1.0","f.2070.1.0","f.2080.1.0","f.2050.3.0","f.2060.3.0","f.2070.3.0","f.2080.3.0",
                             "f.20510.0.0","f.20507.0.0","f.20519.0.0","f.20514.0.0","f.20511.0.0","f.20513.0.0","f.20508.0.0","f.20533.0.0","f.20517.0.0","f.20400.0.0",
                             "f.29003.0.0","f.29007.0.0","f.29005.0.0","f.29002.0.0","f.29006.0.0","f.29010.0.0","f.29008.0.0","f.29023.0.0","f.29004.0.0","f.29198.0.0",
                             "f.28737.0.0","f.28738.0.0","f.28755.0.0",
                             "f.4620.2.0","f.4609.2.0","f.5386.2.0","f.5375.2.0","f.2090.2.0","f.2100.2.0",
                             "f.29062.0.0","f.20516.0.0","f.29200.0.0")]  

# Rename columns for better readability and to align with imaging dataset
# Example: "f.eid" -> "Participant_ID", "f.31.0.0" -> "Sex", etc.

fieldsVTA <- fieldsVTA %>%
  rename(
    Participant_ID = f.eid,
    Sex = f.31.0.0,
    Age_2 = f.21003.2.0,
    BMI_2 = f.21001.2.0,
    Depressed_2 = f.2050.2.0,Unenthusiasm_2 = f.2060.2.0,Restlessness_2 = f.2070.2.0,Lethargy_2 = f.2080.2.0,
    ICVUKB = f.26521.2.0,
    CRP_1 = f.30710.1.0,CRP_0 = f.30710.0.0,
    WBC_0 = f.30000.0.0,WBC_1 = f.30000.1.0,WBC_2 = f.30000.2.0,
    Date_Assess_Center_0 = f.53.0.0,Date_Assess_Center_1 = f.53.1.0,Date_Assess_Center_2 = f.53.2.0,Date_Assess_Center_3 = f.53.3.0,
    Age_first_depression_2016 = f.20433.0.0,Age_last_depression_2016 = f.20434.0.0,Age_first_depression_2022 = f.29034.0.0,Age_last_depression_2022 = f.29036.0.0,
    Date_F32 = f.130894.0.0,Date_F33 = f.130896.0.0,
    Depressed_0 = f.2050.0.0,Unenthusiasm_0 = f.2060.0.0,Restlessness_0 = f.2070.0.0,Lethargy_0 = f.2080.0.0,
    Depressed_1 = f.2050.1.0,Unenthusiasm_1 = f.2060.1.0,Restlessness_1 = f.2070.1.0,Lethargy_1 = f.2080.1.0,
    Depressed_3 = f.2050.3.0,Unenthusiasm_3 = f.2060.3.0,Restlessness_3 = f.2070.3.0,Lethargy_3 = f.2080.3.0,
    PHQ_2016_Recent_Depression = f.20510.0.0,PHQ_2016_Recent_Inadequacy = f.20507.0.0,PHQ_2016_Recent_Tiredness = f.20519.0.0,PHQ_2016_Recent_Lack_Interest = f.20514.0.0,PHQ_2016_Recent_Appetite_change = f.20511.0.0,PHQ_2016_Recent_Self_Harm = f.20513.0.0,PHQ_2016_Recent_Trouble_Concentrating = f.20508.0.0,PHQ_2016_Trouble_Falling_Asleep = f.20533.0.0,PHQ_2016_Any_Sleep_Issues = f.20517.0.0,
    Date_PHQ_2016 = f.20400.0.0,
    PHQ_2022_Recent_Depression = f.29003.0.0,PHQ_2022_Recent_Inadequacy = f.29007.0.0,PHQ_2022_Recent_Tiredness = f.29005.0.0,PHQ_2022_Recent_Lack_Interest = f.29002.0.0,PHQ_2022_Recent_Appetite_change = f.29006.0.0,PHQ_2022_Recent_Self_Harm = f.29010.0.0,PHQ_2022_Recent_Trouble_Concentrating = f.29008.0.0,PHQ_2022_Trouble_Falling_Asleep = f.29023.0.0,PHQ_2022_Any_Sleep_Issues = f.29004.0.0,
    Date_PHQ_2022 = f.29198.0.0,
    Anxiety_2016_Restlessness = f.20516.0.0,
    Anxiety_2022_Restlessness = f.29062.0.0,
    Date_Anxienty_2022 = f.29200.0.0,
    COVID_2022_Depression = f.28737.0.0,COVID_2022_Recent_Lack_Interest = f.28738.0.0,
    Date_COVID_Ques_2022 = f.28755.0.0,
    Nb_Depression_Episodes_2 = f.4620.2.0,Longest_period_of_depression_2 = f.4609.2.0,
    Nb_Disinterest_Episodes_2 = f.5386.2.0,Longest_period_of_Disinterest_2 = f.5375.2.0,
    Seen_GP_Nerves_Depression_2 = f.2090.2.0,Seen_Psychiatrist_Nerves_Depression_2 = f.2100.2.0
  )

# -----------------------------------

# -----------------------------------
# Filtering and Data Transformation
# -----------------------------------
# Retain only participants present in both imaging and UK Biobank datasets
IDsofInterest<-dataset_variables[,1]
fieldsVTA<-fieldsVTA[fieldsVTA$Participant_ID %in% IDsofInterest, ]

# Derive new variables such as AgeSquared and normalize date columns and reformat columns 

fieldsVTA$Sex <- factor(fieldsVTA$Sex,levels = c(0, 1), labels = c("Female", "Male"))
date_columns <- c("Date_Assess_Center_0", "Date_Assess_Center_1", "Date_Assess_Center_2", "Date_Assess_Center_3", "Date_F32", "Date_F33", 
                  "Date_PHQ_2016", "Date_PHQ_2022", "Date_COVID_Ques_2022","Date_Anxienty_2022")

fieldsVTA[date_columns] <- lapply(fieldsVTA[date_columns], as.Date, format = "%Y-%m-%d")

str(fieldsVTA$Date_Assess_Center_2)
str(fieldsVTA$Sex)

fieldsVTA$AgeSquared <- as.numeric(fieldsVTA$Age_2 ** 2) 

#### Unit Test 
if (fieldsVTA$AgeSquared[556]^(1/2) == fieldsVTA$Age_2 [556]) {
} else {
  show_error("An error occurred. Check Squaring")
}
###

###################### Data Cleaning
# Replace invalid values (e.g., negative values) with NA for relevant columns
columns_to_modify <- c("Depressed_0", "Unenthusiasm_0", "Restlessness_0", "Lethargy_0",
                       "Depressed_1", "Unenthusiasm_1", "Restlessness_1", "Lethargy_1",
                       "Depressed_2", "Unenthusiasm_2", "Restlessness_2", "Lethargy_2",
                       "Depressed_3", "Unenthusiasm_3", "Restlessness_3", "Lethargy_3",
                       "PHQ_2016_Recent_Depression", "PHQ_2016_Recent_Lack_Interest", "Anxiety_2016_Restlessness", "PHQ_2016_Recent_Tiredness",
                       "PHQ_2022_Recent_Depression", "PHQ_2022_Recent_Lack_Interest", "Anxiety_2022_Restlessness", "PHQ_2022_Recent_Tiredness",
                       
                       "Age_first_depression_2016","Age_last_depression_2016","Age_first_depression_2022","Age_last_depression_2022",
                       "PHQ_2016_Recent_Inadequacy","PHQ_2016_Recent_Appetite_change","PHQ_2016_Recent_Self_Harm","PHQ_2016_Recent_Trouble_Concentrating","PHQ_2016_Trouble_Falling_Asleep","PHQ_2016_Any_Sleep_Issues",
                       "PHQ_2022_Recent_Inadequacy","PHQ_2022_Recent_Appetite_change","PHQ_2022_Recent_Self_Harm","PHQ_2022_Recent_Trouble_Concentrating","PHQ_2022_Trouble_Falling_Asleep","PHQ_2022_Any_Sleep_Issues",
                       "COVID_2022_Depression","COVID_2022_Recent_Lack_Interest",
                       "Nb_Depression_Episodes_2","Longest_period_of_depression_2",
                       "Nb_Disinterest_Episodes_2","Longest_period_of_Disinterest_2",
                       "Seen_GP_Nerves_Depression_2","Seen_Psychiatrist_Nerves_Depression_2")

fieldsVTA[columns_to_modify] <- lapply(fieldsVTA[columns_to_modify], function(x) ifelse(x < 0, NA, x))

######## Noticed that 2022 Questionnaires are indexed 0-3 instead of 1-4 
fieldsVTA <- fieldsVTA %>%
  mutate(across(c(PHQ_2022_Recent_Depression, PHQ_2022_Recent_Lack_Interest, Anxiety_2022_Restlessness, PHQ_2022_Recent_Tiredness,
                  PHQ_2022_Recent_Inadequacy, PHQ_2022_Recent_Appetite_change, PHQ_2022_Recent_Self_Harm, PHQ_2022_Recent_Trouble_Concentrating,
                  PHQ_2022_Trouble_Falling_Asleep, PHQ_2022_Any_Sleep_Issues), ~ . + 1))
##########

# -----------------------------------
# Calculating Depression and Anxiety Scores
# -----------------------------------
# Create combined scores for depression symptoms (RDS) across time points
#(the _0 --> _3 represents the different time points)
#-- these are done at the same time of the scanning 
fieldsVTA$RDS_0 <- rowSums(fieldsVTA[, c("Depressed_0", "Unenthusiasm_0", "Restlessness_0", "Lethargy_0")], na.rm = FALSE)
fieldsVTA$RDS_1 <- rowSums(fieldsVTA[, c("Depressed_1", "Unenthusiasm_1", "Restlessness_1", "Lethargy_1")], na.rm = FALSE)
fieldsVTA$RDS_2 <- rowSums(fieldsVTA[, c("Depressed_2", "Unenthusiasm_2", "Restlessness_2", "Lethargy_2")], na.rm = FALSE)
fieldsVTA$RDS_3 <- rowSums(fieldsVTA[, c("Depressed_3", "Unenthusiasm_3", "Restlessness_3", "Lethargy_3")], na.rm = FALSE)

## RDS instances 4 and 5 are the combo of Anxiety + PHQ questions 
#-- these RDS scores are done on the 2016 and 2022 online quesstionnaires and not on the day of scanning 
fieldsVTA$RDS_4 <- rowSums(fieldsVTA[, c("PHQ_2016_Recent_Depression", "PHQ_2016_Recent_Lack_Interest", "Anxiety_2016_Restlessness", "PHQ_2016_Recent_Tiredness")], na.rm = FALSE)
fieldsVTA$RDS_5 <- rowSums(fieldsVTA[, c("PHQ_2022_Recent_Depression", "PHQ_2022_Recent_Lack_Interest", "Anxiety_2022_Restlessness", "PHQ_2022_Recent_Tiredness")], na.rm = FALSE)

############ Because Questions from PHQ and depression questionnaires might have been answered at different times 
# we will discard these subjects (equate them as NA) if the time difference is more than 2 weeks
fieldsVTA$RDS_5 <- ifelse(abs(difftime(fieldsVTA$Date_Anxienty_2022, fieldsVTA$Date_PHQ_2022, units = "days")) > 14, NA, fieldsVTA$RDS_5)

#### Unit Test 
if (fieldsVTA$RDS_2 [34] == (fieldsVTA$Depressed_2 [34] + fieldsVTA$Unenthusiasm_2 [34]+ fieldsVTA$Restlessness_2 [34]+ fieldsVTA$Lethargy_2 [34]) ){
} else {
  show_error("An error occurred. Check Combining Total Column into one")
}

# -----------------------------------
# Merge the Datasets (after data manipulation in each)
# -----------------------------------

merged_data <- full_join(fieldsVTA, dataset_variables, by = "Participant_ID")
merged_data$Participant_ID <- as.factor(merged_data$Participant_ID)

# -----------------------------------
# Data Cleaning - Removing NAs and incomplete responses for some variables 
# -----------------------------------
####### remove incomplete responses for subjects from RDS_2 column
merged_data <- merged_data[complete.cases(merged_data$RDS_2), ]
merged_data <- merged_data[complete.cases(merged_data$ICVUKB), ]
merged_data <- merged_data[complete.cases(merged_data$VTA_QSM), ]

####### Normalize VTA volume by ICV
merged_data$VTA_Volume_Normalized <- merged_data$VTA_volume / merged_data$ICVUKB

#to get the nb of outliers removed at each step we need to start with the number of subjects ---------- The reason why we decided to not include the nb of removed subjects os because we had already excluded the subjects from 
NbofSubjB4OutlierRemoval<-nrow(merged_data)
print(NbofSubjB4OutlierRemoval) #initial nb of participants without removal of any outliers

# -----------------------------------

# -----------------------------------
# Removal of Data that doesnt fall within range that makes sense (ex. 0>FA or 1<FA)
# -----------------------------------

# Set the range within which values are considered non-outliers
lower_limit <- 0
upper_limit <- 1

# Remove observations with values outside the specified range

merged_data <- merged_data %>%
  filter(VTA_FW >= lower_limit, VTA_FW <= upper_limit,
         VTA_ICVF >= lower_limit, VTA_ICVF <= upper_limit,
         VTA_ISOVF >= lower_limit, VTA_ISOVF <= upper_limit,
         VTA_OD >= lower_limit, VTA_OD <= upper_limit,
         VTA_Voxel_Count > 0,
         VTA_volume > 0,
         BMI_2 > 0,
         ICVUKB > 0)


# -----------------------------------
# Visualization before outlier removal: frequency and density distribution
# -----------------------------------
# Generate histograms and density plots for VTA volume, ICV, and normalized ratios
####################### Visualizing VTA volume distribution to see if we should remove outliers before or after ratio - as in removing outliers of each of the volume of the VTA and the outliers of brain volume#####################################

# Decided to take the ratio bc it was still fairly normally distributed 

###############Distribution of VTA volume
library(ggplot2)
Histogram_VTA_B4_STD <-ggplot(merged_data, aes(x = VTA_volume)) + geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) + labs(title = "Distribution of VTA Size", x = "VTA Size", y = "Frequency") + theme_minimal()
plot(Histogram_VTA_B4_STD)

Density_VTA_B4_STD <-ggplot(merged_data, aes(x = VTA_volume)) + geom_density(fill = "blue", alpha = 0.5) + labs(title = "Density Plot of VTA Size", x = "VTA Size", y = "Density") + theme_minimal()
plot(Density_VTA_B4_STD)

Combined_VTA_B4_STD <-ggplot(merged_data, aes(x = VTA_volume)) + geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.4) + geom_density(color = "red", size = 1) + labs(title = "Distribution of VTA Size", x = "VTA Size", y = "Density") + theme_minimal()
plot (Combined_VTA_B4_STD)

########ICV
Histogram_ICV_B4_STD <-ggplot(merged_data, aes(x = ICVUKB)) + geom_histogram(binwidth = 10000, fill = "lightblue", color = "black", alpha = 0.7) + labs(title = "Histogram of Intracranial Volume (ICV)", x = "Intracranial Volume", y = "Frequency") + theme_minimal()
plot(Histogram_ICV_B4_STD)

Density_ICV_B4_STD <-ggplot(merged_data, aes(x = ICVUKB)) + geom_density(fill = "green", alpha = 0.5) + labs(title = "Density Plot of Intracranial Volume (ICV)", x = "Intracranial Volume", y = "Density") + theme_minimal()
plot(Density_ICV_B4_STD)

Combined_ICV_B4_STD <-ggplot(merged_data, aes(x = ICVUKB)) + geom_histogram(aes(y = ..density..), binwidth = 10000, fill = "lightblue", color = "black", alpha = 0.6) + geom_density(color = "red", size = 1) + labs(title = "Distribution of Intracranial Volume (ICV)", x = "Intracranial Volume", y = "Density") + theme_minimal()
plot(Combined_ICV_B4_STD)

########Ratio VTA/ICV
Histogram_Ratio_B4_STD <-ggplot(merged_data, aes(x = VTA_Volume_Normalized)) + geom_histogram(binwidth = 0.00001, fill = "blue", color = "black", alpha = 0.7) + labs(title = "Histogram of Ratio VTA/ICV", x = "Volume Ratio", y = "Frequency") + theme_minimal()
plot(Histogram_Ratio_B4_STD)

Density_Ratio_B4_STD <-ggplot(merged_data, aes(x = VTA_Volume_Normalized)) + geom_density(fill = "blue", alpha = 0.5) + labs(title = "Density Plot of Ratio VTA/ICV", x = "Volume Ratio", y = "Density") + theme_minimal()
plot(Density_Ratio_B4_STD)

Combined_Ratio_B4_STD <-ggplot(merged_data, aes(x = VTA_Volume_Normalized)) + geom_histogram(aes(y = ..density..), binwidth = 0.00001, fill = "blue", color = "black", alpha = 0.4) + geom_density(color = "red", size = 1) + labs(title = "Distribution of Ratio VTA/ICV", x = "Volume Ratio", y = "Density") + theme_minimal()
plot(Combined_Ratio_B4_STD)

# -----------------------------------
# QSM Normalization
# -----------------------------------
min_val <- min(merged_data$VTA_QSM, na.rm = TRUE)
max_val <- max(merged_data$VTA_QSM, na.rm = TRUE)

# Normalize the data using scale()
VTA_QSM_normalized <- scale(merged_data$VTA_QSM, center = min_val, scale = max_val - min_val)
merged_data$VTA_QSM_normalized <- VTA_QSM_normalized[,1]
merged_data$label_column <- ifelse(merged_data$VTA_QSM >= 0, "positive", "negative")
merged_data$label_column <- as.factor(merged_data$label_column)
View(merged_data)

QSM_summary_byGroup <- merged_data %>%
  group_by(label_column,RDS_2) %>%
  summarize(mean_value = mean(VTA_QSM, na.rm =TRUE),
            sd_value = sd(VTA_QSM, na.rm = TRUE))

merged_data <- merged_data[complete.cases(merged_data$VTA_QSM), ]

# -----------------------------------

# -----------------------------------
# Outlier Removal
# -----------------------------------

#to get the nb of outliers removed at each step
# Remove Outliers that are 3 std from mean
merged_data <- merged_data %>%
  filter(!(abs(VTA_FW - (mean(VTA_FW))) > 3 * (sd(VTA_FW))),
         !(abs(VTA_ICVF - (mean(VTA_ICVF))) > 3 * (sd(VTA_ICVF))),
         !(abs(VTA_ISOVF - (mean(VTA_ISOVF))) > 3 * (sd(VTA_ISOVF))),
         !(abs(VTA_OD - (mean(VTA_OD))) > 3 * (sd(VTA_OD))),
         !(abs(VTA_Volume_Normalized - (mean(VTA_Volume_Normalized))) > 3 * (sd(VTA_Volume_Normalized))),
         !(abs(BMI_2 - (mean(BMI_2))) > 3 * (sd(BMI_2))),
         !(abs(VTA_QSM - (mean(VTA_QSM))) > 3 * (sd(VTA_QSM))),
  )


#to get the nb of outliers removed at each step
OutliersRemoved <- NbofSubjB4OutlierRemoval-nrow(merged_data)
print(OutliersRemoved)

# -----------------------------------
# Visualization after outlier removal 
# -----------------------------------

Histogram_VTA_After_STD <- ggplot(merged_data, aes(x = VTA_volume)) + geom_histogram(binwidth = 0.5, fill = "blue", color = "black", alpha = 0.7) + labs(title = "Distribution of VTA Size", x = "VTA Size", y = "Frequency") + theme_minimal()
plot(Histogram_VTA_After_STD)

Density_VTA_After_STD <-ggplot(merged_data, aes(x = VTA_volume)) + geom_density(fill = "blue", alpha = 0.5) + labs(title = "Density Plot of VTA Size", x = "VTA Size", y = "Density") + theme_minimal()
plot(Density_VTA_After_STD)

Combined_VTA_After_STD <-ggplot(merged_data, aes(x = VTA_volume)) + geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "blue", color = "black", alpha = 0.4) + geom_density(color = "red", size = 1) + labs(title = "Distribution of VTA Size", x = "VTA Size", y = "Density") + theme_minimal()
plot (Combined_VTA_After_STD)

########ICV
Histogram_ICV_After_STD <-ggplot(merged_data, aes(x = ICVUKB)) + geom_histogram(binwidth = 10000, fill = "lightblue", color = "black", alpha = 0.7) + labs(title = "Histogram of Intracranial Volume (ICV)", x = "Intracranial Volume", y = "Frequency") + theme_minimal()
plot(Histogram_ICV_After_STD)

Density_ICV_After_STD <-ggplot(merged_data, aes(x = ICVUKB)) + geom_density(fill = "green", alpha = 0.5) + labs(title = "Density Plot of Intracranial Volume (ICV)", x = "Intracranial Volume", y = "Density") + theme_minimal()
plot(Density_ICV_After_STD)

Combined_ICV_After_STD <-ggplot(merged_data, aes(x = ICVUKB)) + geom_histogram(aes(y = ..density..), binwidth = 10000, fill = "lightblue", color = "black", alpha = 0.6) + geom_density(color = "red", size = 1) + labs(title = "Distribution of Intracranial Volume (ICV)", x = "Intracranial Volume", y = "Density") + theme_minimal()
plot(Combined_ICV_After_STD)

########Ratio VTA/ICV

Histogram_Ratio_After_STD <-ggplot(merged_data, aes(x = VTA_Volume_Normalized)) + geom_histogram(binwidth = 0.00001, fill = "blue", color = "black", alpha = 0.7) + labs(title = "Histogram of Ratio VTA/ICV", x = "Volume Ratio", y = "Frequency") + theme_minimal()
plot(Histogram_Ratio_After_STD)

Density_Ratio_After_STD <-ggplot(merged_data, aes(x = VTA_Volume_Normalized)) + geom_density(fill = "blue", alpha = 0.5) + labs(title = "Density Plot of Ratio VTA/ICV", x = "Volume Ratio", y = "Density") + theme_minimal()
plot(Density_Ratio_After_STD)

Combined_Ratio_After_STD <-ggplot(merged_data, aes(x = VTA_Volume_Normalized)) + geom_histogram(aes(y = ..density..), binwidth = 0.00001, fill = "blue", color = "black", alpha = 0.4) + geom_density(color = "red", size = 1) + labs(title = "Distribution of Ratio VTA/ICV", x = "Volume Ratio", y = "Density") + theme_minimal()
plot(Combined_Ratio_After_STD)

# These visualizations help identify trends and confirm data integrity after preprocessing.
# -----------------------------------


# -----------------------------------
# Depression History Variable and Descriptive Statistics - based on the ICD-column
# -----------------------------------
# Create a new variable to label participants with or without a history of depression

###made a new variable merged_data$ICD_Depression that lables patients who had a date Date_F32 or Date_F33 as people who have a history of depression in ICD_Depression. if Date_F32 or Date_F33 were NA then the person doesnt have ICD history  
merged_data$ICD_Depression <- factor(
  ifelse(
    (!is.na(merged_data$Date_F32) & merged_data$Date_F32 < merged_data$Date_Assess_Center_2) | 
      (!is.na(merged_data$Date_F33) & merged_data$Date_F33 < merged_data$Date_Assess_Center_2),
    "Yes", 
    "No"
  )
)


# Compute and print descriptive statistics for participants with and without ICD depression history
num_subjects_with_ICD <- sum(merged_data$ICD_Depression == "Yes")
total_count <- nrow(merged_data)
percentage_with_date <- (num_subjects_with_ICD / total_count) * 100

cat("Number of subjects with Depression History in ICD:", num_subjects_with_ICD, "\n")
cat("Percentage of subjects with Depression History in ICD:", round(percentage_with_date, 2), "%\n")

# Filter the data to include only subjects with a positive ICD history
positive_ICD_history <- merged_data[merged_data$ICD_Depression == "Yes", ]
average_RDS_positive_ICD <- mean(positive_ICD_history$RDS_2, na.rm = TRUE)

negative_ICD_history <- merged_data[merged_data$ICD_Depression == "No", ]
average_RDS_negative_ICD <- mean(negative_ICD_history$RDS_2, na.rm = TRUE)

cat("Average RDS value for subjects with a history of depression in ICD:", round(average_RDS_positive_ICD, 2), "\n")
cat("Average RDS value for subjects with NO HISTORY:", round(average_RDS_negative_ICD, 2), "\n")
# -----------------------------------
