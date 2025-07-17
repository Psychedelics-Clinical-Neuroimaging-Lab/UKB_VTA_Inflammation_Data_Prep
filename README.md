# VTA_UKB_Inflammation_Paper

## Overview
This repository contains four scripts designed for processing, analyzing, and visualizing neuroimaging biomarkers and clinical measures related to depression severity. The analysis explores relationships between brain structure, inflammation markers, and depressive symptomatology in the UKB by using diffusion MRI and Quantitative Susceptibility Mapping.

## Slurm scripts 
### 1. **dwi-prep.slurm**
DWI Preprocessing: Data Preparation Script -This is an example of the image prep pipeline. 
This SLURM script prepares DWI subject folders for downstream analysis by automating three key steps:
Unzipping: Extracts subject ZIP archives into individual folders.
File Organization: Moves and renames required files (e.g., bvals, NODDI_ICVF.nii.gz) to each subject's root directory.
Quality Control: Logs missing files in MissingFilesReport.txt and removes incomplete subjects or unnecessary subdirectories.

Runs in parallel using SLURM array jobs across subject batch folders (e.g., 46100, 46200, ...).
Produces clean, standardized subject directories ready for DWI metric extraction.

### 2. **MNI2DWI_ROIs_T1**
This script warps ROIs from MNI space to subject-specific T1 and DWI space, and extracts regional diffusion (ICVF, ISOVF, FW, OD) and QSM metrics. 
To run the script assumes the following are in the HPC directory:
       - the required images (QSM,NODDI,FW,T1) 
       - Subject list array are in the directory before running this script
       - ROIS list (ROIS=("VTA" "PAG" "LC" "DRN" "NTS")) #subcortical atlas automatically includes all these ROIs 

---

## R Scripts

### 1. **DataCleaningVTA.R**
This script focuses on preparing and cleaning the dataset for further analysis. Key functionalities include:
- Loading and merging datasets.
- Preprocessing steps to calculate derived variables such as normalized volumes and free-water maps.
- Handling missing data and generating descriptive statistics.

---

### 2. **DescriptiveStatsVTA.R**
This script performs statistical analyses and generates visualizations for the dataset prepared in the first script.

**Key Objectives:**
1. Generate descriptive statistics for both the full dataset and a subset of participants with a history of depression.
2. Visualize depressive symptom severity using:
   - Bar plots for RDS scores.
   - Interactive visualizations comparing RDS scores by ICD depression status.
3. Conduct ANOVA and linear regression analyses to assess the effects of neuroimaging biomarkers on depressive symptoms.
4. Save publication-ready tables and visualizations.

### 3. **VTA_Supp_Material_ROI_analysis.R**
This script looks at the metrics in different ROIs implicated in depression and updates the regression model. These updates are part of the reviewers suggestions. 

### **Side note**

Figure 2 (subject T1 with VTA atlas) was visualized by using fsleyes 
