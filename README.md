# VTA_UKB_Inflammation_Paper

## Overview
This repository contains two scripts designed for processing, analyzing, and visualizing neuroimaging biomarkers and clinical measures related to depression severity. The analysis leverages advanced statistical methods and data visualization techniques to explore relationships between brain structure, inflammation markers, and depressive symptomatology.

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

### **Side note**

Figure 2 (subject T1 with VTA atlas) was visualized by using fsleyes 
