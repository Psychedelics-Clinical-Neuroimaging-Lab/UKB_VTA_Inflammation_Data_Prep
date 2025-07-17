#!/bin/bash --login
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --array=0-6
#SBATCH --cpus-per-task=1
#SBATCH --mem=10G
#SBATCH --job-name=dwi-prep
#SBATCH --time=02:00:00
#SBATCH --partition=general
#SBATCH --account=a_lena_neuro
#SBATCH -o slurm_%j_output
#SBATCH -e slurm_%j_error

# ------------------------------------------------------------------------------
# This script prepares DWI data by:
# 1. Unzipping subject folders,
# 2. Moving relevant files up to the subject directory and deleting bad subjects,
# 3. Cleaning up unnecessary subfolders.
# Used in SLURM array job format to process chunks of subjects in parallel.
# ------------------------------------------------------------------------------

# seq generates an array starting from the first number, in steps of the second number, until the last number
array=( $(seq 46100 100 46700) )

## pass the working directory in array job or external loop
WORK_DIR=$1

# Print the current array index and corresponding folder
echo SLURM_ARRAY_TASK_ID=$SLURM_ARRAY_TASK_ID
echo This will start from file ${array[$SLURM_ARRAY_TASK_ID]}

# Define the target directory for the current array index
TARGET_DIR="/scratch/project_mnt/S0105/free-water-data/${array[$SLURM_ARRAY_TASK_ID]}"

# Exit if the target directory does not exist
if [[ ! -d "${TARGET_DIR}" ]]; then
  echo "Target directory does not exist: ${TARGET_DIR}"
  exit 1
fi

cd "${TARGET_DIR}"
# ------------------------------------------------------------------------------
# STEP 1: Generate or verify Subjectlist.txt
# ------------------------------------------------------------------------------
# Create Subjectlist.txt if it doesn't exist, by listing folder names and cleaning up
if [ -e "${WORK_DIR}/${array[$SLURM_ARRAY_TASK_ID]}/Subjectlist.txt" ]; then
    echo "Subjectlist.txt exists."
  else
    echo "Subjectlist.txt does not exist. Creating it"
    ls > Subjectlist.txt
    ls | grep -v 'Subjectlist.txt' > Subjectlist.txt
    awk -F'_' '{print $1}' Subjectlist.txt > Subjectlist_tmp.txt && mv Subjectlist_tmp.txt Subjectlist.txt
fi

#If subject list already exists (from previous )
SUBJECT_LIST="${WORK_DIR}/${array[$SLURM_ARRAY_TASK_ID]}/Subjectlist.txt"

# ------------------------------------------------------------------------------
# STEP 2: Unzip subject files into individual subject folders
# ------------------------------------------------------------------------------
while IFS= read -r subj; do
    echo "Processing: ${subj}"
    # Create subject directory if it doesn't exist
    if [[ ! -d "${subj}" ]]; then
        echo "Target directory does not exist: ${subj}"
        mkdir -p "${subj}"
    fi
    

# Check if any matching zip files exist
    if ls "${subj}"*.zip 1> /dev/null 2>&1; then

# Unzip and check if unzip was successful
        if unzip "${subj}"*.zip -d "${subj}/"; then
            echo "Extraction successful, deleting ${subj}*.zip"
            rm "${subj}"*.zip
        else
            echo "Error: Extraction of ${subj}*.zip failed."
        fi
    else
        echo "Warning: No zip files found for ${subj}"
    fi
done < "${SUBJECT_LIST}"

# ------------------------------------------------------------------------------
# STEP 3: Move key files to subject root and remove subjects with missing files
# ------------------------------------------------------------------------------
#create a list of subject IDs that are missing some or all of the files and specify which files they're missing
MISSING_FILES_REPORT="${WORK_DIR}/${array[$SLURM_ARRAY_TASK_ID]}/MissingFilesReport.txt"

# Initialize or clear the report file
echo "SubjectID, MissingFiles" > "${MISSING_FILES_REPORT}"

#define images to keep and move up in directory structure 
while IFS= read -r subj; do
    FILES_TO_FIND=("bvals" "bvecs" "nodif_brain_mask.nii.gz" "nodif_brain.nii.gz" "data_ud.nii.gz" "NODDI_ICVF.nii.gz" "NODDI_ISOVF.nii.gz" "NODDI_OD.nii.gz")
    MISSING_FILES=()
    
    # Part 1: Move and Rename Files
    for file in "${FILES_TO_FIND[@]}"; do
        find "${subj}" -type f -name "${file}" -exec bash -c 'mv "$0" "${1}/${1}_$(basename "$0")"' {} "${subj}" \;
    done
    
    # Part 2: Identify Missing Files
    for file in "${FILES_TO_FIND[@]}"; do
        # Check if the moved & renamed file exists
        if [[ ! -f "${subj}/${subj}_${file}" ]]; then
            MISSING_FILES+=("${file}")
        fi
    done
    
    # If there are missing files, report them
    if [ ${#MISSING_FILES[@]} -gt 0 ]; then
        echo "${subj}, ${MISSING_FILES[*]}" >> "${MISSING_FILES_REPORT}"
    fi
done < "${SUBJECT_LIST}"

# Delete Directories with Missing Files
while IFS=, read -r subj missing_files; do
    if [[ -n "${missing_files// }" && -d "${subj}" ]]; then
        # Ensure to test thoroughly before uncommenting the next line!
         rm -r "${subj}"
        echo "Deleted: ${subj}"
    fi
done < <(tail -n +2 "${MISSING_FILES_REPORT}")

# ------------------------------------------------------------------------------
# STEP 4: Remove unnecessary subdirectories inside subject folders
# ------------------------------------------------------------------------------

#delete subdirectories not needed for further analyses
while IFS= read -r subj; do
    # Check if directory exists and is accessible
    if [[ -d "${subj}" ]]; then
        cd "${subj}" || { echo "Cannot change to directory ${subj}, skipping."; continue; }

        # Check for the presence of subdirectories and delete them.
        if find . -mindepth 1 -type d | read; then
            find . -mindepth 1 -type d -exec rm -r {} +
        else
            echo "No subdirectories found for ${subj}, leaving it untouched."
        fi

        cd - || { echo "Cannot change back to previous directory, exiting."; exit 1; }
    else
        echo "Directory ${subj} not found, skipping."
    fi
done < "${SUBJECT_LIST}"



