#!/bin/bash
#SBATCH --nodes=1
#SBATCH --cpus-per-task=1
#SBATCH --job-name=coreg_ROIs
#SBATCH --output=logs/MNI2DWI_ROIs_%A_%a.out
#SBATCH --error=logs/MNI2DWI_ROIs_%A_%a.err
#SBATCH --time=6:00:00
#SBATCH --mem=10G                   
#SBATCH --partition=general
#SBATCH --account=XXXXX

# ---------------------------
# SLURM script to warp ROIs from MNI space to subject-specific T1 and DWI space,
# and extract regional diffusion (ICVF, ISOVF, FW, OD) and QSM metrics. 
# 
# Make sure that the following is in the HPC directory:
#       - the required images (QSM,NODDI,FW,T1) 
#       - Subject list array are in the directory before running this script
#       - ROIS list (ROIS=("VTA" "PAG" "LC" "DRN" "NTS")) #subcortical atlas automatically includes all these ROIs 
# ---------------------------

##############
# (passed via array job or external loop)
group=$1
WORK_DIR=$2

##############

# Load required neuroimaging modules (ensure these are available via Neurodesk or system environment)
module use /sw/local/rocky8/noarch/neuro/software/neurocommand/local/containers/modules/

#load modules
ml freesurfer/7.4.1
ml fsl/6.0.7.4
ml mrtrix3/3.0.3
ml ants/2.3.5

##############

SUBJECT_LIST="${WORK_DIR}/${group}/Subjectlist.txt"

#check if results directory exists in 
cd "${WORK_DIR}"

if [[ ! -d "${WORK_DIR}/results_46100_to_46700" ]]; then
    echo "Results directory does not exist. Making directory"
    mkdir -p "${WORK_DIR}/results_46100_to_46700"
fi

subjects=($(cat "$SUBJECT_LIST"))

###############
# seq generates an array starting from the first number, in steps of the second number, until the last number
# Get the subject corresponding to this array task ID
subj=${subjects[$SLURM_ARRAY_TASK_ID]}

echo "SLURM_ARRAY_TASK_ID=$SLURM_ARRAY_TASK_ID"
echo "This will start from subject ${subjects[$SLURM_ARRAY_TASK_ID]} from folder ${group}"

#Define path to ROIlist
cd "${WORK_DIR}/${group}/${subj}"
ROI_LIST="${WORK_DIR}/${group}/${subj}/ROI_list.txt"

#volume
OUTPUT_FILE_VTA_VOLUME="${WORK_DIR}/results_46100_to_46700/VTA_volume.txt"
OUTPUT_FILE_PAG_VOLUME="${WORK_DIR}/results_46100_to_46700/PAG_volume.txt"
OUTPUT_FILE_NTS_VOLUME="${WORK_DIR}/results_46100_to_46700/NTS_volume.txt"
OUTPUT_FILE_LC_VOLUME="${WORK_DIR}/results_46100_to_46700/LC_volume.txt"
OUTPUT_FILE_DRN_VOLUME="${WORK_DIR}/results_46100_to_46700/DRN_volume.txt"
#ICVF
OUTPUT_FILE_VTA_ICVF="${WORK_DIR}/results_46100_to_46700/VTA_ICVF.txt"
OUTPUT_FILE_PAG_ICVF="${WORK_DIR}/results_46100_to_46700/PAG_ICVF.txt"
OUTPUT_FILE_NTS_ICVF="${WORK_DIR}/results_46100_to_46700/NTS_ICVF.txt"
OUTPUT_FILE_LC_ICVF="${WORK_DIR}/results_46100_to_46700/LC_ICVF.txt"
OUTPUT_FILE_DRN_ICVF="${WORK_DIR}/results_46100_to_46700/DRN_ICVF.txt"
#ISOVF
OUTPUT_FILE_VTA_ISOVF="${WORK_DIR}/results_46100_to_46700/VTA_ISOVF.txt"
OUTPUT_FILE_PAG_ISOVF="${WORK_DIR}/results_46100_to_46700/PAG_ISOVF.txt"
OUTPUT_FILE_NTS_ISOVF="${WORK_DIR}/results_46100_to_46700/NTS_ISOVF.txt"
OUTPUT_FILE_LC_ISOVF="${WORK_DIR}/results_46100_to_46700/LC_ISOVF.txt"
OUTPUT_FILE_DRN_ISOVF="${WORK_DIR}/results_46100_to_46700/DRN_ISOVF.txt"
#OD
OUTPUT_FILE_VTA_OD="${WORK_DIR}/results_46100_to_46700/VTA_OD.txt"
OUTPUT_FILE_PAG_OD="${WORK_DIR}/results_46100_to_46700/PAG_OD.txt"
OUTPUT_FILE_NTS_OD="${WORK_DIR}/results_46100_to_46700/NTS_OD.txt"
OUTPUT_FILE_LC_OD="${WORK_DIR}/results_46100_to_46700/LC_OD.txt"
OUTPUT_FILE_DRN_OD="${WORK_DIR}/results_46100_to_46700/DRN_OD.txt"
#QSM
OUTPUT_FILE_VTA_QSM="${WORK_DIR}/results_46100_to_46700/VTA_QSM.txt"
OUTPUT_FILE_PAG_QSM="${WORK_DIR}/results_46100_to_46700/PAG_QSM.txt"
OUTPUT_FILE_NTS_QSM="${WORK_DIR}/results_46100_to_46700/NTS_QSM.txt"
OUTPUT_FILE_LC_QSM="${WORK_DIR}/results_46100_to_46700/LC_QSM.txt"
OUTPUT_FILE_DRN_QSM="${WORK_DIR}/results_46100_to_46700/DRN_QSM.txt"

#FW
OUTPUT_FILE_VTA_FW="${WORK_DIR}/results_46100_to_46700/VTA_FW.txt"
OUTPUT_FILE_PAG_FW="${WORK_DIR}/results_46100_to_46700/PAG_FW.txt"
OUTPUT_FILE_NTS_FW="${WORK_DIR}/results_46100_to_46700/NTS_FW.txt"
OUTPUT_FILE_LC_FW="${WORK_DIR}/results_46100_to_46700/LC_FW.txt"
OUTPUT_FILE_DRN_FW="${WORK_DIR}/results_46100_to_46700/DRN_FW.txt"

cd "${WORK_DIR}/${group}/${subj}"
echo "Current working directory ${WORK_DIR}/${group}/${subj}"

################################################ Analysis ################################################

#extract b0s from DWI series (better contrast for co-registration )
dwiextract  ${subj}_data_ud.nii.gz -bzero ${subj}_b0s.nii -fslgrad ${subj}_bvecs ${subj}_bvals -force

#calculate average b0 (need to use 3D instead of 4D image)
mrmath  ${subj}_b0s.nii mean ${subj}_meanb0.nii  -axis 3 -force

#extraxt brain and brain mask
bet2 ${subj}_meanb0.nii ${subj}_b0_brain -f 0.2  -m

#unzip skull-stripped brains (b0 and T1)
gunzip ${subj}_b0_brain.nii.gz
gunzip T1_brain.nii.gz

#register skull-stripped T1 to b0
antsRegistration --verbose 1 --dimensionality 3 --float 0 --output [ants_t12b0,antsWarped_t12b0.nii.gz,antsInverseWarped_t12b0.nii.gz] --interpolation Linear --use-histogram-matching 1 --winsorize-image-intensities [0.005,0.995] --transform Rigid[0.1] --metric CC[${subj}_b0_brain.nii,T1_brain.nii,1,4,Regular,0.1] --convergence [1000x500x250x100,1e-6,10] --shrink-factors 8x4x2x1 --smoothing-sigmas 3x2x1x0vox --transform Affine[0.1] --metric CC[${subj}_b0_brain.nii,T1_brain.nii,1,4,Regular,0.2] --convergence [1000x500x250x100,1e-6,10] --shrink-factors 8x4x2x1 --smoothing-sigmas 3x2x1x0vox --transform SyN[0.1,3,0] --metric CC[${subj}_b0_brain.nii,T1_brain.nii,1,4] --convergence [100x70x50x20,1e-6,10] --shrink-factors 4x2x2x1 --smoothing-sigmas 2x2x1x0vox -x [reference_mask.nii.gz,input_mask.nii.gz]

#generate an identity (deformation field) warp
warpinit T1_brain.nii identity_warp_t12b0[].nii -force

#transform  identity warp
for i in {0..2}; do
antsApplyTransforms -d 3 -e 0 -i identity_warp_t12b0${i}.nii -o mrtrix_warp_t12b0${i}.nii -r ${subj}_b0_brain.nii -t ants_t12b01Warp.nii.gz -t ants_t12b00GenericAffine.mat --default-value 2147483647
done

#correct warp
warpcorrect mrtrix_warp_t12b0[].nii mrtrix_warp_corrected_t12b0.nii -force

#warp image
mrtransform T1_brain.nii -warp mrtrix_warp_corrected_t12b0.nii ${subj}_warped_T12b0_brain.nii.gz -force

#unzip co-registered T1
gunzip ${subj}_warped_T12b0_brain.nii.gz

#register skull-stripped MNI to co-registered b0
antsRegistration --verbose 1 --dimensionality 3 --float 0 --output [ants_MNI2T1,antsWarped_MNI2T1.nii.gz,antsInverseWarped_MNI2T1.nii.gz] --interpolation Linear --use-histogram-matching 1 --winsorize-image-intensities [0.005,0.995] --transform Rigid[0.1] --metric CC[${subj}_warped_T12b0_brain.nii,mni_t1_brain.nii,1,4,Regular,0.1] --convergence [1000x500x250x100,1e-6,10] --shrink-factors 8x4x2x1 --smoothing-sigmas 3x2x1x0vox --transform Affine[0.1] --metric CC[${subj}_warped_T12b0_brain.nii,mni_t1_brain.nii,1,4,Regular,0.2] --convergence [1000x500x250x100,1e-6,10] --shrink-factors 8x4x2x1 --smoothing-sigmas 3x2x1x0vox --transform SyN[0.1,3,0] --metric CC[${subj}_warped_T12b0_brain.nii,mni_t1_brain.nii,1,4] --convergence [100x70x50x20,1e-6,10] --shrink-factors 4x2x2x1 --smoothing-sigmas 2x2x1x0vox -x [reference_mask.nii.gz,input_mask.nii.gz]

#generate an identity (deformation field) warp
warpinit mni_t1_brain.nii identity_warp_MNI2T1[].nii -force

#transform  identity warp
for i in {0..2}; do
antsApplyTransforms -d 3 -e 0 -i identity_warp_MNI2T1${i}.nii -o mrtrix_warp_MNI2T1${i}.nii -r ${subj}_warped_T12b0_brain.nii -t ants_MNI2T11Warp.nii.gz -t ants_MNI2T10GenericAffine.mat --default-value 2147483647
done

#correct warp
warpcorrect mrtrix_warp_MNI2T1[].nii mrtrix_warp_corrected_MNI2T1.nii -force

#warp image
mrtransform mni_t1_brain.nii -warp mrtrix_warp_corrected_MNI2T1.nii ${subj}_warped_MNI2T1_brain.nii.gz -force

#apply warp to ROIs
for roi in $(cat ${ROI_LIST}) ; do
    mrtransform ${roi}_ATLAS_2022.nii -warp mrtrix_warp_corrected_MNI2T1.nii -interp nearest ${subj}_${roi}.nii -force

    #upsample resulting ROIs back to 0.5x0.5x0.5 (applying warp will change to voxel grid of DWI image)
    mrgrid ${subj}_${roi}.nii regrid -voxel 0.5 ${subj}_${roi}_highRes.nii -force
done

#upsample average b0 to 0.5x0.5x0.5 resolution to match ROIs
mrgrid ${subj}_warped_T12b0_brain.nii regrid -voxel 0.5 ${subj}_warped_T12b0_brain_highRes.nii -force

#unzip QSM
mv QSM_CSFref.nii.gz ${subj}_QSM_CSFref.nii.gz
gunzip ${subj}_QSM_CSFref.nii.gz

#register skull-stripped QSM to co-registered b0
antsRegistration --verbose 1 --dimensionality 3 --float 0 --output [ants_QSM2T1,antsWarped_QSM2T1.nii.gz,antsInverseWarped_QSM2T1.nii.gz] --interpolation Linear --use-histogram-matching 1 --winsorize-image-intensities [0.005,0.995] --transform Rigid[0.1] --metric CC[${subj}_warped_T12b0_brain.nii,${subj}_QSM_CSFref.nii,1,4,Regular,0.1] --convergence [1000x500x250x100,1e-6,10] --shrink-factors 8x4x2x1 --smoothing-sigmas 3x2x1x0vox --transform Affine[0.1] --metric CC[${subj}_warped_T12b0_brain.nii,${subj}_QSM_CSFref.nii,1,4,Regular,0.2] --convergence [1000x500x250x100,1e-6,10] --shrink-factors 8x4x2x1 --smoothing-sigmas 3x2x1x0vox --transform SyN[0.1,3,0] --metric CC[${subj}_warped_T12b0_brain.nii,${subj}_QSM_CSFref.nii,1,4] --convergence [100x70x50x20,1e-6,10] --shrink-factors 4x2x2x1 --smoothing-sigmas 2x2x1x0vox -x [reference_mask.nii.gz,input_mask.nii.gz]

#generate an identity (deformation field) warp
warpinit ${subj}_QSM_CSFref.nii identity_warp_QSM2T1[].nii -force

#transform  identity warp
for i in {0..2}; do
antsApplyTransforms -d 3 -e 0 -i identity_warp_QSM2T1${i}.nii -o mrtrix_warp_QSM2T1${i}.nii -r ${subj}_warped_T12b0_brain.nii -t ants_QSM2T11Warp.nii.gz -t ants_QSM2T10GenericAffine.mat --default-value 2147483647
done

#correct warp
warpcorrect mrtrix_warp_QSM2T1[].nii mrtrix_warp_corrected_QSM2T1.nii -force

#warp image
mrtransform ${subj}_QSM_CSFref.nii -warp mrtrix_warp_corrected_QSM2T1.nii ${subj}_warped_QSM2T1_brain.nii.gz -force

################################################ Extract values ################################################
###################
##### Volume
###################
cd "${WORK_DIR}/${group}/${subj}"

#check if output files that will store measures have headers (you can change to whatever you calculate)
for file in $OUTPUT_FILE_VTA_VOLUME $OUTPUT_FILE_PAG_VOLUME $OUTPUT_FILE_NTS_VOLUME $OUTPUT_FILE_LC_VOLUME $OUTPUT_FILE_DRN_VOLUME; do
    if [ ! -s "$file" ]; then
        echo "Subject_ID voxels volume(mm3)" > "$file"
    fi
done

#cycle through ROIs to calculate volume and append to main output files
for roi in $(cat ${ROI_LIST}); do
        output=$(fslstats ${subj}_warped_T12b0_brain_highRes.nii -k ${subj}_${roi}_highRes.nii -V)

        # Determine the correct output file for the current ROI
       if [ "$roi" == "VTA" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_VTA_VOLUME
        elif [ "$roi" == "PAG" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_PAG_VOLUME
        elif [ "$roi" == "NTS" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_NTS_VOLUME
        elif [ "$roi" == "LC" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_LC_VOLUME
        elif [ "$roi" == "DRN" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_DRN_VOLUME
        fi

        # Check if the command produced any output
        if [ -z "$output" ]; then
            echo "fslstats didn't produce any output for ID $subj" >> $OUTPUT_FILE
        else
            # Append both ${subj} and the captured output to the OUTPUT_FILE
            echo "${subj} $output" >> $OUTPUT_FILE
        fi
done

###################
##### ICVF
###################

#check if output files that will store measures have headers (you can change to whatever you calculate)
for file in $OUTPUT_FILE_VTA_ICVF $OUTPUT_FILE_PAG_ICVF $OUTPUT_FILE_NTS_ICVF $OUTPUT_FILE_LC_ICVF $OUTPUT_FILE_DRN_ICVF; do
    if [ ! -s "$file" ]; then
        echo "Subject_ID ICVF" > "$file"
    fi
done

#upsample  to 0.5x0.5x0.5 resolution to match ROIs
mrgrid ${subj}_NODDI_ICVF.nii.gz regrid -voxel 0.5 ${subj}_NODDI_ICVF_highRes.nii.gz -force

#cycle through ROIs to calculate ICVF and append to main output files
for roi in $(cat ${ROI_LIST}); do
        output=$(mrstats ${subj}_NODDI_ICVF_highRes.nii.gz -mask ${subj}_${roi}_highRes.nii -output mean)

        # Determine the correct output file for the current ROI
       if [ "$roi" == "VTA" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_VTA_ICVF
        elif [ "$roi" == "PAG" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_PAG_ICVF
        elif [ "$roi" == "NTS" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_NTS_ICVF
        elif [ "$roi" == "LC" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_LC_ICVF
        elif [ "$roi" == "DRN" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_DRN_ICVF
        fi

        # Check if the command produced any output
        if [ -z "$output" ]; then
            echo "mrstats didn't produce any output for ID $subj" >> $OUTPUT_FILE
        else
            # Append both ${subj} and the captured output to the OUTPUT_FILE
            echo "${subj} $output" >> $OUTPUT_FILE
        fi

done

###################
##### ISOVF
###################

#check if output files that will store measures have headers (you can change to whatever you calculate)
for file in $OUTPUT_FILE_VTA_ISOVF $OUTPUT_FILE_PAG_ISOVF $OUTPUT_FILE_NTS_ISOVF $OUTPUT_FILE_LC_ISOVF $OUTPUT_FILE_DRN_ISOVF; do
    if [ ! -s "$file" ]; then
        echo "Subject_ID ISOVF" > "$file"
       fi
done

#upsample  to 0.5x0.5x0.5 resolution to match ROIs
mrgrid ${subj}_NODDI_ISOVF.nii.gz regrid -voxel 0.5 ${subj}_NODDI_ISOVF_highRes.nii.gz -force

#cycle through ROIs to calculate ISOVF and append to main output files
for roi in $(cat ${ROI_LIST}); do
        output=$(mrstats ${subj}_NODDI_ISOVF_highRes.nii.gz -mask ${subj}_${roi}_highRes.nii -output mean)

        # Determine the correct output file for the current ROI
       if [ "$roi" == "VTA" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_VTA_ISOVF
        elif [ "$roi" == "PAG" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_PAG_ISOVF
        elif [ "$roi" == "NTS" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_NTS_ISOVF
        elif [ "$roi" == "LC" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_LC_ISOVF
        elif [ "$roi" == "DRN" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_DRN_ISOVF
        fi

        # Check if the command produced any output
        if [ -z "$output" ]; then
            echo "mrstats didn't produce any output for ID $subj" >> $OUTPUT_FILE
        else
            # Append both ${subj} and the captured output to the OUTPUT_FILE
            echo "${subj} $output" >> $OUTPUT_FILE
        fi

done

###################
##### OD
###################

#check if output files that will store measures have headers (you can change to whatever you calculate)
for file in $OUTPUT_FILE_VTA_OD $OUTPUT_FILE_PAG_OD $OUTPUT_FILE_NTS_OD $OUTPUT_FILE_LC_OD $OUTPUT_FILE_DRN_OD; do
    if [ ! -s "$file" ]; then
        echo "Subject_ID OD" > "$file"
       fi
done

#upsample  to 0.5x0.5x0.5 resolution to match ROIs
mrgrid ${subj}_NODDI_OD.nii.gz regrid -voxel 0.5 ${subj}_NODDI_OD_highRes.nii.gz -force

#cycle through ROIs to calculate OD and append to main output files
for roi in $(cat ${ROI_LIST}); do
        output=$(mrstats ${subj}_NODDI_OD_highRes.nii.gz -mask ${subj}_${roi}_highRes.nii -output mean)

        # Determine the correct output file for the current ROI
       if [ "$roi" == "VTA" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_VTA_OD
        elif [ "$roi" == "PAG" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_PAG_OD
        elif [ "$roi" == "NTS" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_NTS_OD
        elif [ "$roi" == "LC" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_LC_OD
        elif [ "$roi" == "DRN" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_DRN_OD
        fi

        # Check if the command produced any output
        if [ -z "$output" ]; then
            echo "mrstats didn't produce any output for ID $subj" >> $OUTPUT_FILE
        else
            # Append both ${subj} and the captured output to the OUTPUT_FILE
            echo "${subj} $output" >> $OUTPUT_FILE
        fi

done

###################
##### QSM
###################

#check if output files that will store measures have headers (you can change to whatever you calculate)
for file in $OUTPUT_FILE_VTA_QSM $OUTPUT_FILE_PAG_QSM $OUTPUT_FILE_NTS_QSM $OUTPUT_FILE_LC_QSM $OUTPUT_FILE_DRN_QSM; do
    if [ ! -s "$file" ]; then
        echo "Subject_ID QSM" > "$file"
       fi
done

#upsample  to 0.5x0.5x0.5 resolution to match ROIs
mrgrid ${subj}_warped_QSM2T1_brain.nii.gz regrid -voxel 0.5 ${subj}_QSM_highRes.nii -force

#cycle through ROIs to calculate QSM and append to main output files
for roi in $(cat ${ROI_LIST}); do
        output=$(mrstats ${subj}_QSM_highRes.nii -mask ${subj}_${roi}_highRes.nii -output mean)

        # Determine the correct output file for the current ROI
       if [ "$roi" == "VTA" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_VTA_QSM
        elif [ "$roi" == "PAG" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_PAG_QSM
        elif [ "$roi" == "NTS" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_NTS_QSM
        elif [ "$roi" == "LC" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_LC_QSM
        elif [ "$roi" == "DRN" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_DRN_QSM
        fi

        # Check if the command produced any output
        if [ -z "$output" ]; then
            echo "mrstats didn't produce any output for ID $subj" >> $OUTPUT_FILE
        else
            # Append both ${subj} and the captured output to the OUTPUT_FILE
            echo "${subj} $output" >> $OUTPUT_FILE
        fi

done

###################
##### FW
###################

#check if output files that will store measures have headers (you can change to whatever you calculate)
for file in $OUTPUT_FILE_VTA_FW $OUTPUT_FILE_PAG_FW $OUTPUT_FILE_NTS_FW $OUTPUT_FILE_LC_FW $OUTPUT_FILE_DRN_FW; do
    if [ ! -s "$file" ]; then
        echo "Subject_ID FW" > "$file"
       fi
done

#upsample to 0.5x0.5x0.5 resolution to match ROIs
mrgrid ${subj}_FW.nii.gz regrid -voxel 0.5 ${subj}_FW_highRes.nii -force

#cycle through ROIs to calculate FW and append to main output files
for roi in $(cat ${ROI_LIST}); do
        output=$(mrstats ${subj}_FW_highRes.nii -mask ${subj}_${roi}_highRes.nii -output mean)

        # Determine the correct output file for the current ROI
       if [ "$roi" == "VTA" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_VTA_FW
        elif [ "$roi" == "PAG" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_PAG_FW
        elif [ "$roi" == "NTS" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_NTS_FW
        elif [ "$roi" == "LC" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_LC_FW
        elif [ "$roi" == "DRN" ]; then
            OUTPUT_FILE=$OUTPUT_FILE_DRN_FW
        fi

        # Check if the command produced any output
        if [ -z "$output" ]; then
            echo "mrstats didn't produce any output for ID $subj" >> $OUTPUT_FILE
        else
            # Append both ${subj} and the captured output to the OUTPUT_FILE
            echo "${subj} $output" >> $OUTPUT_FILE
        fi

done

