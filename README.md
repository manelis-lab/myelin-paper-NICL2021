Code and data for:
Baranger, D.A.A., Halchenko, Y.O., Satz, S., Ragozzino, R., Iyengar, S., Swartz, H.A., Manelis, A. Neuroimage: Clinical (2021).
Aberrant levels of cortical myelin distinguish individuals with depressive disorders from healthy controls.

#============= FOLDER STRUCTURE ============= #

MethodX_data/:

data
--mri_derivatives
-----primary/ses-01 ** surface-level T1w/T2w ratio and cortical thickness images for subjects in primary analyses
-----followup       ** surface-level T1w/T2w ratio and cortical thickness images for the subject with follow-up data who converted
-------ses-01       ** follow-up subject session 1
-------ses-02       ** follow-up subject session 2
--other_input       ** clinical, demographic, and parcellated T1w/T2w ratio data
outputs
--cvs               ** output of posthoc analyses varying the number of cross-validation folds
----loocv_inneronly ** output of posthoc analyses varying the number of inner cross-validation folds
--glmnet            ** output of primary glmnet analysis
--permutations      ** output of permutation analyses
--preprocessing     ** output of preprocessing outlier detection
--regressions       ** output of regression analyses
scripts
--analyses          ** scripts for primary analyses, including glmnet, permutations, and regressions
--figures           ** scripts to create figures in the paper
--followup          ** scripts for post hoc analyses
--preprocessing     ** scripts for parcellating mri derivative files and outlier detection


#============= FOLDER CONTENTS ============= #

MethodX_data/data/mri_derivatives/primary/ses-01:
sub-******.L.midthickness.32k_fs_LR.surf.gii           ** left cortical thickness file
sub-******.R.midthickness.32k_fs_LR.surf.gii           ** right cortical thickness file
sub-******.SmoothedMyelinMap_BC.32k_fs_LR.dscalar.nii  ** cortical myelin cifti file

MethodX_data/data/other_input:
converted_participant_parcels_bothsessions.xlsx        ** Cortical myelin values for the follow-up subject who converted
data_360parcels_Glasser32K.csv                         ** Cortical myelin for 360 Glasser parcels, output of scripts/preprocessing/parcellate.R
data_clinical_and_parcels_all.csv                      ** Participant demographics, clinical variables, and cortical myelin values
data_dictionary.csv                                    ** Description of columns in data_clinical_and_parcels_all.csv  
ElasticNet_variables.csv                               ** All variables used for elastic net analyses
glmnet_performance.csv                                 ** Performance metrics for glmnet/LDA classifier

MethodX_data/outputs/cvs:
glmnet_leave-one-out_nested_1uniquepair_removed_HC_UD_[x]_outer_[y]_internalfolds_2021-07-02.txt  ** output of follow up analyses, varying both the internal [y] and outer [x] cv folds

MethodX_data/outputs/cvs/loocv_inneronly:
glmnet_leave-one-out_nested_1uniquepair_removed_HC_UD_[i]_internalfolds_2021-07-02.txt            ** output of follow up analyses, varying the number of inner cv folds [i] (retaining 2 pairs held-out)

MethodX_data/outputs/glmnet:
glmnet_variable_selection.csv                                                           ** frequency of variable selection in true and permutation analyses
glmnet_with_age_sex_iq_leave-one-out_nested_1uniquepair_removed_HC_UD_2020-11-26.txt    ** main results, output of scripts/analyses/glmnet_with_LDA_myelin_paper.R
predict.followup.txt                                                                    ** predicted class for followup participant who converted mid-study

MethodX_data/outputs/permutations:
split[i]_glmnet_permuted_labels_10times_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_2021-02-22 ** Outputs of permutation analyses (100 permutations per file)
split[i]_glmnet_permuted_sets_10times_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_2021-02-22   ** Outputs of permutation analyses (100 permutations per file) - record of all permutation combinations
glmnet_permuted_labels_100times_for_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_2021-02-22.txt ** Combined all label files
glmnet_permuted_sets_100times_for_leave-one-out_nested_with_age_sex_iq_1uniquepair_removed_HC_UD_2021-02-22.txt   ** Combined all set files

MethodX_data/outputs/preprocessing:
outlier.results.csv                         ** results of parcel outlier detection

MethodX_data/outputs/regressions:
regression_dd_control_myelin.csv            ** results of regression analyses between control/dd and myelin
regression_demo_clin_myelin.csv             ** results of regression analyses between clinical/demographic variables and myelin
regression_performance_myelin.csv           ** results of regression analyses between lda accuracy and myelin

MethodX_data/scripts/analyses:
glmnet_with_LDA_myelin_paper.R              ** Nested cross-validation elastic net regression with LDA (primary analysis)
permuted_glmnet_with_LDA_myelin_paper.R     ** Permutation analyses
process_glmnet_output.R                     ** Compute performance metrics of the primary analysis
regression_lda_performance_and_clinical.R   ** Regression analyses between model accuracy and clinical variables
regression_myelin_and_clinical.R            ** Regression analyses between cortical myelin and clinical variables
regression_myelin_patients_vs_controls.R    ** regression analyses comparing myelin values in patients and controls

MethodX_data/scripts/figures:
brain_plot.R                     ** Code for Figure 3
performance_plots.R              ** Code for Figure 2
plot_antidepressants.R           ** Code for Supplemental Figure 3

MethodX_data/scripts/followup:
glmnet_with_LDA_myelin_paper_cvs_outerloop.R      ** repeating the glmnet analyses, varying both the number of inner and outer cv folds
glmnet_with_LDA_myelin_paper_cvs.R                ** repeating the glmnet analyses, varying the number of inner cv folds
Predict_converted.R                               ** predict the group (control, DD) of the participant who converted
process_cvs_parcels.R                             ** process the output of glmnet_with_LDA_myelin_paper_cvs.R & glmnet_with_LDA_myelin_paper_cvs_outerloop.R

MethodX_data/scripts/preprocessing:
outlier_regions.R                    ** Detects outlier parcels
parcellate.R                         ** Parcellate mri derivative files to compute mean T1w/T2w ratio value for each parcel


#============= Other needed files ============= #

https://balsa.wustl.edu/88mp  ** Glasser 360 parcellation atlas

