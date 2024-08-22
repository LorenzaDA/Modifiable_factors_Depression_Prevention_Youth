##################
# 3A. Step 2 - g-formula GenR
##################
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: Generation R data waves @9 and @13
# AIM OF SCRIPT: Run g-formula for screen time
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)


####
# set environment
####

rm(list=ls())
source("/PATH-WHERE-SOURCE-FILE-IS/0a.Source_file_modifiable_dep_youth_GenR.R")
source("/PATH-WHERE-SOURCE-FILE-IS/0b.Source_file_functions_modifiable_dep_youth_GenR.R")
load(paste0(indata, "modifiable_dep_youth_alldata_inclSens_genr.RData"))


# covariates
# NB site and ethn previously adj for (see 1.Dataprep script for rationale)
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "income", "mat_age", "int_t1")

# set datasets
# dd4 = universal prevention setting
# hGr = high genetic liability setting
# hEr = high exposure to stressful life events setting
# indicated = indicated prevention setting
# low_hGr = low genetic liability setting (for sensitivity analyses)
# low_indicated = low symptoms of internalizing at baseline (for sensitivity analyses)
datasets <- list(dd4, hGr, hEr, indicated, low_hGr, low_indicated)
names(datasets) <- c("universal", "selective_PGS", "selective_trauma",
                     "indicated", "low_genetics", "low_indicated")



#####
# screen time - main analysis (g formula hard assignment)
#####

screen_levels <- c("0", "1", "2", "3", "4")

predict_and_contrast_screen_boot(datasets = datasets, 
                                 levels = screen_levels, 
                                 baselinevars = baselinevars,
                                 n_boot = 1000, 
                                 seed = 2023)



