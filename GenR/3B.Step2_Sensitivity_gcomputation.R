#############
# 3B. G formula - sensitivity analyses
#############
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: Generation R data waves @9 and @13
# AIM OF SCRIPT: run sensitivity analyses for the g-formula analyses. these included
# 1. natural course scenario as comparison (predicted int under intervention vs predicted int if behavior of children remains unchanged)
# 2. model misspecification (predicted vs observed internalizing at T2)
# 3. results when adjustments are for T0 internalizing problems (6 years) instead of T1 internalizing problems (10 years)
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)


####
# set environment
####


rm(list=ls())
source("/PATH-WHERE-SOURCE-FILE-IS/0a.Source_file_modifiable_dep_youth_GenR.R")
source("/PATH-WHERE-SOURCE-FILE-IS/0b.Source_file_functions_modifiable_dep_youth_GenR.R")
load(paste0(indata, "/", "modifiable_dep_youth_alldata_genr.RData"))


# set covs
# NB ethn previously residualized for - check rationale in 1.Dataprep script
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "income", "mat_age", "int_t1")

# set datasets
datasets <- list(dd4, hGr, hEr, indicated)
names(datasets) <- c("universal", "selective_genetics", "selective_trauma",
                     "indicated")

# interventions 
screen_levels <- c("0", "1", "2", "3", "4")


#####
# Analyses
#####

### sensitivity - natural course ###
predict_and_contrast_screen_nat(datasets = datasets,
                                levels = screen_levels, 
                                baselinevars = baselinevars)


### sensitivity - model misspecification check ###
screen_modelmisp(datasets = datasets, 
                 baselinevars = baselinevars, 
                 filepath = res)


### adjustments for T0 internalizing problems ###
# this is to test whether the adjustments for T1 internalizing problems might have been overly
# stringent or introduced bias
# instead of adjusting for T1 internalizing problems, we adjust for T0 internalizing problems (age 5 years)

res <- "/SET-NEW-PATH-WHERE-TO-SAVE-RESULTS/"

# change covariate set so it includes T0 internalizing problems instead of T1 
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "income", "mat_age", "int_5")

# rerun the g-formula and calculate causal contrasts
predict_and_contrast_screen_boot(datasets = datasets, 
                                 levels = screen_levels, 
                                 baselinevars = baselinevars,
                                 n_boot = 1000, 
                                 seed = 2023)

