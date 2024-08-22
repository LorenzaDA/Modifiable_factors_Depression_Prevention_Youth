###############
# 2B. Step 1 - sensitivity TTE - GenR
###############
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: Generation R data waves @9 and @13
# AIM OF SCRIPT: Explore associations at 1) a cross-sectional level (int t1), 2) longitudinal level, 3) under all TTE conditions
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)


####
# set environment
####

# load the sample that is not filtered for those with clinical levels of int problems 

rm(list = ls())

source("PATH-WHERE-SOURCE-FILE-IS/0a.Source_file_modifiable_dep_youth_GenR.R")
source("/PATH-WHERE-SOURCE-FILE-IS/0b.Source_file_functions_modifiable_dep_youth_GenR.R")

dd <-  readRDS(paste0(indata, "modifiable_dep_youth_sens_tte_withClinical_Jan2024_GenR.rds"))


#####
# STEP 1: cross-sectional association
#####

# Your baseline variables
# nb here ethnicity is there cause it wasn't previously residualized for that like the t2 version (see 1.dataprep for rationale)
baselinevars <- c("sex", "ethn", "puberty", "age", "parent_edu", "par_psych", "income", "mat_age")


# modifiable factors
pred_vars <- c("pa", "friends", "sleep", "splines::ns(screen,2)")


# scale outcomes
dd$int_t1_centered <- scale(dd$int_t1)
dd$int_t2_centered <- scale(dd$int_t2)

# cross-sectional regression
results_cs <- perform_regression_CS(baselinevars, pred_vars)

# store results and round to 3 decimals
results_cs[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results_cs[, c("Coefficient", "Standard_Error", "P_Value")], 3)


#####
# STEP 2: longitudinal without baseline adjustments 
#####

# without ethn cause it's residualized for int t2
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "income", "mat_age")

# run regression
results_longi <- perform_regression_longi(baselinevars, pred_vars)

# store results and round to 3 decimals
results_longi[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results_longi[, c("Coefficient", "Standard_Error", "P_Value")], 3)


######
# STEP 3: Results when all TTE conditions are implemented
######

# not clinically-relevant internalizing symptoms
# NB this is a slightly different sample from the one for the main analyses because
# of the sibling selection 
dd2 <- dd[dd$int_t1 < 6, ] 

# adjustments for internalizing at baseline are added
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "income", "mat_age", "int_t1")

# perform regression
results_TTE <- perform_regression_longi(baselinevars, pred_vars)

# store results and round to 3 decimals
results_TTE[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results_TTE[, c("Coefficient", "Standard_Error", "P_Value")], 3)

# table to visualize results across the analytic strategies
cs <- "cross-sectional results"
longi <- "longitudinal results (no adjustments)"
tte <- "TTE framework"

results <- rbind(cs, results_cs, longi, results_longi, tte, results_TTE)

# save
write.csv(results, paste0(res, "results_TTEcomparisons_Jan2024.csv"))
