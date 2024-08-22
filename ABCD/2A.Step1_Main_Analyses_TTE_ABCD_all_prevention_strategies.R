################
# 2A. STEP1: Prospective associations under TTE conditions
################
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: ABCD (release 4.0, from download data manager)
# AIM OF SCRIPT: test the prospective association of each modifiable factor with internalizing problems under TTE conditions
# in all preventive scenarios (universal, selective, indicated)
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)


### set environment ###

rm(list=ls())
source("/PATH-WHERE-SOURCE-FILE-IS/0a.source_file_packages_paths_ABCD.R") # load paths and packages
source("/PATH-WHERE-SOURCE-FILE-IS/0b.source_file_analyses_ABCD.R") # load functions
load(paste0(indata, "modifiable_dep_youth_alldata.RData")) # load datasets (this includes the universal, selective and indicate preventive scenarios datasets)

# covs (nb ethn and site residualized for - see script 1 for rationale)
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "int_t1", "mat_age", "income")

# create a list of your datasets and name them - this is for analysis and output prep
datasets <- list(dd4, hEr, hGr, indicated)
names(datasets) <- c("universal", "selective_trauma", "selective_genetics",
                     "indicated")


# vector for all your predictors that you plan to test in relation to internalizing problems
pred_vars <- c("pa", "friends", "sleep", "splines::ns(screen,2)")

# create a list and df structure for storing the results
all_results <- list()

combined_results <- data.frame(Variable = character(),
                               Coefficient = numeric(),
                               Standard_Error = numeric(),
                               P_Value = numeric(),
                               Dataset = character(),
                               row.names = NULL)


### perform regressions ###

# for each dataset (universal, selective (x2), indicated preventive scenario)
for (i in seq_along(datasets)) {
  # perform a regression analysis within that dataset, adjusting for covariates, for each predictor variable (sleep, pa, screen time, friendship)
  results <- perform_regression(datasets[[i]], baselinevars, pred_vars)
  
  # save the results (unrounded format) for each dataset
 unrounded_result_filename <- paste0(res, names(datasets)[i], "_unrounded_results.csv")
 write.csv(results, file = unrounded_result_filename, row.names = FALSE)
  
  # of the results keep the coefficient, SE and p value and round them to 3 decimals
  results[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results[, c("Coefficient", "Standard_Error", "P_Value")], 3)
  
  # create a column that specifies which dataset the results are from (i.e., prevention scenario)
  results$Dataset <- names(datasets)[i]
  
  # save results (rounded format) per dataset
 rounded_result_filename <- paste0(res, names(datasets)[i], "_rounded_results.csv")
  write.csv(results, file = rounded_result_filename, row.names = FALSE)
  
  # combine all results across all datasets
  combined_results <- rbind(combined_results, results)
  
  # print results that are statistically significant
  print(paste("Rows with p-value < 0.05 in", names(datasets)[i]))
  print(results[results$P_Value < 0.05, ])
}

# save the combined results (across all predictor variables and all datasets)
combined_results_filename <- paste0(res, "combined_results.csv")
write.csv(combined_results, file = combined_results_filename, row.names = FALSE)



