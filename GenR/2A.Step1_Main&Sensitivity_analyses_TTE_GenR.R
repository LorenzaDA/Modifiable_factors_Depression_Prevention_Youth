############
# 1. Step1: prospective associations GenR 
############
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: Generation R data waves @9 and @13
# AIM OF SCRIPT: Observed associations between each modifiable factors and internalizing problems across all prevention settings + sensitivity analyses
# Sensitivity analyses included: 
# 1. test negative control (IQ)
# 2. run adjustments for internalizing at 6 years (instead of 10 years - baseline) to evaluate whether results may have been overadjusted and we might have introduced bias in our results
# 3. run adjustments for screen time at 6 years to evalaute whether screen time results are dependent on prior levels of screen time or not
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)


rm(list=ls())
source("/PATH-WHERE-SOURCE-FILE-IS/0a.Source_file_modifiable_dep_youth_GenR.R")
source("/PATH-WHERE-SOURCE-FILE-IS/0b.Source_file_functions_modifiable_dep_youth_GenR.R")
load(paste0(indata, "/", "modifiable_dep_youth_alldata_genr.RData"))

# covariates
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "income", "mat_age", "int_t1")

# set datasets
datasets <- list(dd4, hGr, hEr, indicated)
names(datasets) <- c("universal", "selective_genetics", "selective_trauma",
                     "indicated")


# modifiable factors
pred_vars <- c("pa", "friends", "sleep", "splines::ns(screen,2)")

# Initialize a list to store results for each dataset
all_results <- list()

# Initialize a dataframe to store combined results
combined_results <- data.frame(Variable = character(),
                               Coefficient = numeric(),
                               Standard_Error = numeric(),
                               P_Value = numeric(),
                               Dataset = character(),
                               row.names = NULL)

# loop through each dataset
for (i in seq_along(datasets)) {
  #  regression analysis function to the current dataset for a given modifiable factor
  results <- perform_regression(datasets[[i]], baselinevars, pred_vars)
  
  # store coefficient SE and p val info (rounded to 3 decimals)
  results[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results[, c("Coefficient", "Standard_Error", "P_Value")], 3)
  
  # add a column to store the dataset name (universal, selective genetic, selective trauma, indicated)
  results$Dataset <- names(datasets)[i]
  
  # add the results to the combined dataframe
  combined_results <- rbind(combined_results, results)
  
  # print rows where the p-value is less than 0.05
  print(paste("Rows with p-value < 0.05 in", names(datasets)[i]))
  print(results[results$P_Value < 0.05, ])
}

# save the combined results to a CSV file
combined_results_filename <- paste0(res, "combined_results_GenR_July2024_FINAL.csv")
write.csv(combined_results, file = combined_results_filename, row.names = FALSE)


#### sensitivity analysis with IQ as negative control ####

reg_pa <- lm(iq_t2 ~ iq_t1 + ethn + pa + sex + puberty + age + parent_edu + par_psych + income + mat_age, data = dd4)

reg_screen <- lm(iq_t2 ~ iq_t1 + ethn + screen + sex + puberty + age + parent_edu + par_psych + income + mat_age, data = dd4)

reg_sleep <- lm(iq_t2 ~ iq_t1 + ethn + sleep + sex  + puberty + age + parent_edu + par_psych + income + mat_age, data = dd4)

reg_friends <- lm(iq_t2 ~ iq_t1 + ethn + friends + sex + puberty + age + parent_edu + par_psych + income + mat_age, data = dd4)



### sensitivity analysis - adj for age 6 internalizing instead of age 10 years ###

combined_results <- data.frame(Variable = character(),
                               Coefficient = numeric(),
                               Standard_Error = numeric(),
                               P_Value = numeric(),
                               Dataset = character(),
                               row.names = NULL)


# changed the int_t1 (age 10 years) for the int_5 (age 6 years)
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "income", "mat_age", "int_5")

for (i in seq_along(datasets)) {
  results <- perform_regression(datasets[[i]], baselinevars, pred_vars)
  results[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results[, c("Coefficient", "Standard_Error", "P_Value")], 3)
  results$Dataset <- names(datasets)[i]
  combined_results <- rbind(combined_results, results)
  print(paste("Rows with p-value < 0.05 in", names(datasets)[i]))
  print(results[results$P_Value < 0.05, ])
}

combined_results_filename <- paste0(res, "combined_results_GenR_July2024_ADJ_INT5.csv")
write.csv(combined_results, file = combined_results_filename, row.names = FALSE)



### sensitivity - adjustments for prior screen time use ###
# this is just for screen time to evaluate whether the estimate observed for screen time
# with internalizing is given from screen time use at age 10 years or earlier screen time use

# df to store results
combined_results <- data.frame(Variable = character(),
                               Coefficient = numeric(),
                               Standard_Error = numeric(),
                               P_Value = numeric(),
                               Dataset = character(),
                               row.names = NULL)

# now the covariate list includes screen time at 5 years 
baselinevars <- c("sex", "puberty", "age", "parent_edu", "par_psych", "income", "mat_age", "int_t1", "screen_5")

# main predictor = screen
pred_vars <- c("splines::ns(screen,2)")

# analyses
for (i in seq_along(datasets)) {
  results <- perform_regression(datasets[[i]], baselinevars, pred_vars)
  results[, c("Coefficient", "Standard_Error", "P_Value")] <- round(results[, c("Coefficient", "Standard_Error", "P_Value")], 3)
  results$Dataset <- names(datasets)[i]
  combined_results <- rbind(combined_results, results)
  print(paste("Rows with p-value < 0.05 in", names(datasets)[i]))
  print(results[results$P_Value < 0.05, ])
}

# save
combined_results_filename <- paste0(res, "combined_results_GenR_July2024_ADJ_SCREEN.csv")
write.csv(combined_results, file = combined_results_filename, row.names = FALSE)

