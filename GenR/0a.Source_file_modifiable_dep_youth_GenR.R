#########################
# 0A. Source file GenR - paths and packages
#########################
# PROJECT: Modifiable factors internalizing problems in youth from general population and at risk 
# license CC 4.0 International
# DATA: Generation R data waves @9 and @13
# AIM OF SCRIPT:set paths and load packages for analyses
# AUTHOR: Lorenza Dall'Aglio (ldallaglio@mgh.harvard.edu; lorenza.dallaglio1@gmail.com)

# set directories

indata <- "PATH-WHERE-YOUR-DATA-IS"  

setwd(indata)

res <- "PATH-WHERE-YOU-WANT-YOUR-RESULTS"

fig <- "PATH-WHERE-YOU-WANT-YOUR-FIGURES"

tab <- "PATH-WHERE-YOU-WANT-YOUR-TABLES"

# load packages 

x <- c("dplyr", "data.table", 
       "ggplot2", "cowplot", "ggpubr",
       "tidyverse", "naniar", "gtsummary", "flextable", "RColorBrewer","lme4", 
       "lattice", "tidyr","raincloudplots", "devtools", "corrplot", "psych",
       "ggplotify", "magick", "lattice", "reshape2", "table1", "chatgpt", 
       "mice", "sjmisc", "data.table", "dplyr", "gtsummary", "boot")

lapply(x, require, character.only = T)


