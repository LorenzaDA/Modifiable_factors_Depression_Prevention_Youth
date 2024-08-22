################
# 0A. source file ABCD
################

# set directories

indata <- "WHERE-YOUR-DATA-IS"  

setwd(indata)

fig <- "WHERE-YOU-WANT-YOUR-FIGURES"

tab <- "WHERE-YOU-WANT-YOUR-TABLES"

res <- "WHERE-YOU-WANT-YOUR-RESULTS"

# load packages 

x <- c("dplyr", "data.table", 
       "ggplot2", "cowplot", "ggpubr",
       "tidyverse", "naniar", "gtsummary", "flextable", "RColorBrewer","lme4", 
       "lattice", "tidyr","raincloudplots", "devtools", "corrplot", "psych",
        "ggplotify", "magick", "lattice", "reshape2", "table1", "chatgpt", 
       "mice", "sjmisc", "data.table", "dplyr", "gtsummary", "boot")


lapply(x, require, character.only = T)





