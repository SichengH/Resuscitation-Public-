library(data.table)
library(ggplot2)
library(dplyr)
library(boot) 
library(table1)
'%!in%' <- function(x,y)!('%in%'(x,y))

# Takes the cohort from MIMIC-IV and MIMIC-III(not yet ready). 
# Make them into analysis ready format and storage in the public repo.

setwd("/Users/sichenghao/Documents/GitHub/Resuscitation-CHF-ESRD/Data(MIMIC)/")
cohort <- fread("cohort_IV.csv")

cohort_nondup <- cohort %>% filter(subgroup %in% c("CHF", "ESRD", "Sepsis Only"))
cohort_dup1 <- cohort %>% filter(subgroup == "CHF+ESRD") %>% mutate(subgroup = "CHF")
cohort_dup2 <- cohort %>% filter(subgroup == "CHF+ESRD") %>% mutate(subgroup = "ESRD")
cohort_cov <- rbind(cohort_nondup, cohort_dup1)
cohort_cov <- rbind(cohort_cov, cohort_dup2)

setwd("/Users/sichenghao/Documents/GitHub/Resuscitation-Public-/")
fwrite(cohort,"cohort.csv")
