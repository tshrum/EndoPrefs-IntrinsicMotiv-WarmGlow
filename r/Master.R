# Master Script
# Intrinsic vs. Extrinsic Motivation: September 2019 Study
# For public data and code repository
# Code written by Trisha Shrum & Roberta Molokandov
# Email tshrum@uvm.edu if you find any errors or have any questions.

#Clear workspace
rm(list=ls()) 

# Load packages
source('r/Load.R', echo=TRUE)

# Load functions
source('r/functions.R', echo=TRUE)

#### Data Prep using Identified Data ####
# Code left for transparency. Contact tshrum@uvm.edu if you would like access to the original files
# Run data prep scripts
# source('r/DataPrep/DataPrep_Part1.R')
# source('r/DataPrep/DataPrep_Part2.R')

# Retention/Attrition Analysis # NOTE: Relies on de-identified data, cannot run from public code
# source('r/Analysis/retentionAnalysis.R')

# Merge P1 and P2 datasets and deidentify
# source('r/DataPrep/merge.R')

# Saving deidentified Data
# write_csv(dAll, "data/deID.csv")
# save(dAll, file = "data/dAll.RData")
# save(dP1, file = "data/dP1.RData")
# save(dP2, file = "data/dP2.RData")

# Loading deidentified and prepared data files
load("data/dAll.RData")
load("data/dP1.RData")
load("data/dP2.RData")

# Removing respondents who completed survey in the lowest 2.5% of the sample
source('r/DataPrep/clean.R', echo=TRUE)

#### Analysis script ####
source('r/Analysis/endogenousPreferencesPaper.R')

#### Randomization Check ####
source('r/Analysis/randomizationCheck.R')






