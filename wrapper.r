# combining the extreme limits analyses into a wrapper that we can run with multiple hummingbird species

#load the libraries
library(lattice)
library(latticeExtra)
library(ggplot2)

#set working directory
wd = "/Users/sarah/Documents/GitHub/extreme_limits/"
datapath = "/Users/sarah/Desktop/Dropbox/ActiveResearchProjects/Hummingbird_extreme_limits/"
setwd(wd)

#load the RData
load(paste(datapath, "yearly.catch.stat.rdata", sep=""))
load(paste(datapath, "yearly.climate.rdata", sep=""))

#source the files that contain functions for the analysis
source('Model_evaluation_v4.r')
#source('data_prep_for_transfer_PSAB_only.r')
#source('Plot_conditional_effects_interaction_models.r')

#next steps...
