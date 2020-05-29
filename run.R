## Load required data ##########################################################
load("indicators.Rdata")

## Set bootstrap replicates ####################################################
REPLICATES <- 9

## State selection
STATES <- c(1, 7, 13)

source("sudanMNbootstrap.R")
source("anaemiaBootstrap.R")
source("inflammationBootstrap.R")
source("ironBootstrap.R")
source("calciumBootstrap.R")
source("iodineBootstrap.R")
source("combineResults.R")
