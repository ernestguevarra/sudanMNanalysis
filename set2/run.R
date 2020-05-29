## Load required data ##########################################################
load("indicators.Rdata")

## Set bootstrap replicates ####################################################
REPLICATES <- 399

## State selection
STATES <- c(2, 8, 14)

source("sudanMNbootstrap.R")
source("anaemiaBootstrap.R")
source("inflammationBootstrap.R")
source("ironBootstrap.R")
source("calciumBootstrap.R")
source("iodineBootstrap.R")
source("combineResults.R")
