## Load required data ##########################################################
load("indicators.Rdata")

## Set bootstrap replicates ####################################################
REPLICATES <- 9

## State selection
STATES <- c(1, 7, 13)

source("sudanMNbootstrap.R")
source("anaemiaBootstrapLoc.R")
source("inflammationBootstrapLOc.R")
source("ironBootstrapLoc.R")
source("calciumBootstrapLoc.R")
source("iodineBootstrapLoc.R")
source("iodineBootstrapLoc2.R")
source("iodineBootstrapLoc3.R")
source("iodineBootstrapLoc4.R")
source("combineResultsLoc.R")
