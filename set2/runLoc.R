## Load required data ##########################################################
load("indicators.Rdata")

## Set bootstrap replicates ####################################################
REPLICATES <- 399

## State selection
STATES <- c(2, 8, 14)

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
