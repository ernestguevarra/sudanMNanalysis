## Load required data ##########################################################
load("indicators.Rdata")

## Set bootstrap replicates ####################################################
REPLICATES <- 399

## State selection
STATES <- c(2, 8, 14)

source("sudanMNbootstrap.R")
source("anyAnaemiaBootstrap.R")

save(anaemiaResults, file = "anaemia.Rdata")