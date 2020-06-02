## Load required data ##########################################################
load("indicators.Rdata")

## Set bootstrap replicates ####################################################
REPLICATES <- 399

## State selection
STATES <- c(1, 7, 13)

source("sudanMNbootstrap.R")
source("anyAnaemiaBootstrap.R")

save(anaemiaResults, file = "anaemia.Rdata")