## Bootstrap anaemia indicators ################################################

## Subset indicators to 3 states data
subDF <- subset(indicators, stateID %in% STATES)

## Create anaemia indicator groups
subDF$indicatorGroup <- with(subDF, {
  ifelse(pregnant == 1, "Pregnant",
    ifelse(pregnant == 2 & lactating == 2, "Non-pregnant non-lactating",
      ifelse(pregnant == 2 & lactating == 1, "Non-pregnant lactating", NA)))
})

## Get state names
stateNames <- unique(locNames$state[locNames$stateID %in% STATES])

## indicator name vector
params <- c("iodine")

## Create empty data.frame for concatenating boot results
bootDF <- data.frame(matrix(nrow = REPLICATES, 
                            ncol = length(params) * length(stateNames) * 3, 
                            byrow = TRUE))

bootDFnames <- NULL

for(i in stateNames) {
  for(j in c("Pregnant", "Non-pregnant non-lactating", "Non-pregnant lactating")) {
    for(k in params) {
      bootDFnames <- c(bootDFnames, paste(i, j, k, sep = "_"))
    }
  }
}

## rename bootDF
names(bootDF) <- bootDFnames

## Cycle through states
for(i in sort(unique(subDF$stateID))) {
  ## Get current state name
  currentStateName <- unique(locNames$state[locNames$stateID == i])
  ## Cycle through grouping categrories
  for(j in c("Pregnant", "Non-pregnant non-lactating", "Non-pregnant lactating")) {
    ## Subset to current grouping category
    currentGroup <- subset(subDF, stateID == i & indicatorGroup == j)
    ## Cycle through indicators
    for(k in params) {
      ##
      cat("\n", currentStateName, " - ", j, " - ", k, "\n\n", sep = ""); flush.console()
      ## Create empty concatenating vector for current bootstrap outputs
      currentBoot <- NA
      ## Check if current group is not empty and then bootstrap
      if(nrow(currentGroup) > 0) {
        ## Boot
        currentBoot <- bootBW(x = currentGroup, 
                              w = psuData[psuData$psu %in% currentGroup$psu, ],
                              statistic = ifelse(k %in% c("adjHb", 
                                                          "adjFerritin", 
                                                          "crp", 
                                                          "calcium", 
                                                          "iodine"), bootMedian, bootClassic), 
                              params = k,
                              outputColumns = paste(currentStateName, 
                                                    j,
                                                    k, 
                                                    sep = "_"),
                              replicates = REPLICATES)
      }
      bootDF[[paste(currentStateName, j, k, sep = "_")]] <- currentBoot
    }
  }
}

## Get estimates and CIs
bootResults <- apply(X = bootDF, MARGIN = 2, 
                     FUN = quantile, 
                     probs = c(0.5, 0.025, 0.975), 
                     na.rm = TRUE)

## Get robust SD
bootSD <- apply(X = bootDF, MARGIN = 2, FUN = robustSD)

## Convert output to long form
xx <- data.frame(t(bootResults), bootSD)

## Rename results
names(xx) <- c("estimate", "lcl", "ucl", "sd")

## Get admin and identifying data
yy <- stringr::str_split(string = row.names(xx), pattern = "_", simplify = TRUE)

iodineResults <- data.frame(State = yy[ , 1],
                            Indicator = paste(yy[ , 2], "Median urinary iodine concentration (microgram/L)", sep = ": "),
                            xx,
                            row.names = NULL,
                            stringsAsFactors = FALSE)
