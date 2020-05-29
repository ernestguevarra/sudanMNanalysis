## Bootstrap anaemia indicators ################################################

## Subset indicators to 3 states data - State 1, 7, 13
subDF <- subset(indicators, stateID %in% c(1, 7, 13))

## Create anaemia indicator groups
subDF$indicatorGroup <- with(subDF, {
  ifelse(pregnant == 1, "pregnant",
    ifelse(pregnant == 2 & lactating == 2, "notPregnantNotLactating",
      ifelse(pregnant == 2 & lactating == 1, "notPregnantLactating", NA)))
})

## Get state names
stateNames <- unique(locNames$state[locNames$stateID %in% c(1, 7, 13)])

## indicator name vector
params <- c("iodine", "ID1A", "ID1B", "ID2", "ID3", "ID4", "ID5", "ID6")

## Create empty data.frame for concatenating boot results
#bootDF <- data.frame(matrix(nrow = 399, ncol = 36, byrow = TRUE))
bootDF <- data.frame(matrix(nrow = 9, ncol = 72, byrow = TRUE))

names(bootDF) <- c(paste(params, "pregnant", unique(locNames$state[locNames$stateID == 1]), sep = "_"),
                   paste(params, "notPregnantNotLactating", unique(locNames$state[locNames$stateID == 1]), sep = "_"),
                   paste(params, "notPregnantLactating", unique(locNames$state[locNames$stateID == 1]), sep = "_"),
                   paste(params, "pregnant", unique(locNames$state[locNames$stateID == 7]), sep = "_"),
                   paste(params, "notPregnantNotLactating", unique(locNames$state[locNames$stateID == 7]), sep = "_"),
                   paste(params, "notPregnantLactating", unique(locNames$state[locNames$stateID == 7]), sep = "_"),
                   paste(params, "pregnant", unique(locNames$state[locNames$stateID == 13]), sep = "_"),
                   paste(params, "notPregnantNotLactating", unique(locNames$state[locNames$stateID == 13]), sep = "_"),
                   paste(params, "notPregnantLactating", unique(locNames$state[locNames$stateID == 13]), sep = "_"))

## Cycle through states
for(i in sort(unique(subDF$stateID))) {
  ## Get current state name
  currentStateName <- unique(locNames$state[locNames$stateID == i])
  ## Cycle through grouping categrories
  for(j in c("pregnant", "notPregnantNotLactating", "notPregnantLactating")) {
    ## Subset to current grouping category
    currentGroup <- subset(subDF, stateID == i & indicatorGroup == j)
    ## Cycle through indicators
    for(k in params) {
      ## Create empty concatenating vector for current bootstrap outputs
      currentBoot <- NA
      ## Check if current group is not empty and then bootstrap
      if(nrow(currentGroup) > 0) {
        ## Boot
        currentBoot <- bootBW(x = currentGroup, 
                              w = psuData[psuData$psu %in% currentGroup$psu, ],
                              statistic = bootClassic, 
                              params = k,
                              outputColumns = paste(k, 
                                                    j, 
                                                    currentStateName, 
                                                    sep = "_"),
                              replicates = REPLICATES)
      }
      bootDF[[paste(k, j, currentStateName, sep = "_")]] <- currentBoot
    }
  }
}

## Get estimates and CIs
bootResults <- apply(X = bootDF, MARGIN = 2, 
                     FUN = quantile, 
                     probs = c(0.5, 0.025, 0.975), 
                     na.rm = TRUE)

## Convert output to long form
xx <- data.frame(t(bootResults))

## Rename rows
row.names(xx) <- 1:nrow(xx)

## Rename results
names(xx) <- c("estimate", "lcl", "ucl")

## Calculate number of indicator groups
nIndicatorGroups <- length(params) * length(unique(subDF$indicatorGroup[!is.na(subDF$indicatorGroup)]))
nStateGroups <- length(stateNames) * length(unique(subDF$indicatorGroup[!is.na(subDF$indicatorGroup)]))

## Create proper data.frames
State <- c(rep(stateNames[1], nIndicatorGroups),
           rep(stateNames[2], nIndicatorGroups),
           rep(stateNames[3], nIndicatorGroups))

Indicator <- c(paste("Pregnant: ", 
                     c("Mean urinary iodine concentration (microgram/L)", 
                       "Insufficient iodine", 
                       "Insufficient iodine", 
                       "Mild iodine deficiency",
                       "Moderate iodine deficiency",
                       "Severe iodine deficiency",
                       "Iodine above requirements",
                       "Iodine excessive"), 
                     sep = ""),
               paste("Non-pregnant non-lactating: ",
                     c("Mean urinary iodine concentration (microgram/L)", 
                       "Insufficient iodine", 
                       "Insufficient iodine", 
                       "Mild iodine deficiency",
                       "Moderate iodine deficiency",
                       "Severe iodine deficiency",
                       "Iodine above requirements",
                       "Iodine excessive"),
                     sep = ""),
               paste("Non-pregnant lactating: ",
                     c("Mean urinary iodine concentration (microgram/L)", 
                       "Insufficient iodine", 
                       "Insufficient iodine", 
                       "Mild iodine deficiency",
                       "Moderate iodine deficiency",
                       "Severe iodine deficiency",
                       "Iodine above requirements",
                       "Iodine excessive"),
                     sep = ""))

Type <- c(rep(c("Mean", "Proportion", "Proportion", "Proportion",
                "Proportion", "Proportion", "Proportion", "Proportion"), 
              nStateGroups))

iodineResults <- data.frame(State, Indicator, Type, xx)