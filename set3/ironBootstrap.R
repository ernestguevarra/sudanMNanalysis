## Bootstrap iron indicators ################################################

## Subset indicators to 3 states data
subDF <- subset(indicators, stateID %in% STATES)

## Create anaemia indicator groups
subDF$indicatorGroup <- ifelse(subDF$ageGrp == 1, "child",
                               ifelse(subDF$pregnant == 1, "pregnant",
                                      ifelse(subDF$pregnant == 2, "notPregnant", NA)))

## Get state names
stateNames <- unique(locNames$state[locNames$stateID %in% STATES])

## indicator name vector
params <- c("adjFerritin", "IR1", "IR2", "IDA")

## Create empty data.frame for concatenating boot results
#bootDF <- data.frame(matrix(nrow = 399, ncol = 36, byrow = TRUE))
bootDF <- data.frame(matrix(nrow = REPLICATES, ncol = 36, byrow = TRUE))

names(bootDF) <- c(paste(params, "child", stateNames[1], sep = "_"),
                   paste(params, "pregnant", stateNames[1], sep = "_"),
                   paste(params, "notPregnant", stateNames[1], sep = "_"),
                   paste(params, "child", stateNames[2], sep = "_"),
                   paste(params, "pregnant", stateNames[2], sep = "_"),
                   paste(params, "notPregnant", stateNames[2], sep = "_"),
                   paste(params, "child", stateNames[3], sep = "_"),
                   paste(params, "pregnant", stateNames[3], sep = "_"),
                   paste(params, "notPregnant", stateNames[3], sep = "_"))

## Cycle through states
for(i in sort(unique(subDF$stateID))) {
  ## Get current state name
  currentStateName <- unique(locNames$state[locNames$stateID == i])
  ## Cycle through grouping categrories
  for(j in c("child", "pregnant", "notPregnant")) {
    ## Subset to current grouping category
    currentGroup <- subset(subDF, stateID == i & indicatorGroup == j)
    ## Cycle through indicators
    for(k in params) {
      ##
      cat("\n", unique(locNames$state[locNames$stateID == i]), " - ", j, " - ", k, "\n\n", sep = ""); flush.console()
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

## Get robust SD
bootSD <- apply(X = bootDF, MARGIN = 2, FUN = robustSD)

## Convert output to long form
xx <- data.frame(t(bootResults), bootSD)

## Rename rows
row.names(xx) <- 1:nrow(xx)

## Rename results
names(xx) <- c("estimate", "lcl", "ucl", "sd")

## Calculate number of indicator groups
nIndicatorGroups <- length(params) * length(unique(subDF$indicatorGroup[!is.na(subDF$indicatorGroup)]))
nStateGroups <- length(stateNames) * length(unique(subDF$indicatorGroup[!is.na(subDF$indicatorGroup)]))

## Create proper data.frames
State <- c(rep(stateNames[1], nIndicatorGroups),
           rep(stateNames[2], nIndicatorGroups),
           rep(stateNames[3], nIndicatorGroups))

Indicator <- c(paste("Child: ", 
                     c("Mean serum ferritin concentration (ng/mL)", 
                       "Iron deficiency", 
                       "Iron overload", 
                       "Iron deficiency anaemia"), 
                     sep = ""),
               paste("Pregnant: ",
                     c("Mean serum ferritin concentration (ng/mL)", 
                       "Iron deficiency", 
                       "Iron overload", 
                       "Iron deficiency anaemia"),
                     sep = ""),
               paste("Non-pregnant: ",
                     c("Mean serum ferritin concentration (ng/mL)", 
                       "Iron deficiency", 
                       "Iron overload", 
                       "Iron deficiency anaemia"),
                     sep = ""))

Type <- c(rep(c("Mean", "Proportion", "Proportion", "Proportion"), nStateGroups))

ironResults <- data.frame(State, Indicator, Type, xx)
