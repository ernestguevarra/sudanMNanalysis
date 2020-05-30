## Bootstrap anaemia indicators ################################################

## Subset indicators to 3 states data
subDF <- subset(indicators, stateID %in% STATES)

## Create anaemia indicator groups
subDF$indicatorGroup <- ifelse(subDF$ageGrp == 1, "Child",
                               ifelse(subDF$pregnant == 1, "Pregnant",
                                      ifelse(subDF$pregnant == 2, "Non-pregnant", NA)))

## Get state names
stateNames <- unique(locNames$state[locNames$stateID %in% STATES])

## indicator name vector
params <- c("AN1", "AN2", "AN3")

## Create empty data.frame for concatenating boot results
bootDF <- data.frame(matrix(nrow = REPLICATES, 
                            ncol = length(params) * length(locNames$locality[locNames$stateID %in% STATES]) * 3, 
                            byrow = TRUE))

bootDFnames <- NULL

for(i in stateNames) {
  for(j in unique(locNames$locality[locNames$state == i])) {
    for(k in c("Child", "Pregnant", "Non-pregnant")) {
      for(l in params) {
        bootDFnames <- c(bootDFnames, paste(i, j, k, l, sep = "_"))
      }
    }
  }
}

names(bootDF) <- bootDFnames

## Cycle through states
for(i in sort(unique(subDF$localityID))) {
  ## Get current state name
  currentStateName <- unique(locNames$state[locNames$localityID == i])
  ## Get current locality name
  currentLocalityName <- unique(locNames$locality[locNames$localityID == i])
  ## Cycle through grouping categrories
  for(j in c("Child", "Pregnant", "Non-pregnant")) {
    ## Subset to current grouping category
    currentGroup <- subset(subDF, localityID == i & indicatorGroup == j)
    ## Cycle through indicators
    for(k in params) {
      cat("\n", 
          currentStateName, " - ", 
          currentLocalityName, " - ", 
          j, " - ",
          k, "\n\n", sep = ""); flush.console()
      ## Create empty concatenating vector for current bootstrap outputs
      currentBoot <- NA
      ## Check if current group is not empty and then bootstrap
      if(nrow(currentGroup) > 0) {
        ## Boot
        currentBoot <- bootBW(x = currentGroup, 
                              w = psuData[psuData$psu %in% currentGroup$psu, ],
                              statistic = bootClassic, 
                              params = k,
                              outputColumns = paste(currentStateName,
                                                    currentLocalityName,
                                                    j,
                                                    k,
                                                    sep = "_"),
                              replicates = REPLICATES)
      }
      bootDF[[paste(currentStateName, currentLocalityName, j, k, sep = "_")]] <- currentBoot
    }
  }
}

## Get estimates and CIs
bootResults <- apply(X = bootDF, MARGIN = 2, 
                     FUN = quantile, 
                     probs = c(0.5, 0.025, 0.975), 
                     na.rm = TRUE)

bootSD <- apply(X = bootDF, MARGIN = 2, FUN = robustSD)

## Convert output to long form
xx <- data.frame(t(bootResults), bootSD)

## Rename rows
#row.names(xx) <- 1:nrow(xx)

## Rename results
names(xx) <- c("estimate", "lcl", "ucl", "sd")

## Get admin and identifying data
yy <- stringr::str_split(string = row.names(xx), pattern = "_", simplify = TRUE)

indicatorName <- ifelse(yy[ , 4] == "AN1", "Mild anaemia",
                   ifelse(yy[ , 4] == "AN2", "Moderate anaemia", "Severe Anaemia"))

anaemiaResults <- data.frame(State = yy[ , 1],
                             Locality = yy[ , 2],
                             Indicator = paste(yy[ , 3], indicatorName, sep = ": "),
                             xx,
                             row.names = NULL,
                             stringsAsFactors = FALSE)

