## Libraries
library(openxlsx)
library(stringr)

## Read results data
localityResults <- read.csv("_byStatesMNresults.csv", stringsAsFactors = FALSE)

localityResults <- localityResults[!localityResults$Locality %in% c("Heiban", "El Buram", "Um Durein"), ]

## Locality populations for "post-stratification"
localityPops <- read.csv("data/localityPops.csv", stringsAsFactors = FALSE)
## Remove three localities not surveyed
localityPops <- localityPops[!localityPops$locality %in% c("Umdoren", 
                                                           "Alburam", 
                                                           "Heban"), ]

indicatorBase <- localityResults[localityResults$State == "Northern" & localityResults$Locality == "Dongola", "Indicator"]
locNames <- read.csv("data/locNames.csv", stringsAsFactors = FALSE)

## Accumulator for pooled results
allResults <- NULL
resultsWB <- createWorkbook()

## Cycle through states
#for(i in 1:nrow(indicatorBase))
for(i in unique(localityResults$State))
{
  ## current state accumulator for pooled results
  stateResults <- NULL
  ##
  addWorksheet(wb = resultsWB, sheetName = i)
  ## Cycle through rows of indicators
  for(j in 1:length(indicatorBase)) 
  {
    ## Get current state
    currentState <- subset(localityResults, State == i)
    ## Cycle through localities in current state
    estimates <- standardErrors <- weights <- NULL
    for(k in unique(currentState$Locality))
    {
      ## Get estimates for current indicator, SE, and population weight in each
      ## locality in current state
      estimates <- c(estimates, currentState[currentState$Locality == k, "estimate"][j])
      standardErrors <- c(standardErrors, currentState[currentState$Locality == k, "sd"][j])
      ## Get weight for state
      weights <- c(weights, localityPops$pop[localityPops$state == i & localityPops$locality == k])
    }
    pooledEstimate <- sum(estimates * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
    pooledSE  <- sqrt(sum(standardErrors^2 * weights / sum(weights, na.rm = TRUE), na.rm = TRUE))
    
    ## Correct for LCL less than 0
    pooledLCL <- pooledEstimate - 1.96 * pooledSE
    pooledLCL <- ifelse(pooledLCL < 0 & pooledEstimate > 0.0001, 0.0001, pooledLCL)
    pooledLCL <- ifelse(pooledLCL < 0 & pooledEstimate >= 0 & pooledEstimate < 0.0001, 0, pooledLCL)

    ## Correct for UCL greater than 1
    pooledUCL <- pooledEstimate + 1.96 * pooledSE
    pooledUCL <- ifelse(pooledUCL > 1, 0.9999, pooledUCL)

    ## Convert proportion estimates to percentages and round off estimates to 2 decimal places
    pooledEstimate <- round(pooledEstimate * 100, digits = 2)
    pooledLCL <- round(pooledLCL * 100, digits = 2)
    pooledUCL <- round(pooledUCL * 100, digits = 2)
    
    ## Make a results row
    resultRow <- c(pooledEstimate, pooledLCL, pooledUCL)
    stateResults <- rbind(stateResults, resultRow)
  }
  stateResults <- data.frame(indicatorBase, "Proportion", stateResults, stringsAsFactors = FALSE)
  names(stateResults) <- c("Indicator", "Type", "Estimate", "LCL", "UCL")
  row.names(stateResults) <- 1:nrow(stateResults)
  writeData(wb = resultsWB, sheet = i, x = stateResults)
  stateResults <- data.frame(State = i, stateResults, stringsAsFactors = FALSE)
  allResults <- rbind(allResults, stateResults)
}

write.csv(allResults, "_stateResults.csv", row.names = FALSE)
saveWorkbook(wb = resultsWB, file = "_byStates.xlsx", overwrite = TRUE)

######################### Perform national estimation ##########################

## Accumulator for pooled results
nationalResults <- NULL
resultsWB <- createWorkbook()
addWorksheet(wb = resultsWB, sheetName = "national")

## Cycle through states
for(i in 1:length(indicatorBase))
{
  estimates <- standardErrors <- weights <- NULL
  for(j in unique(localityResults$State)) {
    ##
    currentState <- subset(localityResults, State == j)
    ## Cycle through localities in current state
    for(k in unique(currentState$Locality))
      {
      ## Get estimates for current indicator, SE, and population weight in each
      ## locality in current state
      estimates <- c(estimates, currentState[currentState$Locality == k, ]$estimate[i])
      standardErrors <- c(standardErrors, currentState[currentState$Locality == k, ]$sd[i])
      ## Get weight for state
      weights <- c(weights, localityPops$pop[localityPops$state == j & localityPops$locality == k])
    }
  }
  pooledEstimate <- sum(estimates * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  pooledSE  <- sqrt(sum(standardErrors^2 * weights / sum(weights, na.rm = TRUE), na.rm = TRUE))
    
  ## Correct for LCL less than 0
  pooledLCL <- pooledEstimate - 1.96 * pooledSE
  pooledLCL <- ifelse(pooledLCL < 0 & pooledEstimate > 0.0001, 0.0001, pooledLCL)
  pooledLCL <- ifelse(pooledLCL < 0 & pooledEstimate >= 0 & pooledEstimate < 0.0001, 0, pooledLCL)
    
  ## Correct for UCL greater than 1
  pooledUCL <- pooledEstimate + 1.96 * pooledSE
  pooledUCL <- ifelse(pooledUCL > 1, 0.9999, pooledUCL)
    
  ## Convert proportion estimates to percentages and round off estimates to 2 decimal places
  pooledEstimate <- round(pooledEstimate * 100, digits = 2)
  pooledLCL <- round(pooledLCL * 100, digits = 2)
  pooledUCL <- round(pooledUCL * 100, digits = 2)

  ## Make a results row
  resultRow <- c(pooledEstimate, pooledLCL, pooledUCL)
  nationalResults <- rbind(nationalResults, resultRow)
}

nationalResults <- data.frame(indicatorBase, "Proportion", nationalResults)
names(nationalResults) <- c("Indicator", "Type", "Estimate", "LCL", "UCL")
row.names(nationalResults) <- 1:nrow(nationalResults)

writeData(wb = resultsWB, sheet = "national", x = nationalResults)

write.csv(nationalResults, "_nationalResults.csv", row.names = FALSE)
saveWorkbook(wb = resultsWB, file = "_national.xlsx", overwrite = TRUE)
