## Libraries
library(openxlsx)
library(stringr)

## Read results data
stateResults <- read.csv("_byStatesMNresults.csv", stringsAsFactors = FALSE)
statePops <- read.xlsx("_statePops.xlsx", sheet = 1)
statePops$state <- str_remove_all(string = statePops$state, pattern = " State")

indicatorBase <- stateResults[stateResults$State == "Northern", "Indicator"]
locNames <- read.csv("data/locNames.csv", stringsAsFactors = FALSE)

## Accumulator for pooled results
nationalResults <- NULL

## Cycle through states
for(i in 1:length(indicatorBase))
{
  estimates <- standardErrors <- weights <- NULL
  for(j in unique(stateResults$State)) 
  {
    ## Get estimates for current indicator, SE, and population weight in current state
    estimates <- c(estimates, stateResults[stateResults$State == j, "estimate"][i])
    standardErrors <- c(standardErrors, stateResults[stateResults$State == j, "sd"][i])
    ## Get weight for state
    weights <- c(weights, statePops$pop[statePops$state == j])
  }
  pooledEstimate <- sum(estimates * weights, na.rm = TRUE) / sum(weights, na.rm = TRUE)
  pooledSE  <- sqrt(sum(standardErrors^2 * weights / sum(weights, na.rm = TRUE), na.rm = TRUE))
    
  ## Correct for LCL less than 0
  pooledLCL <- pooledEstimate - 1.96 * pooledSE
  #pooledLCL <- ifelse(pooledLCL < 0 & pooledEstimate > 0.0001, 0.0001, pooledLCL)
  #pooledLCL <- ifelse(pooledLCL < 0 & pooledEstimate >= 0 & pooledEstimate < 0.0001, 0, pooledLCL)

  ## Correct for UCL greater than 1
  pooledUCL <- pooledEstimate + 1.96 * pooledSE
  #pooledUCL <- ifelse(pooledUCL > 1, 0.9999, pooledUCL)

  ## Convert proportion estimates to percentages and round off estimates to 2 decimal places
  #pooledEstimate <- round(pooledEstimate * 100, digits = 2)
  #pooledLCL <- round(pooledLCL * 100, digits = 2)
  #pooledUCL <- round(pooledUCL * 100, digits = 2)
    
  ## Make a results row
  resultRow <- c(pooledEstimate, pooledLCL, pooledUCL)
  nationalResults <- rbind(nationalResults, resultRow)
}

nationalResults <- data.frame(indicatorBase, nationalResults, stringsAsFactors = FALSE)
names(nationalResults) <- c("Indicator", "Estimate", "LCL", "UCL")
row.names(nationalResults) <- 1:nrow(nationalResults)

nationalResults$LCL <- ifelse(nationalResults$LCL < 0, 0.0001, nationalResults$LCL)

write.csv(nationalResults, "_nationalResults.csv", row.names = FALSE)

nationalResults[!str_detect(nationalResults$Indicator, pattern = "Median"), c("Estimate", "LCL", "UCL")] <- round(nationalResults[!str_detect(nationalResults$Indicator, pattern = "Median"), c("Estimate", "LCL", "UCL")] * 100, digits = 2)
nationalResults[str_detect(nationalResults$Indicator, pattern = "Median"), c("Estimate", "LCL", "UCL")] <- round(nationalResults[str_detect(nationalResults$Indicator, pattern = "Median"), c("Estimate", "LCL", "UCL")], digits = 2)

write.xlsx(x = nationalResults, file = "_nationalResults.xlsx")

