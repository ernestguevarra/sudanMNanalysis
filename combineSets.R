## Concatenate allResults from the various sets

load("set1/results.Rdata")
stateResults <- allResults

load("set2/results.Rdata")
stateResults <- rbind(stateResults, allResults)

load("set3/results.Rdata")
stateResults <- rbind(stateResults, allResults)

load("set4/results.Rdata")
stateResults <- rbind(stateResults, allResults)

load("set5/results.Rdata")
stateResults <- rbind(stateResults, allResults)

load("set6/results.Rdata")
stateResults <- rbind(stateResults, allResults)

#stateResults <- stateResults[!is.na(stateResults$estimate), ]

write.csv(stateResults, "_byStatesMNresults.csv", row.names = FALSE)

## Save per state result into a worksheet in a single workbook
for(i in unique(stateResults$State)) {
  results <- openxlsx::createWorkbook()
  for(j in unique(stateResults$Locality[stateResults$State == i])) {
    openxlsx::addWorksheet(wb = results, sheetName = j)
    
    currentLocalityResults <- subset(stateResults,
                                    State == i & Locality == j,
                                    select = c(-State, -Locality, -sd))
    
    currentLocalityResults[ , c("estimate", "lcl", "ucl")] <- round(currentLocalityResults[ , c("estimate", "lcl", "ucl")] * 100, digits = 2)
    
    openxlsx::writeData(wb = results, 
                        sheet = j, 
                        currentLocalityResults)
  }
  openxlsx::saveWorkbook(wb = results, 
                         file = paste("localityResults/_", i, ".xlsx", sep = "", collapse = " "), 
                         overwrite = TRUE)
}


