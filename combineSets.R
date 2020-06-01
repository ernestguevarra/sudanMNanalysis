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


results <- openxlsx::createWorkbook()

## Save per state result into a worksheet in a single workbook
for(i in unique(stateResults$State)) {
  currentStateResults <- subset(stateResults,
                                State == i,
                                select = c(-State, -sd))
  
  currentStateResults[!stringr::str_detect(currentStateResults$Indicator, "Median"), c("estimate", "lcl", "ucl")] <- round(currentStateResults[!stringr::str_detect(currentStateResults$Indicator, "Median"), c("estimate", "lcl", "ucl")] * 100, digits = 2)
  currentStateResults[stringr::str_detect(currentStateResults$Indicator, "Median"), c("estimate", "lcl", "ucl")] <- round(currentStateResults[stringr::str_detect(currentStateResults$Indicator, "Median"), c("estimate", "lcl", "ucl")], digits = 2)
  
  openxlsx::addWorksheet(wb = results, sheetName = i)
  openxlsx::writeData(wb = results, 
                      sheet = i, 
                      currentStateResults)
}

openxlsx::saveWorkbook(wb = results, file = "_byStates.xlsx", overwrite = TRUE)


