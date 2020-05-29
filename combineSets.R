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

## Save per state result into a worksheet in a single workbook
allResults <- openxlsx::createWorkbook()

for(i in unique(stateResults$State)) {
  openxlsx::addWorksheet(wb = allResults, sheetName = i)
  openxlsx::writeData(wb = allResults, sheet = i, stateResults[stateResults$State == i, ])
}

openxlsx::saveWorkbook(allResults, file = "_byStatesMNresults.xlsx")
