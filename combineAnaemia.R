## Concatenate allResults from the various sets

load("set1/anaemia.Rdata")
allAnaemia <- anaemiaResults

load("set2/anaemia.Rdata")
allAnaemia<- rbind(allAnaemia, anaemiaResults)

load("set3/anaemia.Rdata")
allAnaemia<- rbind(allAnaemia, anaemiaResults)

load("set4/anaemia.Rdata")
allAnaemia<- rbind(allAnaemia, anaemiaResults)

load("set5/anaemia.Rdata")
allAnaemia<- rbind(allAnaemia, anaemiaResults)

load("set6/anaemia.Rdata")
allAnaemia<- rbind(allAnaemia, anaemiaResults)

#stateResults <- stateResults[!is.na(stateResults$estimate), ]

write.csv(allAnaemia, "_anaemiaResults.csv", row.names = FALSE)


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


