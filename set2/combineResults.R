## Combine all
allResults <- data.frame(
  rbind(anaemiaResults, inflammationResults, ironResults,
        calciumResults, iodineResults, iodineResults2,
        iodineResults3, iodineResults4), stringsAsFactors = FALSE
)

## Save
save(allResults, file = "results.Rdata")


