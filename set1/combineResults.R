## Combine all
allResults <- data.frame(
  rbind(anaemiaResults, inflammationResults, ironResults,
        calciumResults, iodineResults), stringsAsFactors = FALSE
)

## Save
save(allResults, file = "results.Rdata")


