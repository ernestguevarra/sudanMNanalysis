allResults <- data.frame(
  rbind(anaemiaResults, inflammationResults, ironResults,
        calciumResults, iodineResults), stringsAsFactors = FALSE
)

save(allResults, file = "results.Rdata")



