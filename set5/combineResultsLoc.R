allResults <- data.frame(
  rbind(anaemiaResults, inflammationResults, ironResults,
        calciumResults, iodineResults1, iodineResults2,
        iodineResults3, iodineResults4), stringsAsFactors = FALSE
)

save(allResults, file = "results.Rdata")



