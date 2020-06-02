## Libraries
library(openxlsx)
library(magrittr)
library(stringr)
library(dplyr)

## Read data
stateResults <- read.csv("_byStatesMNresults.csv", stringsAsFactors = FALSE)
stateResults$Indicator <- stateResults$Indicator %>%
  str_replace_all(pattern = "Anaemia", replacement = "anaemia")

stateResults[str_detect(stateResults$Indicator, "Median"), c("estimate", "lcl", "ucl")] <- round(stateResults[str_detect(stateResults$Indicator, "Median"), c("estimate", "lcl", "ucl")], digits = 2)
stateResults[!str_detect(stateResults$Indicator, "Median"), c("estimate", "lcl", "ucl")] <- round(stateResults[!str_detect(stateResults$Indicator, "Median"), c("estimate", "lcl", "ucl")] * 100, digits = 2)

nationalResults <- read.csv("_nationalResults.csv", stringsAsFactors = FALSE)
nationalResults$Indicator <- nationalResults$Indicator %>%
  str_replace_all(pattern = "Anaemia", replacement = "anaemia")

nationalResults[str_detect(nationalResults$Indicator, "Median"), c("Estimate", "LCL", "UCL")] <- round(nationalResults[str_detect(nationalResults$Indicator, "Median"), c("Estimate", "LCL", "UCL")], digits = 2)
nationalResults[!str_detect(nationalResults$Indicator, "Median"), c("Estimate", "LCL", "UCL")] <- round(nationalResults[!str_detect(nationalResults$Indicator, "Median"), c("Estimate", "LCL", "UCL")] * 100, digits = 2)

##
reportTables <- createWorkbook()

## Create children tables ######################################################

## Anaemia
childAnaemia <- data.frame(stateResults %>%
                             filter(Indicator == "Child: Median adjusted serum haemoglobin concentration (g/dL)") %>%
                             dplyr::select(State, estimate, lcl, ucl) %>%
                             mutate(),
                           stateResults %>%
                             filter(Indicator == "Child: Mild anaemia") %>%
                             dplyr::select(estimate, lcl, ucl),
                           stateResults %>%
                             filter(Indicator == "Child: Moderate anaemia") %>%
                             dplyr::select(estimate, lcl, ucl),
                           stateResults %>%
                             filter(Indicator == "Child: Severe anaemia") %>%
                             dplyr::select(estimate, lcl, ucl),
                           stringsAsFactors = FALSE)

childAnaemia <- childAnaemia[order(childAnaemia$State), ]

childAnaemia <- rbind(childAnaemia,
                      unlist(
                        c(State = 1,
                          nationalResults[nationalResults$Indicator == "Child: Median adjusted serum haemoglobin concentration (g/dL)", c("Estimate", "LCL", "UCL")],
                          nationalResults[nationalResults$Indicator == "Child: Mild anaemia", c("Estimate", "LCL", "UCL")],
                          nationalResults[nationalResults$Indicator == "Child: Moderate anaemia", c("Estimate", "LCL", "UCL")],
                          nationalResults[nationalResults$Indicator == "Child: Severe anaemia", c("Estimate", "LCL", "UCL")])))

childAnaemia[19, "State"] <- "National"

names(childAnaemia) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 4))

addWorksheet(wb = reportTables, sheetName = "childAnaemia")
writeData(wb = reportTables, sheet = "childAnaemia", x = childAnaemia)

## Iron
childIron <- data.frame(stateResults %>%
                          filter(Indicator == "Child: Median adjusted serum ferritin concentration (ng/mL)") %>%
                          dplyr::select(State, estimate, lcl, ucl),
                        stateResults %>%
                          filter(Indicator == "Child: Iron deficiency") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stateResults %>%
                          filter(Indicator == "Child: Iron deficiency anaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stringsAsFactors = FALSE)

childIron <- childIron[order(childIron$State), ]

childIron <- rbind(childIron,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Child: Median adjusted serum ferritin concentration (ng/mL)", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Child: Iron deficiency", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Child: Iron deficiency anaemia", c("Estimate", "LCL", "UCL")])))

childIron[19, "State"] <- "National"

names(childIron) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 3))

addWorksheet(wb = reportTables, sheetName = "childIron")
writeData(wb = reportTables, sheet = "childIron", x = childIron)

## Acute inflammation
childInflammation <- data.frame(stateResults %>%
                                  filter(Indicator == "Child: Median serum c-reactive protein concentration (mg/L)") %>%
                                  dplyr::select(State, estimate, lcl, ucl),
                                stateResults %>%
                                  filter(Indicator == "Child: Acute inflammation") %>%
                                  dplyr::select(estimate, lcl, ucl),
                                stringsAsFactors = FALSE)

childInflammation <- childInflammation[order(childInflammation$State), ]

childInflammation <- rbind(childInflammation,
                           unlist(
                             c(State = 1,
                               nationalResults[nationalResults$Indicator == "Child: Median serum c-reactive protein concentration (mg/L)", c("Estimate", "LCL", "UCL")],
                               nationalResults[nationalResults$Indicator == "Child: Acute inflammation", c("Estimate", "LCL", "UCL")])))

childInflammation[19, "State"] <- "National"

names(childInflammation) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 2))

addWorksheet(wb = reportTables, sheetName = "childInflammation")
writeData(wb = reportTables, sheet = "childInflammation", x = childInflammation)

## Calcium
childCalcium <- data.frame(stateResults %>%
                             filter(Indicator == "Child: Median serum calcium concentration (mg/dL)") %>%
                             dplyr::select(State, estimate, lcl, ucl),
                           stateResults %>%
                             filter(Indicator == "Child: Hypocalcaemia") %>%
                             dplyr::select(estimate, lcl, ucl),
                           stateResults %>%
                             filter(Indicator == "Child: Hypercalcaemia") %>%
                             dplyr::select(estimate, lcl, ucl),
                           stringsAsFactors = FALSE)

childCalcium <- childCalcium[order(childCalcium$State), ]

childCalcium <- rbind(childCalcium,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Child: Median serum calcium concentration (mg/dL)", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Child: Hypocalcaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Child: Hypercalcaemia", c("Estimate", "LCL", "UCL")])))

childCalcium[19, "State"] <- "National"

names(childCalcium) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 3))

addWorksheet(wb = reportTables, sheetName = "childCalcium")
writeData(wb = reportTables, sheet = "childCalcium", x = childCalcium)


## Create non-pregnant tables ##################################################

## Anaemia
npAnaemia <- data.frame(stateResults %>%
                          filter(Indicator == "Non-pregnant: Median adjusted serum haemoglobin concentration (g/dL)") %>%
                          dplyr::select(State, estimate, lcl, ucl),
                        stateResults %>%
                          filter(Indicator == "Non-pregnant: Mild anaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stateResults %>%
                          filter(Indicator == "Non-pregnant: Moderate anaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stateResults %>%
                          filter(Indicator == "Non-pregnant: Severe anaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stringsAsFactors = FALSE)

npAnaemia <- npAnaemia[order(npAnaemia$State), ]

npAnaemia <- rbind(npAnaemia,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Median adjusted serum haemoglobin concentration (g/dL)", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Mild anaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Moderate anaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Severe anaemia", c("Estimate", "LCL", "UCL")])))

npAnaemia[19, "State"] <- "National"

names(npAnaemia) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 4))

addWorksheet(wb = reportTables, sheetName = "npAnaemia")
writeData(wb = reportTables, sheet = "npAnaemia", x = npAnaemia)

## Iron
npIron <- data.frame(stateResults %>%
                       filter(Indicator == "Non-pregnant: Median adjusted serum ferritin concentration (ng/mL)") %>%
                       dplyr::select(State, estimate, lcl, ucl),
                     stateResults %>%
                       filter(Indicator == "Non-pregnant: Iron deficiency") %>%
                       dplyr::select(estimate, lcl, ucl),
                     stateResults %>%
                       filter(Indicator == "Non-pregnant: Iron deficiency anaemia") %>%
                       dplyr::select(estimate, lcl, ucl),
                     stringsAsFactors = FALSE)

npIron <- npIron[order(npIron$State), ]

npIron <- rbind(npIron,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Median adjusted serum ferritin concentration (ng/mL)", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Iron deficiency", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Iron deficiency anaemia", c("Estimate", "LCL", "UCL")])))

npIron[19, "State"] <- "National"

names(npIron) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 3))

addWorksheet(wb = reportTables, sheetName = "npIron")
writeData(wb = reportTables, sheet = "npIron", x = npIron)

## Acute inflammation
npInflammation <- data.frame(stateResults %>%
                               filter(Indicator == "Non-pregnant: Median serum c-reactive protein concentration (mg/L)") %>%
                               dplyr::select(State, estimate, lcl, ucl),
                             stateResults %>%
                               filter(Indicator == "Non-pregnant: Acute inflammation") %>%
                               dplyr::select(estimate, lcl, ucl),
                             stringsAsFactors = FALSE)

npInflammation <- npInflammation[order(npInflammation$State), ]

npInflammation <- rbind(npInflammation,
                           unlist(
                             c(State = 1,
                               nationalResults[nationalResults$Indicator == "Non-pregnant: Median serum c-reactive protein concentration (mg/L)", c("Estimate", "LCL", "UCL")],
                               nationalResults[nationalResults$Indicator == "Non-pregnant: Acute inflammation", c("Estimate", "LCL", "UCL")])))

npInflammation[19, "State"] <- "National"

names(npInflammation) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 2))

addWorksheet(wb = reportTables, sheetName = "npInflammation")
writeData(wb = reportTables, sheet = "npInflammation", x = npInflammation)

## Calcium
npCalcium <- data.frame(stateResults %>%
                          filter(Indicator == "Non-pregnant: Median serum calcium concentration (mg/dL)") %>%
                          dplyr::select(State, estimate, lcl, ucl),
                        stateResults %>%
                          filter(Indicator == "Non-pregnant: Hypocalcaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stateResults %>%
                          filter(Indicator == "Non-pregnant: Hypercalcaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stringsAsFactors = FALSE)

npCalcium <- npCalcium[order(npCalcium$State), ]

npCalcium <- rbind(npCalcium,
                      unlist(
                        c(State = 1,
                          nationalResults[nationalResults$Indicator == "Non-pregnant: Median serum calcium concentration (mg/dL)", c("Estimate", "LCL", "UCL")],
                          nationalResults[nationalResults$Indicator == "Non-pregnant: Hypocalcaemia", c("Estimate", "LCL", "UCL")],
                          nationalResults[nationalResults$Indicator == "Non-pregnant: Hypercalcaemia", c("Estimate", "LCL", "UCL")])))

npCalcium[19, "State"] <- "National"

names(npCalcium) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 3))

addWorksheet(wb = reportTables, sheetName = "npCalcium")
writeData(wb = reportTables, sheet = "npCalcium", x = npCalcium)

## Iodine
npnlIodine <- data.frame(stateResults %>%
                           filter(Indicator == "Non-pregnant non-lactating: Median urinary iodine concentration (microgram/L)") %>%
                           dplyr::select(State, estimate, lcl, ucl),
                         stringsAsFactors = FALSE)

npnlIodine <- npnlIodine[order(npnlIodine$State), ]

npnlIodine <- rbind(npnlIodine,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Non-pregnant non-lactating: Median urinary iodine concentration (microgram/L)", c("Estimate", "LCL", "UCL")])))

npnlIodine[19, "State"] <- "National"

names(npnlIodine) <- c("State", "Estimate", "95% LCL", "95% UCL")

addWorksheet(wb = reportTables, sheetName = "npnlIodine")
writeData(wb = reportTables, sheet = "npnlIodine", x = npnlIodine)

## Intake
nplIodine <- data.frame(stateResults %>%
                          filter(Indicator == "Non-pregnant lactating: Median urinary iodine concentration (microgram/L)") %>%
                          dplyr::select(State, estimate, lcl, ucl),
                        stringsAsFactors = FALSE)

nplIodine <- nplIodine[order(nplIodine$State), ]

nplIodine <- rbind(nplIodine,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Non-pregnant lactating: Median urinary iodine concentration (microgram/L)", c("Estimate", "LCL", "UCL")])))

nplIodine[19, "State"] <- "National"

names(nplIodine) <- c("State", "Estimate", "95% LCL", "95% UCL")

addWorksheet(wb = reportTables, sheetName = "nplIodine")
writeData(wb = reportTables, sheet = "nplIodine", x = nplIodine)

## Create pregnant tables ##################################################

## Anaemia
pAnaemia <- data.frame(stateResults %>%
                         filter(Indicator == "Pregnant: Median adjusted serum haemoglobin concentration (g/dL)") %>%
                         dplyr::select(State, estimate, lcl, ucl),
                       stateResults %>%
                         filter(Indicator == "Pregnant: Mild anaemia") %>%
                         dplyr::select(estimate, lcl, ucl),
                       stateResults %>%
                         filter(Indicator == "Pregnant: Moderate anaemia") %>%
                         dplyr::select(estimate, lcl, ucl),
                       stateResults %>%
                         filter(Indicator == "Pregnant: Severe anaemia") %>%
                         dplyr::select(estimate, lcl, ucl),
                       stringsAsFactors = FALSE)

pAnaemia <- pAnaemia[order(pAnaemia$State), ]

pAnaemia <- rbind(pAnaemia,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Pregnant: Median adjusted serum haemoglobin concentration (g/dL)", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Pregnant: Mild anaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Pregnant: Moderate anaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Pregnant: Severe anaemia", c("Estimate", "LCL", "UCL")])))

pAnaemia[19, "State"] <- "National"

names(pAnaemia) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 4))

addWorksheet(wb = reportTables, sheetName = "pAnaemia")
writeData(wb = reportTables, sheet = "pAnaemia", x = pAnaemia)

## Iron
pIron <- data.frame(stateResults %>%
                      filter(Indicator == "Pregnant: Median adjusted serum ferritin concentration (ng/mL)") %>%
                      dplyr::select(State, estimate, lcl, ucl),
                    stateResults %>%
                      filter(Indicator == "Pregnant: Iron deficiency") %>%
                      dplyr::select(estimate, lcl, ucl),
                    stateResults %>%
                      filter(Indicator == "Pregnant: Iron deficiency anaemia") %>%
                      dplyr::select(estimate, lcl, ucl),
                    stringsAsFactors = FALSE)

pIron <- pIron[order(pIron$State), ]

pIron <- rbind(pIron,
                unlist(
                  c(State = 1,
                    nationalResults[nationalResults$Indicator == "Pregnant: Median adjusted serum ferritin concentration (ng/mL)", c("Estimate", "LCL", "UCL")],
                    nationalResults[nationalResults$Indicator == "Pregnant: Iron deficiency", c("Estimate", "LCL", "UCL")],
                    nationalResults[nationalResults$Indicator == "Pregnant: Iron deficiency anaemia", c("Estimate", "LCL", "UCL")])))

pIron[19, "State"] <- "National"

names(pIron) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 3))

addWorksheet(wb = reportTables, sheetName = "pIron")
writeData(wb = reportTables, sheet = "pIron", x = pIron)

## Acute inflammation
pInflammation <- data.frame(stateResults %>%
                              filter(Indicator == "Pregnant: Median serum c-reactive protein concentration (mg/L)") %>%
                              dplyr::select(State, estimate, lcl, ucl),
                            stateResults %>%
                              filter(Indicator == "Pregnant: Acute inflammation") %>%
                              dplyr::select(estimate, lcl, ucl),
                             stringsAsFactors = FALSE)

pInflammation <- pInflammation[order(pInflammation$State), ]

pInflammation <- rbind(pInflammation,
                        unlist(
                          c(State = 1,
                            nationalResults[nationalResults$Indicator == "Pregnant: Median serum c-reactive protein concentration (mg/L)", c("Estimate", "LCL", "UCL")],
                            nationalResults[nationalResults$Indicator == "Pregnant: Acute inflammation", c("Estimate", "LCL", "UCL")])))

pInflammation[19, "State"] <- "National"

names(pInflammation) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 2))

addWorksheet(wb = reportTables, sheetName = "pInflammation")
writeData(wb = reportTables, sheet = "pInflammation", x = pInflammation)

## Calcium
pCalcium <- data.frame(stateResults %>%
                         filter(Indicator == "Pregnant: Median serum calcium concentration (mg/dL)") %>%
                         dplyr::select(State, estimate, lcl, ucl),
                       stateResults %>%
                         filter(Indicator == "Pregnant: Hypocalcaemia") %>%
                         dplyr::select(estimate, lcl, ucl),
                       stateResults %>%
                         filter(Indicator == "Pregnant: Hypercalcaemia") %>%
                         dplyr::select(estimate, lcl, ucl),
                       stringsAsFactors = FALSE)

pCalcium <- pCalcium[order(pCalcium$State), ]

pCalcium <- rbind(pCalcium,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Pregnant: Median serum calcium concentration (mg/dL)", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Pregnant: Hypocalcaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Pregnant: Hypercalcaemia", c("Estimate", "LCL", "UCL")])))

pCalcium[19, "State"] <- "National"

names(pCalcium) <- c("State", rep(c("Estimate", "95% LCL", "95% UCL"), 3))

addWorksheet(wb = reportTables, sheetName = "pCalcium")
writeData(wb = reportTables, sheet = "pCalcium", x = pCalcium)

## Iodine

pIodine <- data.frame(stateResults %>%
                        filter(Indicator == "Pregnant: Median urinary iodine concentration (microgram/L)") %>%
                        dplyr::select(State, estimate, lcl, ucl),
                      stringsAsFactors = FALSE)

pIodine <- pIodine[order(pIodine$State), ]

pIodine <- rbind(pIodine,
                  unlist(
                    c(State = 1,
                      nationalResults[nationalResults$Indicator == "Pregnant: Median urinary iodine concentration (microgram/L)", c("Estimate", "LCL", "UCL")])))

pIodine[19, "State"] <- "National"

names(pIodine) <- c("State", "Estimate", "95% LCL", "95% UCL")

addWorksheet(wb = reportTables, sheetName = "pIodine")
writeData(wb = reportTables, sheet = "pIodine", x = pIodine)

saveWorkbook(wb = reportTables, file = "reportTables/reportTables.xlsx", overwrite = TRUE)
