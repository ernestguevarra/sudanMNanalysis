## Libraries
library(openxlsx)
library(magrittr)
library(stringr)
library(dplyr)

## Read data
localityResults <- read.csv("_byStatesMNresults.csv", stringsAsFactors = FALSE)
localityResults <- localityResults[!localityResults$Locality %in% c("Heiban", "El Buram", "Um Durein"), ]
localityResults$estimate <- round(localityResults$estimate * 100, digits = 2)
localityResults$lcl <- round(localityResults$lcl * 100, digits = 2)
localityResults$ucl <- round(localityResults$ucl * 100, digits = 2)

localityResults$Indicator <- localityResults$Indicator %>%
  str_replace_all(pattern = "Anaemia", replacement = "anaemia") %>%
  str_replace_all(pattern = "Hpercalcaemia", replacement = "Hypercalcaemia")

stateResults <- read.csv("_stateResults.csv", stringsAsFactors = FALSE)
stateResults$Indicator <- stateResults$Indicator %>%
  str_replace_all(pattern = "Anaemia", replacement = "anaemia") %>%
  str_replace_all(pattern = "Hpercalcaemia", replacement = "Hypercalcaemia")

nationalResults <- read.csv("_nationalResults.csv", stringsAsFactors = FALSE)
nationalResults$Indicator <- nationalResults$Indicator %>%
  str_replace_all(pattern = "Anaemia", replacement = "anaemia") %>%
  str_replace_all(pattern = "Hpercalcaemia", replacement = "Hypercalcaemia")

##
reportTables <- createWorkbook()

## Create children tables ######################################################

## Anaemia
childAnaemia <- data.frame(localityResults %>% 
                                filter(Indicator == "Child: Mild anaemia") %>%
                                dplyr::select(State, Locality, estimate, lcl, ucl),
                              localityResults %>%
                                filter(Indicator == "Child: Moderate anaemia") %>%
                                dplyr::select(estimate, lcl, ucl),
                              localityResults %>%
                                filter(Indicator == "Child: Severe anaemia") %>%
                                dplyr::select(estimate, lcl, ucl),
                              stringsAsFactors = FALSE)
  
addWorksheet(wb = reportTables, sheetName = "childAnaemiaLocality")
writeData(wb = reportTables, sheet = "childAnaemiaLocality", x = childAnaemia)

childAnaemia <- data.frame(stateResults %>%
                             filter(Indicator == "Child: Mild anaemia") %>%
                             dplyr::select(State, Estimate, LCL, UCL),
                           stateResults %>%
                             filter(Indicator == "Child: Moderate anaemia") %>%
                             dplyr::select(Estimate, LCL, UCL),
                           stateResults %>%
                             filter(Indicator == "Child: Severe anaemia") %>%
                             dplyr::select(Estimate, LCL, UCL),
                           stringsAsFactors = FALSE)

childAnaemia <- rbind(childAnaemia,
                      unlist(
                        c(State = 1,
                        nationalResults[nationalResults$Indicator == "Child: Mild anaemia", c("Estimate", "LCL", "UCL")],
                        nationalResults[nationalResults$Indicator == "Child: Moderate anaemia", c("Estimate", "LCL", "UCL")],
                        nationalResults[nationalResults$Indicator == "Child: Severe anaemia", c("Estimate", "LCL", "UCL")])))

childAnaemia[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "childAnaemiaState")
writeData(wb = reportTables, sheet = "childAnaemiaState", x = childAnaemia)

## Iron
childIron <- data.frame(localityResults %>%
                          filter(Indicator == "Child: Iron deficiency") %>%
                          dplyr::select(State, Locality, estimate, lcl, ucl),
                        localityResults %>%
                          filter(Indicator == "Child: Iron deficiency anaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "childIronLocality")
writeData(wb = reportTables, sheet = "childIronLocality", x = childIron)

childIron <- data.frame(stateResults %>%
                          filter(Indicator == "Child: Iron deficiency") %>%
                          dplyr::select(State, Estimate, LCL, UCL),
                        stateResults %>%
                          filter(Indicator == "Child: Iron deficiency anaemia") %>%
                          dplyr::select(Estimate, LCL, UCL),
                        stringsAsFactors = FALSE)

childIron <- rbind(childIron,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Child: Iron deficiency", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Child: Iron deficiency anaemia", c("Estimate", "LCL", "UCL")])))

childIron[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "childIronState")
writeData(wb = reportTables, sheet = "childIronState", x = childIron)

## Acute inflammation
childInflammation <- data.frame(localityResults %>%
                                  filter(Indicator == "Child: Acute inflammation") %>%
                                  dplyr::select(State, Locality, estimate, lcl, ucl),
                                stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "childInflammationLocality")
writeData(wb = reportTables, sheet = "childInflammationLocality", x = childInflammation)

childInflammation <- data.frame(stateResults %>%
                                  filter(Indicator == "Child: Acute inflammation") %>%
                                  dplyr::select(State, Estimate, LCL, UCL),
                                stringsAsFactors = FALSE)

childInflammation <- rbind(childInflammation,
                           unlist(
                             c(State = 1,
                               nationalResults[nationalResults$Indicator == "Child: Acute inflammation", c("Estimate", "LCL", "UCL")])))

childInflammation[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "childInflammationState")
writeData(wb = reportTables, sheet = "childInflammationState", x = childInflammation)

## Calcium
childCalcium <- data.frame(localityResults %>%
                             filter(Indicator == "Child: Hypocalcaemia") %>%
                             dplyr::select(State, Locality, estimate, lcl, ucl),
                           localityResults %>%
                             filter(Indicator == "Child: Hypercalcaemia") %>%
                             dplyr::select("estimate", "lcl", "ucl"),
                           stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "childCalciumLocality")
writeData(wb = reportTables, sheet = "childCalciumLocality", x = childCalcium)

childCalcium <- data.frame(stateResults %>%
                             filter(Indicator == "Child: Hypocalcaemia") %>%
                             dplyr::select(State, Estimate, LCL, UCL),
                           stateResults %>%
                             filter(Indicator == "Child: Hypercalcaemia") %>%
                             dplyr::select(Estimate, LCL, UCL),
                           stringsAsFactors = FALSE)

childCalcium <- rbind(childCalcium,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Child: Hypocalcaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Child: Hypercalcaemia", c("Estimate", "LCL", "UCL")])))

childCalcium[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "childCalciumState")
writeData(wb = reportTables, sheet = "childCalciumState", x = childCalcium)


## Create non-pregnant tables ##################################################

## Anaemia
npAnaemia <- data.frame(localityResults %>% 
                          filter(Indicator == "Non-pregnant: Mild anaemia") %>%
                          dplyr::select(State, Locality, estimate, lcl, ucl),
                        localityResults %>%
                          filter(Indicator == "Non-pregnant: Moderate anaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        localityResults %>%
                          filter(Indicator == "Non-pregnant: Severe anaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "npAnaemiaLocality")
writeData(wb = reportTables, sheet = "npAnaemiaLocality", x = npAnaemia)

npAnaemia <- data.frame(stateResults %>%
                          filter(Indicator == "Non-pregnant: Mild anaemia") %>%
                          dplyr::select(State, Estimate, LCL, UCL),
                        stateResults %>%
                          filter(Indicator == "Non-pregnant: Moderate anaemia") %>%
                          dplyr::select(Estimate, LCL, UCL),
                        stateResults %>%
                          filter(Indicator == "Non-pregnant: Severe anaemia") %>%
                          dplyr::select(Estimate, LCL, UCL),
                        stringsAsFactors = FALSE)

npAnaemia <- rbind(npAnaemia,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Mild anaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Moderate anaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Severe anaemia", c("Estimate", "LCL", "UCL")])))

npAnaemia[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "npAnaemiaState")
writeData(wb = reportTables, sheet = "npAnaemiaState", x = npAnaemia)

## Iron
npIron <- data.frame(localityResults %>%
                       filter(Indicator == "Non-pregnant: Iron deficiency") %>%
                       dplyr::select(State, Locality, estimate, lcl, ucl),
                     localityResults %>%
                       filter(Indicator == "Non-pregnant: Iron deficiency anaemia") %>%
                       dplyr::select(estimate, lcl, ucl),
                     stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "npIronLocality")
writeData(wb = reportTables, sheet = "npIronLocality", x = npIron)

npIron <- data.frame(stateResults %>%
                       filter(Indicator == "Non-pregnant: Iron deficiency") %>%
                       dplyr::select(State, Estimate, LCL, UCL),
                     stateResults %>%
                       filter(Indicator == "Non-pregnant: Iron deficiency anaemia") %>%
                       dplyr::select(Estimate, LCL, UCL),
                     stringsAsFactors = FALSE)

npIron <- rbind(npIron,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Iron deficiency", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant: Iron deficiency anaemia", c("Estimate", "LCL", "UCL")])))

npIron[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "npIronState")
writeData(wb = reportTables, sheet = "npIronState", x = npIron)

## Acute inflammation
npInflammation <- data.frame(localityResults %>%
                               filter(Indicator == "Non-pregnant: Acute inflammation") %>%
                               dplyr::select(State, Locality, estimate, lcl, ucl),
                             stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "npInflammationLocality")
writeData(wb = reportTables, sheet = "npInflammationLocality", x = npInflammation)

npInflammation <- data.frame(stateResults %>%
                               filter(Indicator == "Non-pregnant: Acute inflammation") %>%
                               dplyr::select(State, Estimate, LCL, UCL),
                             stringsAsFactors = FALSE)

npInflammation <- rbind(npInflammation,
                           unlist(
                             c(State = 1,
                               nationalResults[nationalResults$Indicator == "Non-pregnant: Acute inflammation", c("Estimate", "LCL", "UCL")])))

npInflammation[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "npInflammationState")
writeData(wb = reportTables, sheet = "npInflammationState", x = npInflammation)

## Calcium
npCalcium <- data.frame(localityResults %>%
                          filter(Indicator == "Non-pregnant: Hypocalcaemia") %>%
                          dplyr::select(State, Locality, estimate, lcl, ucl),
                        localityResults %>%
                          filter(Indicator == "Non-pregnant: Hypercalcaemia") %>%
                          dplyr::select("estimate", "lcl", "ucl"),
                        stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "npCalciumLocality")
writeData(wb = reportTables, sheet = "npCalciumLocality", x = npCalcium)

npCalcium <- data.frame(stateResults %>%
                          filter(Indicator == "Non-pregnant: Hypocalcaemia") %>%
                             dplyr::select(State, Estimate, LCL, UCL),
                        stateResults %>%
                          filter(Indicator == "Non-pregnant: Hypercalcaemia") %>%
                          dplyr::select(Estimate, LCL, UCL),
                        stringsAsFactors = FALSE)

npCalcium <- rbind(npCalcium,
                      unlist(
                        c(State = 1,
                          nationalResults[nationalResults$Indicator == "Non-pregnant: Hypocalcaemia", c("Estimate", "LCL", "UCL")],
                          nationalResults[nationalResults$Indicator == "Non-pregnant: Hypercalcaemia", c("Estimate", "LCL", "UCL")])))

npCalcium[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "npCalciumState")
writeData(wb = reportTables, sheet = "npCalciumState", x = npCalcium)

## Iodine
npnlIodine <- data.frame(localityResults %>% 
                           filter(Indicator == "Non-pregnant non-lactating: Mild iodine deficiency") %>%
                           dplyr::select(State, Locality, estimate, lcl, ucl),
                         localityResults %>%
                           filter(Indicator == "Non-pregnant non-lactating: Moderate iodine deficiency") %>%
                           dplyr::select(estimate, lcl, ucl),
                         localityResults %>%
                           filter(Indicator == "Non-pregnant non-lactating: Severe iodine deficiency") %>%
                           dplyr::select(estimate, lcl, ucl),
                          stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "npnlIodineLocality")
writeData(wb = reportTables, sheet = "npnlIodineLocality", x = npnlIodine)

npnlIodine <- data.frame(stateResults %>%
                           filter(Indicator == "Non-pregnant non-lactating: Mild iodine deficiency") %>%
                           dplyr::select(State, Estimate, LCL, UCL),
                         stateResults %>%
                           filter(Indicator == "Non-pregnant non-lactating: Moderate iodine deficiency") %>%
                           dplyr::select(Estimate, LCL, UCL),
                         stateResults %>%
                           filter(Indicator == "Non-pregnant non-lactating: Severe iodine deficiency") %>%
                           dplyr::select(Estimate, LCL, UCL),
                         stringsAsFactors = FALSE)

npnlIodine <- rbind(npnlIodine,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Non-pregnant non-lactating: Mild iodine deficiency", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant non-lactating: Moderate iodine deficiency", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Non-pregnant non-lactating: Severe iodine deficiency", c("Estimate", "LCL", "UCL")])))

npnlIodine[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "npnlIodineState")
writeData(wb = reportTables, sheet = "npnlIodineState", x = npnlIodine)

## Intake

npIodine <- data.frame(localityResults %>% 
                         filter(Indicator == "Non-pregnant non-lactating: Iodine intake above requirements") %>%
                         dplyr::select(State, Locality, estimate, lcl, ucl),
                       localityResults %>%
                         filter(Indicator == "Non-pregnant non-lactating: Iodine intake excessive") %>%
                         dplyr::select(estimate, lcl, ucl),
                       localityResults %>% 
                         filter(Indicator == "Non-pregnant lactating: Iodine intake above requirements") %>%
                         dplyr::select(estimate, lcl, ucl),
                       localityResults %>%
                         filter(Indicator == "Non-pregnant lactating: Iodine intake excessive") %>%
                         dplyr::select(estimate, lcl, ucl),
                       stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "npIntakeLocality")
writeData(wb = reportTables, sheet = "npIntakeLocality", x = npIodine)

npIodine <- data.frame(stateResults %>%
                         filter(Indicator == "Non-pregnant non-lactating: Iodine intake above requirements") %>%
                         dplyr::select(State, Estimate, LCL, UCL),
                       stateResults %>%
                         filter(Indicator == "Non-pregnant non-lactating: Iodine intake excessive") %>%
                         dplyr::select(Estimate, LCL, UCL),
                       stateResults %>%
                         filter(Indicator == "Non-pregnant lactating: Iodine intake above requirements") %>%
                         dplyr::select(Estimate, LCL, UCL),
                       stateResults %>%
                         filter(Indicator == "Non-pregnant lactating: Iodine intake excessive") %>%
                         dplyr::select(Estimate, LCL, UCL),
                       stringsAsFactors = FALSE)

npIodine <- rbind(npIodine,
                    unlist(
                      c(State = 1,
                        nationalResults[nationalResults$Indicator == "Non-pregnant non-lactating: Iodine intake above requirements", c("Estimate", "LCL", "UCL")],
                        nationalResults[nationalResults$Indicator == "Non-pregnant non-lactating: Iodine intake excessive", c("Estimate", "LCL", "UCL")],
                        nationalResults[nationalResults$Indicator == "Non-pregnant lactating: Iodine intake above requirements", c("Estimate", "LCL", "UCL")],
                        nationalResults[nationalResults$Indicator == "Non-pregnant lactating: Iodine intake excessive", c("Estimate", "LCL", "UCL")])))

npIodine[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "npIntakeState")
writeData(wb = reportTables, sheet = "npIntakeState", x = npIodine)


## Create pregnant tables ##################################################

## Anaemia
pAnaemia <- data.frame(localityResults %>% 
                          filter(Indicator == "Pregnant: Mild anaemia") %>%
                          dplyr::select(State, Locality, estimate, lcl, ucl),
                        localityResults %>%
                          filter(Indicator == "Pregnant: Moderate anaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        localityResults %>%
                          filter(Indicator == "Pregnant: Severe anaemia") %>%
                          dplyr::select(estimate, lcl, ucl),
                        stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "pAnaemiaLocality")
writeData(wb = reportTables, sheet = "pAnaemiaLocality", x = pAnaemia)

pAnaemia <- data.frame(stateResults %>%
                          filter(Indicator == "Pregnant: Mild anaemia") %>%
                          dplyr::select(State, Estimate, LCL, UCL),
                        stateResults %>%
                          filter(Indicator == "Pregnant: Moderate anaemia") %>%
                          dplyr::select(Estimate, LCL, UCL),
                        stateResults %>%
                          filter(Indicator == "Pregnant: Severe anaemia") %>%
                          dplyr::select(Estimate, LCL, UCL),
                        stringsAsFactors = FALSE)

pAnaemia <- rbind(pAnaemia,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Pregnant: Mild anaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Pregnant: Moderate anaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Pregnant: Severe anaemia", c("Estimate", "LCL", "UCL")])))

pAnaemia[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "pAnaemiaState")
writeData(wb = reportTables, sheet = "pAnaemiaState", x = pAnaemia)

## Iron
pIron <- data.frame(localityResults %>%
                       filter(Indicator == "Pregnant: Iron deficiency") %>%
                       dplyr::select(State, Locality, estimate, lcl, ucl),
                     localityResults %>%
                       filter(Indicator == "Pregnant: Iron deficiency anaemia") %>%
                       dplyr::select(estimate, lcl, ucl),
                     stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "pIronLocality")
writeData(wb = reportTables, sheet = "pIronLocality", x = pIron)

pIron <- data.frame(stateResults %>%
                       filter(Indicator == "Pregnant: Iron deficiency") %>%
                       dplyr::select(State, Estimate, LCL, UCL),
                     stateResults %>%
                       filter(Indicator == "Pregnant: Iron deficiency anaemia") %>%
                       dplyr::select(Estimate, LCL, UCL),
                     stringsAsFactors = FALSE)

pIron <- rbind(pIron,
                unlist(
                  c(State = 1,
                    nationalResults[nationalResults$Indicator == "Pregnant: Iron deficiency", c("Estimate", "LCL", "UCL")],
                    nationalResults[nationalResults$Indicator == "Pregnant: Iron deficiency anaemia", c("Estimate", "LCL", "UCL")])))

pIron[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "pIronState")
writeData(wb = reportTables, sheet = "pIronState", x = pIron)

## Acute inflammation
pInflammation <- data.frame(localityResults %>%
                               filter(Indicator == "Pregnant: Acute inflammation") %>%
                               dplyr::select(State, Locality, estimate, lcl, ucl),
                             stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "pInflammationLocality")
writeData(wb = reportTables, sheet = "pInflammationLocality", x = pInflammation)

pInflammation <- data.frame(stateResults %>%
                               filter(Indicator == "Pregnant: Acute inflammation") %>%
                               dplyr::select(State, Estimate, LCL, UCL),
                             stringsAsFactors = FALSE)

pInflammation <- rbind(pInflammation,
                        unlist(
                          c(State = 1,
                            nationalResults[nationalResults$Indicator == "Pregnant: Acute inflammation", c("Estimate", "LCL", "UCL")])))

pInflammation[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "pInflammationState")
writeData(wb = reportTables, sheet = "pInflammationState", x = pInflammation)

## Calcium
pCalcium <- data.frame(localityResults %>%
                          filter(Indicator == "Pregnant: Hypocalcaemia") %>%
                          dplyr::select(State, Locality, estimate, lcl, ucl),
                        localityResults %>%
                          filter(Indicator == "Pregnant: Hypercalcaemia") %>%
                          dplyr::select("estimate", "lcl", "ucl"),
                        stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "pCalciumLocality")
writeData(wb = reportTables, sheet = "pCalciumLocality", x = pCalcium)

pCalcium <- data.frame(stateResults %>%
                          filter(Indicator == "Pregnant: Hypocalcaemia") %>%
                          dplyr::select(State, Estimate, LCL, UCL),
                        stateResults %>%
                          filter(Indicator == "Pregnant: Hypercalcaemia") %>%
                          dplyr::select(Estimate, LCL, UCL),
                        stringsAsFactors = FALSE)

pCalcium <- rbind(pCalcium,
                   unlist(
                     c(State = 1,
                       nationalResults[nationalResults$Indicator == "Pregnant: Hypocalcaemia", c("Estimate", "LCL", "UCL")],
                       nationalResults[nationalResults$Indicator == "Pregnant: Hypercalcaemia", c("Estimate", "LCL", "UCL")])))

pCalcium[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "pCalciumState")
writeData(wb = reportTables, sheet = "pCalciumState", x = pCalcium)

## Iodine

## Intake

pIodine <- data.frame(localityResults %>% 
                        filter(Indicator == "Pregnant: Iodine insufficiency") %>%
                        dplyr::select(State, Locality, estimate, lcl, ucl),
                      localityResults %>% 
                         filter(Indicator == "Pregnant: Iodine intake above requirements") %>%
                         dplyr::select(estimate, lcl, ucl),
                      localityResults %>%
                         filter(Indicator == "Pregnant: Iodine intake excessive") %>%
                         dplyr::select(estimate, lcl, ucl),
                      stringsAsFactors = FALSE)

addWorksheet(wb = reportTables, sheetName = "pIntakeLocality")
writeData(wb = reportTables, sheet = "pIntakeLocality", x = pIodine)

pIodine <- data.frame(stateResults %>%
                        filter(Indicator == "Pregnant: Iodine insufficiency") %>%
                        dplyr::select(State, Estimate, LCL, UCL),
                      stateResults %>%
                         filter(Indicator == "Pregnant: Iodine intake above requirements") %>%
                         dplyr::select(Estimate, LCL, UCL),
                      stateResults %>%
                         filter(Indicator == "Pregnant: Iodine intake excessive") %>%
                         dplyr::select(Estimate, LCL, UCL),
                      stringsAsFactors = FALSE)

pIodine <- rbind(pIodine,
                  unlist(
                    c(State = 1,
                      nationalResults[nationalResults$Indicator == "Pregnant: Iodine insufficiency", c("Estimate", "LCL", "UCL")],
                      nationalResults[nationalResults$Indicator == "Pregnant: Iodine intake above requirements", c("Estimate", "LCL", "UCL")],
                      nationalResults[nationalResults$Indicator == "Pregnant: Iodine intake excessive", c("Estimate", "LCL", "UCL")])))

pIodine[19, "State"] <- "National"

addWorksheet(wb = reportTables, sheetName = "pIntakeState")
writeData(wb = reportTables, sheet = "pIntakeState", x = pIodine)

saveWorkbook(wb = reportTables, file = "reportTables/reportTables.xlsx", overwrite = TRUE)