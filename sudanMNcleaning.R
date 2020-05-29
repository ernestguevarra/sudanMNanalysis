################################################################################
#
# Sudan Micronutrient Data Cleaning
#
################################################################################

## Workspace setup #############################################################

## Load libraries
library(openxlsx)
library(rgdal)
library(rgeos)
library(raster)

## Load functions


## Read micronutrient data
mnData <- read.xlsx("data/mnData_master_Sudan_V12.xlsx", sheet = 1)

## Read PSU data
psuData <- read.csv("data/updatedPSU.csv", stringsAsFactors = FALSE)

## Read locality and state administrative structure data
locNames <- read.csv("data/locNames.csv", stringsAsFactors = FALSE)

## Remove arabic names for states and localities
locNames <- subset(locNames, select = c(-stateAR, -localityAR))

## Read SRTM raster for elevation data
sudanSRTM <- raster(x = "data/srtm/SDN_alt.vrt")

## Read Sudan locality shapefile
sudan02 <- readOGR(dsn = "data/locality/", layer = "sudan02")


## Clean micronutrient data ####################################################

## Check that all numeric variables are numeric and change accordingly and
## rename variables to more coherent labels
mnData <- data.frame(stateID = as.integer(mnData$state),
                     barcode = as.integer(mnData$bc),
                     localityID = as.integer(mnData$locality),
                     psu = as.integer(mnData$psu),
                     womanAge = as.numeric(mnData$m.age),
                     childAge = as.numeric(mnData$ch.age),
                     sex = as.integer(mnData$sex),
                     muac = as.numeric(mnData$muac),
                     weight = as.numeric(mnData$ch.weight),
                     height = as.numeric(mnData$ch.height),
                     oedema = as.numeric(mnData$ch.oedema),
                     hb = as.numeric(mnData$hb),
                     group = mnData$group,
                     calcium = as.numeric(mnData$calcium),
                     crp = as.numeric(mnData$crp),
                     ferritin = as.numeric(mnData$ferritin),
                     iodine = as.numeric(mnData$iodine),
                     retinol = as.numeric(mnData$retinol),
                     vitd = as.numeric(mnData$vitD))

## Classify respondents as children or women of reproductive age
##   1 = child less than 59 months old
##   2 = woman 15-49 years of age

## Check first if any respondent has values for both ch.age and m.age
mnData[all(!is.na(mnData[ , c("womanAge", "childAge")]))]

## Create ageGrp
ageGrp <- NA
ageGrp[mnData$childAge <= 60 & is.na(mnData$womanAge)] <- 1
ageGrp[is.na(mnData$childAge) & is.na(mnData$womanAge) & mnData$group == "Child"] <- 1

ageGrp[mnData$womanAge %in% 15:49 & is.na(mnData$childAge)] <- 2
ageGrp[is.na(mnData$childAge) & is.na(mnData$womanAge) & mnData$group != "Child"] <- 1

## Check for NAs
table(ageGrp, useNA = "always")

## 73 records do not have enough information to determine their age group

## Special classifications: pregnant
##   1 = YES
##   2 = NO
##   NA = not known status or status does not apply

pregnant <- NA
pregnant[mnData$group == "Pregnant Principal carer"]                        <- 1
pregnant[mnData$group == "Pregnant Not Principal carer"]                    <- 1
pregnant[mnData$group == "Pregnant and Lactating Principal Carer"]          <- 1
pregnant[mnData$group == "Principal carer nighther pregnant nor lactating"] <- 2
pregnant[mnData$group == "Lactating Principal carer"]                       <- 2

## Check for NAs that are not children
table(pregnant[ageGrp == 2], useNA = "always")

## Total of 2111 WRA whose pregnancy status is unknown

## Special classifications: lactating
##   1 = YES
##   2 = NO
##   NA = not known status or status does not apply

lactating <- NA
lactating[mnData$group == "Pregnant and Lactating Principal Carer"]          <- 1
lactating[mnData$group == "Lactating Principal carer"]                       <- 1
lactating[mnData$group == "Pregnant Principal carer"]                        <- 2
lactating[mnData$group == "Pregnant Not Principal carer"]                    <- 2
lactating[mnData$group == "Principal carer nighther pregnant nor lactating"] <- 2

## Check for NAs that are not children
table(lactating[ageGrp == 2], useNA = "always")

## Total of 2111 WRA whose lactation status is unknown

## Add new variables to mnData
mnData <- data.frame(mnData, ageGrp, pregnant, lactating, stringsAsFactors = FALSE)

## Check data for missing values for psu
table(is.na(mnData$psu) | !mnData$psu %in% psuData$psu)

## 962 rows of data with NA value for PSU or with PSU not in the survey data. 
## There is no known way to determine PSU through other data. These data will
## be removed

## Remove data with missing PSUs or PSUs not mathcing 
mnData <- mnData[!is.na(mnData$psu) | mnData$psu %in% psuData$psu, ]

## Check for data with missing locality or with localityID not in survey data
table(is.na(mnData$localityID) | !mnData$localityID %in% psuData$locality)

## Check for data with missing state or with stateID not in survey data
table(is.na(mnData$stateID) | !mnData$stateID %in% psuData$state)

## Check for data that has none of the micronutrient data
table(apply(is.na(mnData[ c("hb", "calcium", "crp", "ferritin", "iodine")]), MARGIN = 1, FUN = all))

## 25 rows of data has no information on any micronutrients. These rows will be
## removed
mnData <- mnData[!apply(is.na(mnData[ c("hb", "calcium", "crp", "ferritin", "iodine")]), MARGIN = 1, FUN = all), ]

## 


