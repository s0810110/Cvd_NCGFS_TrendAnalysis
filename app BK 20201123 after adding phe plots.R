# Load packages ----
library(shiny)
library(ggplot2)
# User interface ----
library(shinythemes)
library(shinyWidgets)

# Geomapping -----
# library(rgdal)
# library(rgeos)
# library(tidyverse)
# library(stringr)

# library(spatialEco)
# library(plyr)
# library(dplyr)
# Geomapping -----

library(data.table)
library(sf)
#library(tmap)
library(sp)
library(mapview)
library(raster)

#leafgl
library(leaflet)
#library(leafgl)
library(colourvalues)

#date conversion
library(xts)

#funnel plot
library(funnelR)

#adding columns 
library(tibble)

#for melting dataframe
library(reshape2)

#for interactive plot 
library(plotly)

#for logo dashboardheader
library(shinydashboard)

#mixedsort 
library(gtools)

#for finding element in string 
library(stringi)


library(keras)
#use_condaenv("plaidml")  # only switch on in Mac os environment 


#for hidden sidebar widgets 
library(shinyjs)

#for copying objects without reference 
library(rlang)

#parse numbers 
library(readr)

#add help icon
library(shinyhelper)

#remove any unnecessary variables 
rm(list=ls())
gc()

# Load data ----
#cvdData <- fread('data/MSOAs_latest.csv', data.table=FALSE)

cvdData <- fread('data/MSOAs_latest.csv', data.table=FALSE)

#deprivIndx <- fread('data/File_2_-_IoD2019_Domains_of_Deprivation.csv', select=c('LocAuthDistrict_2019', 'IMD_Rank', 'IMD_Decile', 'Income_Rank', 'Income_Decile', 'Employment_Rank', 'Employment_Decile', 'Education_Skills_Training_Rank', 'Education_Skills _Training Decile', 'Health_Deprivation_Disability_Rank', 'Health_Deprivation_Disability_Decile', 'Crime_Rank', 'Crime_Decile', 'Barriers_to_Housing_and_Services_Rank', 'Barriers_to_Housing_and_Services_Decile', 'Living_Environment_Rank', 'Living_Environment_Decile'), data.table=FALSE)

#pubSize <- fread('data/publichousesandbarsbylocalauthority20012018.csv', select=c('Area code', 'count_2018'), data.table=FALSE)

deprivIndx <- fread('data/File_2_-_IoD2019_Domains_of_Deprivation.csv',  select=c('LocAuthDistrict_2019', 'IMD_Rank', 'IMD_Decile', 'Income_Rank', 'Income_Decile', 'Employment_Rank', 'Employment_Decile', 'Education_Skills_Training_Rank', 'Education_Skills _Training Decile', 'Health_Deprivation_Disability_Rank', 'Health_Deprivation_Disability_Decile', 'Crime_Rank', 'Crime_Decile', 'Barriers_to_Housing_and_Services_Rank', 'Barriers_to_Housing_and_Services_Decile', 'Living_Environment_Rank', 'Living_Environment_Decile'), data.table=FALSE)

pubSize <- fread('data/publichousesandbarsbylocalauthority20012018.csv',  select=c('Area code', 'count_2018'), data.table=FALSE)

#industrySize <- fread('data/ukbusinessworkbook2020.csv', select=c('Local_AuthCode_2020', 'Retail', 'Transport_Storage_inc_postal', 'Accommodation_food_services', 'Education', 'Health', 'Arts_entertainment_recreation_other_services'), data.table=FALSE)

industrySize <- fread('data/ukbusinessworkbook2020.csv',  select=c('Local_AuthCode_2020', 'Retail', 'Transport_Storage_inc_postal', 'Accommodation_food_services', 'Education', 'Health', 'Arts_entertainment_recreation_other_services'), data.table=FALSE)



deprivIndx[] <- lapply(deprivIndx, gsub, pattern=',', replacement='')

deprivIndx[,2:ncol(deprivIndx)] <- lapply(deprivIndx[,2:ncol(deprivIndx)], as.numeric)

# aggregate to obtain mean deprivation indices by unique local authority districts
meanDprvIndByLA <- aggregate(formula=.~LocAuthDistrict_2019, data=deprivIndx, FUN=mean)


# Local Authority data -----------------------------------------------------

# lsoaLAuthLookup <- fread("C:\\Installation WorkPCToLaptop\\Dashboard\\cardioDshBCvdDL\\data\\2019PopulationEstimateEngWal\\boundaryLines\\LocalAuthorityEngW_2011\\LSOA_to_Local_Authority_2017_Lookup.csv", select=c('LSOA11CD', 'LAD17NM'), data.table=FALSE)


# Local Authority data -----------------------------------------------------

# get location and weekly cases columns 
# casesCols <- cvdData[,c(5, 6, 9:ncol(cvdData))]

# #do this for only liverpool initially 
# #casesCols <- casesCols[casesCols$lad19_nm == "Liverpool", ]

# names(casesCols)[length(names(casesCols))]<-"wk_41" 

# #remove NA columns 
# casesCols <- casesCols[, colSums(is.na(casesCols)) != nrow(casesCols)]

# # aggregate to obtain count by unique local authority districts
# casesByLA <- aggregate(formula=.~lad19_cd+lad19_nm, data=casesCols, FUN=sum)

# remove LA names not in casesByLA 
# deprivIndx <- deprivIndx[deprivIndx$LocAuthDistrict_2019 %in% as.character(casesByLA$lad19_cd),]

# # remove duplicate LA codes 
# deprivIndxUniq <- deprivIndx[!duplicated(deprivIndx$LocAuthDistrict_2019),]

# # merge casesByLA LA code with deprivIndxUniq LA code 
# casesByLA_DprvInd <- merge(casesByLA, deprivIndxUniq, by.x="lad19_cd",by.y="LocAuthDistrict_2019")
				
				
# merge pub size data -------------------------------------------------------------------

pubSize$count_2018 <- gsub(',', '', pubSize$count_2018)

pubSize$count_2018 <- as.numeric(pubSize$count_2018)

# aggregate to obtain count by unique local authority districts
pubCountByLA <- aggregate(formula=.~`Area code`, data=pubSize, FUN=sum)

# remove LA names not in deprivIndx 
pubCountByLA <- pubCountByLA[pubCountByLA$`Area code` %in% as.character(meanDprvIndByLA$LocAuthDistrict_2019),]

# remove duplicate LA codes 
pubCountByLAUniq <- pubCountByLA[!duplicated(pubCountByLA$`Area code`),]

# merge deprivIndx LA code with pubCountByLAUniq: 312 unique LA codes out of 318 remain after joining deprivation and pub 
pubCntByLA_DprvInd_Pub <- merge(pubCountByLAUniq, meanDprvIndByLA, by.x="Area code", by.y="LocAuthDistrict_2019")

names(pubCntByLA_DprvInd_Pub)[names(pubCntByLA_DprvInd_Pub) == "count_2018"] <- "pub_count_2018"

# merge pub size data -------------------------------------------------------------------

# merge industry size data -------------------------------------------------------------------

industrySize[] <- lapply(industrySize, gsub, pattern=',', replacement='')

# remove LA names not in pubCntByLA_DprvInd_Pub 
industrySize <- industrySize[industrySize$Local_AuthCode_2020 %in% as.character(pubCntByLA_DprvInd_Pub$`Area code`),]

# remove duplicate LA codes 
industrySizeUniq <- industrySize[!duplicated(industrySize$Local_AuthCode_2020),]

# merge pubCntByLA_DprvInd_Pub LA code with industrySizeUniq LA code 
pubCntByLA_DprvInd_Pub_Indstr <- merge(pubCntByLA_DprvInd_Pub, industrySizeUniq, by.x="Area code",by.y="Local_AuthCode_2020")

# merge industry size data -------------------------------------------------------------------


# read population estimates 2019 -------------------------------------------------


femalesPpltn <- fread('data/femalePpltn2019ladcodes.csv', select=c('Code', 'All_females'), data.table=FALSE)

malesPpltn <- fread('data/malePpltn2019ladcodes.csv', select=c('Code', 'All_males'), data.table=FALSE)

inFlowPpltn <- fread('data/migrationFlow2019ladcodes.csv', data.table=FALSE)

agePpltn <- fread('data/age_2019ladcodes.csv', data.table=FALSE)

ppltnEst10yrs <- Reduce((function() {counter = 1
									function(x, y) {
									counter <<- counter + 1
									d = merge(x, y, by = 'Code')
									
									}})(), list(femalesPpltn, malesPpltn, inFlowPpltn, agePpltn))

#merge needs to be done 
ppltnEst10yrs[] <- lapply(ppltnEst10yrs, gsub, pattern=',', replacement='')

# remove LA names not in pubCntByLA_DprvInd_Pub_Indstr 
ppltnEst10yrs <- ppltnEst10yrs[ppltnEst10yrs$Code %in% as.character(pubCntByLA_DprvInd_Pub_Indstr$`Area code`),]

# remove duplicate LA codes 
ppltnEst10yrsUniq <- ppltnEst10yrs[!duplicated(ppltnEst10yrs$Code),]

# merge pubCntByLA_DprvInd_Pub_Indstr LA code with ppltnEst10yrsUniq LA code 
pubCntByLA_DprvInd_PubIndstr_ppltn <- merge(pubCntByLA_DprvInd_Pub_Indstr, ppltnEst10yrsUniq, by.x="Area code",by.y="Code")

pubCntByLA_DprvInd_PubIndstr_ppltn$Name <- NULL

# read population estimates 2019 -------------------------------------------------


# if by PHE Region --------------------------------------------------------------------------------------------

# Public health england region ----------------------------------------------------


lAuthPHE_Lkup <- fread('data/Local_Authority_to_PHE_Region_Dec_2019_Lookup_Eng.csv', select=c('LAD19CD', 'LAD19NM', 'PHEC19CD', 'PHEC19NM'), data.table=FALSE)

phe_CentresBndryLn <- st_read("data/Public_Health_England_Centres__December_2016__Boundaries.shp")

phe_CentresBndryLn$phec16nm <- NULL
phe_CentresBndryLn$bng_e <- NULL
phe_CentresBndryLn$bng_n <- NULL
phe_CentresBndryLn$st_areasha <- NULL
phe_CentresBndryLn$st_lengths <- NULL
phe_CentresBndryLn$objectid <- NULL


# remove LA codes not in pubCntByLA_DprvInd_PubIndstr_ppltn 
lAuthPHE_Lkup <- lAuthPHE_Lkup[lAuthPHE_Lkup$LAD19CD %in% as.character(pubCntByLA_DprvInd_PubIndstr_ppltn$`Area code`),]

# remove duplicate LA codes 
lAuthPHE_LkupUniq <- lAuthPHE_Lkup[!duplicated(lAuthPHE_Lkup$LAD19CD),]

# merge pubCntByLA_DprvInd_PubIndstr_ppltn LA code with lAuthPHE_LkupUniq lookup 
PHE_centreGrp <- merge(pubCntByLA_DprvInd_PubIndstr_ppltn, lAuthPHE_LkupUniq, by.x="Area code", by.y="LAD19CD")

PHE_centreGrp$LAD19NM <- NULL
#PHE_centreGrp$PHEC19NM <- NULL

PHE_centreGrp[,2:(ncol(PHE_centreGrp) - 2)] <- lapply(PHE_centreGrp[,2:(ncol(PHE_centreGrp) - 2)], as.numeric)


# Public health england region ----------------------------------------------------

# if by PHE Region --------------------------------------------------------------------------------------------


# if by Local Authority -------------------------------------------------------------------

#use casesByLA_DprvInd_PubIndstr_ppltn

# jointWeekNCases_Mltd <- reshape2::melt(casesByLA_DprvInd_Pub_Indstr, id.var = c('lad19_cd', 'lad19_nm', 'IMD_Rank', 'IMD_Decile', 'Income_Rank', 'Income_Decile', 'Employment_Rank', 'Employment_Decile', 'Education_Skills_Training_Rank', 'Education_Skills _Training Decile', 'Health_Deprivation_Disability_Rank', 'Health_Deprivation_Disability_Decile', 'Crime_Rank', 'Crime_Decile', 'Barriers_to_Housing_and_Services_Rank', 'Barriers_to_Housing_and_Services_Decile', 'Living_Environment_Rank', 'Living_Environment_Decile', 'pub_count_2018', 'Retail', 'Transport_Storage_inc_postal', 'Accommodation_food_services', 'Education', 'Health', 'Arts_entertainment_recreation_other_services'), variable.name = 'weekNumber')

# jointWeekNCases_Mltd <- reshape2::melt(PHE_centreGrp, measure.vars = c('wk_05', 'wk_06', 'wk_07', 'wk_08', 'wk_09', 'wk_10', 'wk_11', 'wk_12', 'wk_13', 'wk_14', 'wk_15', 'wk_16', 'wk_17', 'wk_18', 'wk_19', 'wk_20', 'wk_21', 'wk_22', 'wk_23', 'wk_24', 'wk_25', 'wk_26', 'wk_27', 'wk_28', 'wk_29', 'wk_30', 'wk_31', 'wk_32', 'wk_33', 'wk_34', 'wk_35', 'wk_36', 'wk_37', 'wk_38', 'wk_39', 'wk_40', 'wk_41'), variable.name = 'weekNumber')

# names(jointWeekNCases_Mltd)[names(jointWeekNCases_Mltd) == "value"] <- "cvdCaseCount"

# col_idx <- grep("weekNumber", names(jointWeekNCases_Mltd))

# jointWeekNCases_Mltd <- jointWeekNCases_Mltd[, c(col_idx, (1:ncol(jointWeekNCases_Mltd))[-col_idx])]

jointWeekNCases_Mltd <- PHE_centreGrp

col_idx <- grep("PHEC19CD", names(jointWeekNCases_Mltd))

jointWeekNCases_Mltd <- jointWeekNCases_Mltd[, c(col_idx, (1:ncol(jointWeekNCases_Mltd))[-col_idx])]

col_idx <- grep("PHEC19NM", names(jointWeekNCases_Mltd))

jointWeekNCases_Mltd <- jointWeekNCases_Mltd[, c(col_idx, (1:ncol(jointWeekNCases_Mltd))[-col_idx])]



#jointWeekNCases_Mltd[] <- lapply(jointWeekNCases_Mltd, gsub, pattern=',', replacement='')

#jointWeekNCases_Mltd[,6:ncol(jointWeekNCases_Mltd)] <- lapply(jointWeekNCases_Mltd[,6:ncol(jointWeekNCases_Mltd)], as.numeric)

#jointWeekNCases_Mltd <- reshape2::melt(casesByLA_DprvInd_Pub_Indstr, id.var = c('lad19_cd', 'lad19_nm'), variable.name = 'weekNumber')


# get time data ---------------------------------------------------------------------

# unformatWeeks <- colnames(cvdData[, 9:ncol(cvdData)])

# write.csv(x=as.data.frame(unformatWeeks), file="C:\\Installation WorkPCToLaptop\\Dashboard\\cardioDshBCvdDL\\data\\unformatWeeks.csv")

# time month data plus additional cvd model criteria
timeDt <- fread('data/unformatWeeks.csv',  select=c('unformatWeeks', 'LockdownScore', 'TravelSpainEstimate', 'School_Opening', 'QuarantineMeasures'), data.table=FALSE)

## timeDt <- fread('C:\\Installation WorkPCToLaptop\\Dashboard\\cardioDshBCvdDL\\data\\unformatWeeks.csv',  select=c('unformatWeeks', 'Month_Num', 'LockdownScore', 'TravelSpainEstimate', 'School_Opening', 'QuarantineMeasures'), data.table=FALSE)

# Getting latest gov uk cvd dataset case count by local authority ======================================================

# library(ukcovid19)

# query_filters <- c(
    # "areaType=ltla"
# )
# query_structure <- list(
    # date = "date", 
    # name = "areaName", 
    # code = "areaCode", 
    # daily = "newCasesBySpecimenDate"
    # #cumulative = "cumCasesBySpecimenDate"   #cumulative values cannot be summed by week 

# )
# localAuthDataLatestCases <- get_data(filters = query_filters, structure = query_structure)


# query_filters <- c(
    # "areaType=ltla"
# )
# query_structure2 <- list(
    # date = "date", 
    # name = "areaName", 
    # code = "areaCode", 
	# dailyMtly = "newDeathsByDeathDate"
    # #cumulativeMtly = "cumDeathsByDeathDate"
   
# )
# localAuthDataLatestMtly <- get_data(filters = query_filters, structure = query_structure2)


# localAuthDataLatest <- merge(localAuthDataLatestCases, localAuthDataLatestMtly, by=c("code", "name", "date"), all.x = TRUE)
# localAuthDataLatest$cumulative <- NULL
# localAuthDataLatest$cumulativeMtly <- NULL
	
# localAuthDataLatest[is.na(localAuthDataLatest)] <- 0

# localAuthDataLatest$month <- month(as.POSIXlt(localAuthDataLatest$date))

# #convert from daily to weekly 
# library(lubridate) # for the wday() and ymd() functions
# localAuthDataLatest$date <- ymd(localAuthDataLatest$date)
# saturdays <- localAuthDataLatest[wday(localAuthDataLatest$date) == 7, ] # filter for Saturdays
# startDate <- min(saturdays$date) # select first Saturday
# localAuthDataLatest$week <- floor(as.numeric(difftime(localAuthDataLatest$date, startDate, units = "weeks"))) + 5 + 1
# #adjust for weeks by adding 1 (as this dataset starts one week after unformatWeeks); + 5 as unformatWeeks starts at week 5, this one at 1

# #add wk and 0s for single digits
# localAuthDataLatest$week <- sprintf("wk_%02d", localAuthDataLatest$week)

# localAuthDataLatest_Week <-aggregate(. ~ week+code+name, localAuthDataLatest, sum)  #removed month as it causes duplicate weeks 
# localAuthDataLatest_Week$date <- NULL

# #Extend last row by constand values for unformatWeeks 
# minWk <- as.numeric(max(gsub("wk_", "", unique(timeDt$unformatWeeks)))) + 1
		
# maxWk <- as.numeric(max(gsub("wk_", "", unique(localAuthDataLatest_Week$week))))

# diff <- maxWk - minWk + 1

# timeDtExtnd <- rbind(timeDt, transform(timeDt[rep(nrow(timeDt), diff),], unformatWeeks = sprintf("wk_%s", minWk:maxWk)))

				
# #merge unformatWeeks with gov uk mtly and cases data 
# localAuthDataLatest_Week_Policy <- merge(localAuthDataLatest_Week, timeDtExtnd, by.x="week", by.y="unformatWeeks")

# write.csv(x=localAuthDataLatest_Week_Policy, file="data/localAuthDataLatest_Week_Policy.csv")

localAuthDataLatest_Week_Policy <- fread('data/localAuthDataLatest_Week_Policy.csv', data.table=FALSE)

localAuthDataLatest_Week_Policy$V1 <- NULL
localAuthDataLatest_Week_Policy$month <- NULL
localAuthDataLatest_Week_Policy$wk_int <- parse_number(localAuthDataLatest_Week_Policy$week) #using wk integer instead of month as part of training 

# Getting latest gov uk cvd dataset case count by local authority ======================================================


# merge localAuthDataLatest_Week_Policy week number and lad code with jointWeekNCases_Mltd week number and lad code
jointWeekNCases_Mltd_time <- merge(localAuthDataLatest_Week_Policy, jointWeekNCases_Mltd, by.x="code", by.y="Area code", all.x = TRUE)

col_idx <- grep("PHEC19CD", names(jointWeekNCases_Mltd_time))

jointWeekNCases_Mltd_time <- jointWeekNCases_Mltd_time[, c(col_idx, (1:ncol(jointWeekNCases_Mltd_time))[-col_idx])]

col_idx <- grep("PHEC19NM", names(jointWeekNCases_Mltd_time))

jointWeekNCases_Mltd_time <- jointWeekNCases_Mltd_time[, c(col_idx, (1:ncol(jointWeekNCases_Mltd_time))[-col_idx])]

col_idx <- grep("name", names(jointWeekNCases_Mltd_time))

jointWeekNCases_Mltd_time <- jointWeekNCases_Mltd_time[, c(col_idx, (1:ncol(jointWeekNCases_Mltd_time))[-col_idx])]

colnames(jointWeekNCases_Mltd_time)[which(names(jointWeekNCases_Mltd_time) == "name")]  <- c("lad19_nm")

#jointWeekNCases_Mltd_time$name <- NULL

#using the latest local authority code from gov uk api 
colnames(jointWeekNCases_Mltd_time)[which(names(jointWeekNCases_Mltd_time) == "code")]  <- c("lad20_cd")

#Remove NA values 
jointWeekNCases_Mltd_time <- jointWeekNCases_Mltd_time[complete.cases(jointWeekNCases_Mltd_time), ]

# get time data ---------------------------------------------------------------------



# combine with local authority spatial data ----------------------------------------------

#use Ultra local authority 2019 ---------------------------------------

engWLAuthBndryLn <- st_read("data/Local_Authority_Districts__December_2019__Boundaries_UK_BUC.shp")

engWLAuthBndryLn$lad19nmw <- NULL
engWLAuthBndryLn$bng_e <- NULL
engWLAuthBndryLn$bng_n <- NULL
engWLAuthBndryLn$st_areasha <- NULL
engWLAuthBndryLn$st_lengths <- NULL
engWLAuthBndryLn$lad19nm <- NULL
engWLAuthBndryLn$objectid <- NULL
				
				engWLAuthBndryLnSF <- sf::st_as_sf(engWLAuthBndryLn)
				
				engWLAuthBndryLnSF_poly <- st_cast(engWLAuthBndryLnSF,"POLYGON")
				
				# remove LA codes not in jointWeekNCases_Mltd_time 
				engWLAuthBndryLnSF_poly <- engWLAuthBndryLnSF_poly[engWLAuthBndryLnSF_poly$lad19cd %in% as.character(jointWeekNCases_Mltd_time$lad20_cd),]

				# remove duplicate LA codes 
				engWLAuthBndryLnSF_polyUniq <- engWLAuthBndryLnSF_poly[!duplicated(engWLAuthBndryLnSF_poly$lad19cd),]
				
# merge casesByLA_DprvInd_Pub LA code with industrySizeUniq LA code 
jointWeekNCases_Mltd_timeSp <- merge(jointWeekNCases_Mltd_time, engWLAuthBndryLnSF_polyUniq, by.x="lad20_cd",by.y="lad19cd")


# combine with local authority spatial data ----------------------------------------------



# scale data --------------------------------------------------------------------------------------
#dplyr::select_if(jointWeekNCases_Mltd_timeSp, is.numeric)

# jointWeekNCases_Mltd_timeSp[,6:(ncol(jointWeekNCases_Mltd_timeSp) - 1)] <- lapply(jointWeekNCases_Mltd_timeSp[,6:(ncol(jointWeekNCases_Mltd_timeSp) - 1)], as.numeric)

#exclude last column geometry from scaling as it is polygon 

#obtain only numeric columns 
nums <- unlist(lapply(jointWeekNCases_Mltd_timeSp, is.numeric))  
jointWeekNCases_Mltd_timeSp[ , nums]

#preprocess the data by subtracting the mean of each time series and dividing by the standard deviation
#for when some columns have different scale, so make them on same scale (possibly more useful for other features e.g. deprivation indices)
meanTrain <- apply(jointWeekNCases_Mltd_timeSp[ , nums], 2, mean) #only do when other features e.g. deprivation indices have differenr orders of magnitude 
stdTrain <- apply(jointWeekNCases_Mltd_timeSp[ , nums], 2, sd)
jointWeekNCases_Mltd_timeSp[, nums] <- scale(jointWeekNCases_Mltd_timeSp[, nums], center = meanTrain, scale = stdTrain)

jointWeekNCases_Mltd_timeSpSortedScaled <- jointWeekNCases_Mltd_timeSp[order(jointWeekNCases_Mltd_timeSp$week),]

colnames(jointWeekNCases_Mltd_timeSpSortedScaled)[which(names(jointWeekNCases_Mltd_timeSpSortedScaled) == "daily")]  <- c("cvdCaseCount")
colnames(jointWeekNCases_Mltd_timeSpSortedScaled)[which(names(jointWeekNCases_Mltd_timeSpSortedScaled) == "dailyMtly")]  <- c("cvdMtlyCount")

# scale data --------------------------------------------------------------------------------------



locAuthList <- unique(jointWeekNCases_Mltd_timeSpSortedScaled$lad19_nm[!is.na(jointWeekNCases_Mltd_timeSpSortedScaled$lad19_nm)])
 
weekList <- unique(jointWeekNCases_Mltd_timeSpSortedScaled$week[!is.na(jointWeekNCases_Mltd_timeSpSortedScaled$week)])

#weekList <- c(weekList, "wk_42", "wk_43")

# if by Local Authority -------------------------------------------------------------------


# set a random seed for reproducability
set.seed(1)

data_orig <- jointWeekNCases_Mltd_timeSpSortedScaled

#code for performing 75 to 25 split based on local authority code ---------------------------------
#only used to proof model is accurate, not used in enhanced dashboard model 

uniqueLA <- unique(data_orig$lad20_cd)

smp_size <- floor(0.80 * length(uniqueLA))

train_ind <- sample(seq_len(length(uniqueLA)), size = smp_size)

trainInd <- uniqueLA[train_ind] 

train <- data_orig[data_orig$lad20_cd %in% trainInd, ]
validatn <- data_orig[!(data_orig$lad20_cd %in% trainInd), ]

data_orig_TrainVal <- rbind(train, validatn)   #use index to separate training from validation, otherwise training cannot continue

#code for performing 75 to 25 split based on local authority code ---------------------------------

laInd <- 0
#laIndVal <- 0 #validation index 

# lookback 4. For training only 
generator <- function(data, lookback = 3, delay = 1, min_index = 1, max_index = 10,
                      shuffle = FALSE, batch_size = 7, step = 1) {
  
  function() {  #laInd has to be within inner function for it to increment 
   
  data <- data[min_index: max_index, ]  #code for performing 75 to 25 split based on local authority code
   
  uniqueLA <- unique(data$lad20_cd)
  
	  if(is.null(laInd)){
		   laInd <<- 0
	  }
	  
	  if (laInd > length(uniqueLA)) {
		   laInd <<- 1
	  } else {
		   laInd <<- laInd + 1
	  }    
	  
	  randomLACode <- uniqueLA[laInd]   #randomly select a Local Authorty code 
  
  
  data_pre <- data[data$lad20_cd == randomLACode, ]  #get only data for this Local Authority code 

  # remove duplicate weeks
  #data_pre <- data_pre[!duplicated(data_pre$week),] #not required after removing month 

  #obtain only numeric columns 
  nums <- unlist(lapply(data_pre, is.numeric))  
  
  dataNum <- data_pre[ , nums]     #remove non-numeric columns 

  if (is.null(max_index))
    max_index <- nrow(dataNum) - delay - 1
  #i <- min_index + lookback
  i <- min_index 
 
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)  #+lookback ensures that index does not go before 1 when we are looking back
	  #This generates a different sample index of batch size each time function is used 
	  #rows <- sample(c((1+3):10), size = 5)
    } else {
      # if (i + batch_size > max_index + 2)
        # i <<- min_index + lookback
      # rows <- c(i:min(i+batch_size-1, max_index))
      # i <<- i + length(rows) - 1
	  
	  exclParam <- delay
	  rows <- (lookback+1):(nrow(dataNum) - exclParam)   #exclParam = 6 to exclude last three wks from training when lookforward or delay is 3. exclParam = lookforward or delay, when no data is excluded from training 
    }
	
	#samples <- array(0, dim = c(length(rows), 3 / 1, dim(dataNum)[[-1]])) 
    samples <- array(0, dim = c(length(rows),
                                2 / step,   #lookback /step
                                dim(dataNum)[[-1]]))  
    #targets <- array(0, dim = c(length(rows), 2))   #column 1 for cases; column 2 for mtly
                    
	target1 <- array(0, dim = c(length(rows)))  
    target2 <- array(0, dim = c(length(rows)))  	
					
	#get column index of cvdCaseCount				
	cvdCasesColIndex <- grep("cvdCaseCount", colnames(dataNum))
	
	#get column index of cvdMtlyCount				
	cvdMtlyColIndex <- grep("cvdMtlyCount", colnames(dataNum))
	
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]],
                     length.out = dim(samples)[[2]])  # obtains indices from a random value in between lookback and max_index take away lookback in steps up to that value #changed from dim(samples)[[2]] to 3
      samples[j,,] <- as.matrix(dataNum[indices,])   #change dataframe to matrix i.e. dataNum[indices,] to as.matrix(dataNum[indices,])
      # targets[j] <- dataNum[rows[[j]] + delay, cvdCasesColIndex]  # indice of random value plus look forward (delay e.g. 24hrs) obtains column value for look forward dataNum # cvdCases is column 128
	  
	  target1[j] <- dataNum[rows[[j]] + delay, cvdCasesColIndex]  # indice of random value plus look forward (delay e.g. 24hrs) obtains column value for look forward dataNum # cvdCases is column 128
	  
	  target2[j] <- dataNum[rows[[j]] + delay, cvdMtlyColIndex]
    } 
	targets <- list(target1, target2)
	list(samples, targets)
  }
}


# # Model 1: 0.02 mae GRU 2 layers ---------------------------------------------------------------

lookback <- 1 
step <- 1
delay <- 5 	
batch_size <- 7 

#training generator looks at the first half of the timesteps i.e. 37 / 2 = 18
train_gen <- generator(
  data_orig_TrainVal,
  lookback = lookback,
  delay = delay,
  min_index = 1,
  max_index = 9033,		#increase from 10 to 18 # changed from 26 to 36 for implementation rather than evaluation
  shuffle = FALSE,
  step = step, 
  batch_size = batch_size	#increase from 5 to 12 to increase sample size 
)

#validation generator looks at the following 1 / 4 of timesteps i.e. 37 / 4 = 9.25 i.e 19 to 28 # changed to from 27 to 37 as test is not needed 
val_gen <- generator(
  data_orig_TrainVal,
  lookback = lookback,
  delay = delay,
  min_index = 9034,
  max_index = 11299,
  step = step,
  batch_size = 5
)

# #test generator looks at the remainder from 29 to 37. This is not really needed 
# test_gen <- generator(
  # data_orig,
  # lookback = lookback,
  # delay = delay,
  # min_index = 28,
  # max_index = 36,
  # step = step,
  # batch_size = 6
# )

# Model 1: 0.02 mae GRU 2 layers ---------------------------------------------------------------


# Model 2: without 75 to 25 split ---------------------------------------------------------------

# lookback <- 1 
# step <- 1
# delay <- 3 	 #changed from 3 to 5 
# batch_size <- 7 

# #training generator looks at the first half of the timesteps i.e. 37 / 2 = 18
# train_gen <- generator(
  # data_orig,
  # lookback = lookback,
  # delay = delay,
  # min_index = 1,
  # max_index = 37,		#Index does not really matter in this case as we use all rows 
  # shuffle = FALSE,
  # step = step, 
  # batch_size = batch_size	#batch size is irrelevant in this case as we use all rows 
# )

# #validation generator looks at the following 1 / 4 of timesteps i.e. 37 / 4 = 9.25 i.e 19 to 28 # changed to from 27 to 37 as test is not needed 
# val_gen = generator(
  # data_orig,
  # lookback = lookback,
  # delay = delay,
  # min_index = 1,
  # max_index = 37, 	#Index does not really matter in this case as we use all rows 
  # step = step,
  # batch_size = batch_size	#batch size is irrelevant in this case as we use all rows 
# )

# #test generator looks at the remainder from 29 to 37. This is not really needed 
# test_gen <- generator(
  # data_orig,
  # lookback = lookback,
  # delay = delay,
  # min_index = 28,
  # max_index = 36,
  # step = step,
  # batch_size = 6
# )

# Model 2: without 75 to 25 split ---------------------------------------------------------------



# How many steps to draw from val_gen in order to see the entire validation set. Only 0.85 as dataset is small 
# val_steps <- (28 - 19 - lookback) / 7
# val_steps <- 1



# library(keras)
# evaluate_naive_method <- function() {
  # batch_maes <- c()
  # for (step in 1:val_steps) {
    # c(samples, targets) %<-% val_gen()
    # preds <- samples[,dim(samples)[[2]],1]
    # mae <- mean(abs(preds - targets))  #naive baseline assuming last week is same as next week 
    # batch_maes <- c(batch_maes, mae)
  # }
  # print(mean(batch_maes))
# }

# evaluate_naive_method()


# num_samples <- length(weekList) - 6
# stepsPerEpoch <- ceiling(num_samples / batch_size)

#stepsPerEpoch <- 1

# val_steps <- 1

# new_gen <- keras:::as_generator.function(train_gen)
# new_genVal <- keras:::as_generator.function(val_gen)



#Already trained
# Trying the GRU model with recurrent dropouts and stacked recurrent layer instead 
# model <- keras_model_sequential() %>% 
  # layer_gru(units = 32, 
            # dropout = 0.1, 
            # recurrent_dropout = 0.5,
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
            # return_sequences = TRUE,
            # input_shape = list(NULL, dim(data[ , 6:(ncol(data) - 1)])[[-1]])) %>%  #change from dim(data)[-1]) to dim(data[ , 4:(ncol(data_pre) - 1)])[[-1]]
  # layer_gru(units = 64, activation = "relu",  
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
            # dropout = 0.1,
            # recurrent_dropout = 0.5) %>% 
  # layer_dense(units = 1)

# model %>% compile(
  # optimizer = optimizer_rmsprop(),
  # loss = "mae"
# )

# Trying the GRU model with recurrent dropouts and stacked recurrent layer instead 
# model <- keras_model_sequential() %>% 
  # layer_lstm(units = 128, 
            # #dropout = 0.1, 
            # #recurrent_dropout = 0.5,
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
            # return_sequences = TRUE,
            # input_shape = list(NULL, dim(data[ , 6:(ncol(data) - 1)])[[-1]])) %>%  #change from dim(data)[-1]) to dim(data[ , 4:(ncol(data_pre) - 1)])[[-1]]
  # layer_lstm(units = 512, activation = "relu",  
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
			# return_sequences = TRUE,
            # #dropout = 0.1,
            # #recurrent_dropout = 0.5
			# ) %>%
  # layer_lstm(units = 256, activation = "relu",  
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
			# return_sequences = TRUE,
            # #dropout = 0.1,
            # #recurrent_dropout = 0.5
			# ) %>% 			
  # layer_dense(units = 1, activation = "linear")

# model %>% compile(
  # optimizer = optimizer_adam(
  # lr = 0.01),
  # loss = 'mse',
  # metrics = list('mae') 
# )

# model %>% compile(
  # optimizer = optimizer_sgd(
  # lr = 0.01),
  # loss = "mae"
# )

# model %>% compile(
  # optimizer = optimizer_rmsprop(lr = 0.05),
  # loss = "mae"
# )

# nums <- unlist(lapply(data_orig, is.numeric))  
  
# data_orig_Num <- data_orig[ , nums]

#updated model to include cases and mtly from gov uk api
# model <- keras_model_sequential() %>% 
  # layer_gru(units = 32, 
            # dropout = 0.1, 
            # recurrent_dropout = 0.5,
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
            # return_sequences = TRUE,
            # input_shape = list(NULL, dim(data_orig_Num)[[-1]])) %>%  #change from dim(data)[-1]) to dim(data[ , 4:(ncol(data_pre) - 1)])[[-1]]
  # layer_gru(units = 64, activation = "relu",  
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
            # dropout = 0.1,
			# return_sequences = TRUE,
            # recurrent_dropout = 0.5) %>% 
  # layer_dense(units = 1)

# model %>% compile(
  # optimizer = optimizer_rmsprop(),
  # loss = "mae"
# )

# Functional API for predicting 2 outputs ------------------------------------------------------------

num_samples <- length(weekList) - 6
stepsPerEpoch <- ceiling(num_samples / batch_size)

val_steps <- 1

new_gen <- keras:::as_generator.function(train_gen)
new_genVal <- keras:::as_generator.function(val_gen)

nums <- unlist(lapply(data_orig, is.numeric))  
data_orig_Num <- data_orig[ , nums]

#main_input <- layer_input(shape = list(NULL, dim(data_orig_Num)[[-1]]), name = 'main_input')

# gru_out <- main_input %>%
  # layer_gru(units = 32,
			# dropout = 0.1, 
            # recurrent_dropout = 0.5,
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
            # return_sequences = TRUE) 
			
# auxiliary_output <- gru_out %>% 
  # layer_gru(units = 64, activation = "relu",  
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
            # dropout = 0.1,
			# return_sequences = TRUE,
            # recurrent_dropout = 0.5) %>%			
			# layer_dense(units = 1, name = 'aux_output')
			
			
# main_output <- gru_out %>% 
  # layer_gru(units = 64, activation = "relu",  
			# # kernel_regularizer = regularizer_l1_l2(l1 = 0.001, l2 = 0.001),
            # dropout = 0.1,
			# return_sequences = TRUE,
            # recurrent_dropout = 0.5) %>%			
			# layer_dense(units = 1, name = 'main_output')
		
		
# model <- keras_model(
  # inputs = c(main_input),
  # outputs = c(main_output, auxiliary_output)
# )


# model %>% compile(
  # optimizer = optimizer_rmsprop(),
  # loss = "mae"
# )
# # Functional API for predicting 2 outputs ------------------------------------------------------------

# history <- model %>% fit_generator(
  # new_gen,
  # steps_per_epoch = stepsPerEpoch,
  # callbacks = list(
  # callback_model_checkpoint('model_trainingOnly/weights.{epoch:02d}-{val_loss:.5f}.hdf5', monitor = "val_loss", verbose = 0, save_best_only = TRUE, save_weights_only = FALSE, mode = "min", period = NULL, save_freq = "epoch"), callback_early_stopping(monitor = "val_loss", mode = "min", min_delta = 0.001, patience=200, restore_best_weights = TRUE)),
  # epochs = 500,
  # validation_data = new_genVal,
  # validation_steps = val_steps
# )



#Model 1 -----------------------------------------------------

#model <- load_model_hdf5("C:\\Installation WorkPCToLaptop\\Dashboard\\cardioDshBCvdDL\\model\\model1\\weights.58-0.02_4_500epochs.hdf5")
#4x500 epochs 

#Model 1 -----------------------------------------------------


#Model 2 -----------------------------------------------------

#model <- load_model_hdf5("C:\\Installation WorkPCToLaptop\\Dashboard\\cardioDshBCvdDL\\model\\model4\\weights.02-0.1290_2_miniEpoch500.hdf5")
#4x500 epochs 

#Model 2 -----------------------------------------------------

#Model 5 -----------------------------------------------------

#model <- load_model_hdf5("model_trainingOnly/model5wksAhead/weights.93-0.23380_1_500epochsEarlyStop.hdf5")

#Model 5 -----------------------------------------------------



#Model 4 -----------------------------------------------------

#current optimal for 3 weeks ahead
#model <- load_model_hdf5("model_trainingOnly/model4/weights.02-0.1290_2_miniEpoch500_delay3.hdf5")

#Model 4 -----------------------------------------------------


#Model current -----------------------------------------------------

modelMaster <<- load_model_hdf5("data/weights.05-0.17456_4_500epochsEarlyStop.hdf5")

model <<- load_model_hdf5("data/weights.05-0.17456_4_500epochsEarlyStop.hdf5")

#Model current -----------------------------------------------------

#continued training
# history <- model %>% fit_generator(
  # new_gen,
  # steps_per_epoch = stepsPerEpoch,
  # callbacks = list(
  # callback_model_checkpoint('C:\\Installation WorkPCToLaptop\\Dashboard\\cardioDshBCvdDL\\model\\AdditionalVariables\\weights.{epoch:02d}-{val_loss:.2f}.hdf5', monitor = "val_loss", 
  # verbose = 0, save_best_only = TRUE, save_weights_only = FALSE, mode = "min", period = NULL, save_freq = "epoch")),
  # epochs = 500,
  # validation_data = new_genVal,
  # validation_steps = val_steps
# )



# load UK map -----------------------------------------------------

uk <- st_read("data/gadm36_GBR_2.shp")

# load UK map -----------------------------------------------------


# opValues <- cardioData$X3.02.Procedure.Date

# cardioData$opDate <- as.Date(opValues, format = "%d/%m/%Y")

# rm(opValues)

# #hospList <- unique(cardioData$hospital[!is.na(cardioData$hospital)])


# priorityList <- unique(cardioData$X2.35.Operative.Urgency[!is.na(cardioData$X2.35.Operative.Urgency)])

# priorityList <- priorityList[stri_detect_fixed(priorityList , ".")]

# priorityList <- mixedsort(priorityList)

#HospCountryList <- unique(cardioData$Hosp.Country[!is.na(cardioData$Hosp.Country)])

#yearList <- unique(cardioData$year[!is.na(cardioData$year)])

#sd = as.Date("2009-01-01")
#ed = as.Date("2018-01-01")  

#yearList <- substr(seq(sd, ed, "years"), 1, 4)

#cardioData <- cardioData[cardioData$year %in% yearList, ]  


#dropdown button appearance
dropdownButton <- function(label = "", status = c("default", "primary", "success", "info", "warning", "danger"), ..., width = NULL) {

  status <- match.arg(status)
  # dropdown button content
  html_ul <- list(
    class = "dropdown-menu",
    style = if (!is.null(width)) 
      paste0("width: ", validateCssUnit(width), ";"),
    lapply(X = list(...), FUN = tags$li, style = "margin-left: 10px; margin-right: 10px;")
  )
  # dropdown button apparence
  html_button <- list(
    class = paste0("btn btn-", status," dropdown-toggle"),
    type = "button", 
    `data-toggle` = "dropdown"
  )
  html_button <- c(html_button, list(label))
  html_button <- c(html_button, list(tags$span(class = "caret")))
  # final result
  tags$div(
    class = "dropdown",
    do.call(tags$button, html_button),
    do.call(tags$ul, html_ul),
    tags$script(
      "$('.dropdown-menu').click(function(e) {
      e.stopPropagation();
});")
  )
  }


colorSet2 <- c("blue","green","brown4","red","salmon","#7CD3CF","purple","#D29766","orange","cyan", "darkturquoise", "#D64BD6")
library(RColorBrewer)
n <- 45
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
colorSet3 <- sample(col_vector, n)

colorSet2 <- c(colorSet2, colorSet3, "black", "black")



lsoaGrpSF_longLat <- NULL


# Getting latest gov uk cvd dataset case count by local authority -------------------------------------------

# library(ukcovid19)

# query_filters <- c(
    # "areaType=ltla"
# )
# query_structure <- list(
    # date = "date", 
    # name = "areaName", 
    # code = "areaCode", 
    # daily = "newCasesBySpecimenDate",
    # cumulative = "cumCasesBySpecimenDate"
# )
# localAuthDataLatest <- get_data(filters = query_filters, structure = query_structure)



# library(lubridate) # for the wday() and ymd() functions
# localAuthDataLatest$date <- ymd(localAuthDataLatest$date)
# saturdays <- localAuthDataLatest[wday(localAuthDataLatest$date) == 7, ] # filter for Saturdays
# startDate <- min(saturdays$date) # select first Saturday
# localAuthDataLatest$week <- floor(as.numeric(difftime(localAuthDataLatest$date, startDate, units = "weeks"))) + 5 + 1
# #adjust for weeks by adding 1 (as this dataset starts one week after unformatWeeks); + 5 as unformatWeeks starts at week 5, this one at 1

# #add wk and 0s for single digits
# localAuthDataLatest$week <- sprintf("wk_%02d", localAuthDataLatest$week)

# localAuthDataLatest_Week <-aggregate(. ~ week+code+name, localAuthDataLatest, sum)

# write.csv(x=localAuthDataLatest_Week, file="C:\\Installation WorkPCToLaptop\\Dashboard\\cardioDshBCvdDL\\data\\localAuthDataLatest_Week.csv")

#localAuthDataLatest_Week <- fread('C:\\Installation WorkPCToLaptop\\Dashboard\\cardioDshBCvdDL\\data\\localAuthDataLatest_Week.csv', data.table=FALSE)

# Getting latest gov uk cvd dataset case count by local authority -------------------------------------------



ui <- function(){
  addResourcePath("data", "data")
  navbarPage(title=div(img(src="data/logoNCGFS.png")), selected = "", collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
             tabPanel("",
                      fluidPage(
						tags$head(tags$style(HTML('
							ul.dropdown-menu {
							padding: 10px;
							width: 50px;
							min-width: 142px;
							} 
							.dropdown-toggle {
							padding:8px;							
							}
							.navbar-header {
							height: 9em;
							}

							.shiny-notification {
							  height: 50px;
							  width: 800px;
							  position:fixed;
							  top: calc(50% - 50px);;
							  left: calc(50% - 400px);;
							}
							body {padding-top: 10.8em;}
							
							'))),
						
					
                        # tabsetPanel(
                          # tabPanel("General Metrics", br(),
                                   sidebarLayout(
                                     sidebarPanel(style = "overflow-y:scroll; max-height: 600px; position:relative;",
										  useShinyjs(),
										  h4(strong("Main Criteria")),
										  div(style="display:inline-block", dropdownButton(
											label = "Select Location", status = "default",
											
											actionButton(inputId = "grp", label = "(Un)group", width= 80, style='padding:4px; font-size:80%'),
											div(style = "margin-top: 5px; margin-bottom: 5px;", 
											actionButton(inputId = "all", label = "(Un)select all", width= 80, style='padding:4px; font-size:80%')
											),
											uiOutput('locAuthCheckbx')
											) ),
										  # div(style="display:inline-block", dropdownButton(
											# label = "Select Week", status = "default",
											# actionButton(inputId = "allYr", label = "(Un)select all", width= 80, style='padding:4px; font-size:80%'),
											# # uiOutput('wksCheckbx')
											# checkboxGroupInput(inputId = "checkWeek", label = "Choose", choices = weekList, selected = weekList)
											# ) ), br(),	
										  # div(style="display:inline-block", dropdownButton(
											# label = "Select Actual Week", status = "default",
											# actionButton(inputId = "allYr", label = "(Un)select all", width= 80, style='padding:4px; font-size:80%'),
											# uiOutput('wksCheckbx')
											# #checkboxGroupInput(inputId = "checkWeek", label = "Choose", choices = weekList, selected = weekList)
											# ) ),
										  div(style="display:inline-block; margin-top: 5px;", dropdownButton(
											label = shiny::HTML("<span style='font-size: 90%;'>Plot Type</span>"), status = 'default',
											
											actionButton(inputId = "grpRegion", label = "(LAD)PHE", width= 87, style='padding:4px; font-size:80%'), 
											div(style = "width: 1px;", 
											# radioButtons(inputId = "checkSgryType", label = "Choose", choices = list("Overall Activity" ="Overall_Activity" , "Aortic Valve" = "AorticValveSurgery", "Mitral Valve" = "MitralValveSurgery", "Thoracic Aorta Surgery" = "Surgery_on_thoracic_aorta", "Isolated CABG" = "isolatedCABG"))
											) 											
											) ), br(), br(),
										  sliderInput(inputId = "checkWeek", label = "Select Weeks", parse_number(min(weekList)), parse_number(max(weekList)), value = c(40, 46), step = 1, animate = TRUE) %>% helper(type = "inline", title = "Select Weeks",content = c("Used for Configure Model mode. Select a week or a range of weeks for which changes to National Metrics or Local Metrics will apply when Load/Update Model is clicked. There is a five week delay between when the changes are applied to when the changes are seen in the predictions. For example, when simulating a full lock down from week 45 (00.01 on Thursday 5 November), the full lock down changes should be applied from week 40 onwards rather than from week 45 onwards.")),
										  # hidden(div(style="display:inline-block; padding:5px;", dropdownButton(
											# label = shiny::HTML("<span style='font-size: 90%;'>Select Subtype</span>"), status = 'default',
											# actionButton(inputId = "allYr", label = "(Un)select all", width= 80, style='padding:4px; font-size:80%'),
											# div(style = "width: 1px;", 
											# radioButtons(inputId = "checkSubType", label = "Choose", choices = "NA")
											# ) 
											# ))),
											# #verbatimTextOutput(outputId = "res2"),
											# br(), br(),
									   h4(strong("Other Criteria")), 	
									   div(style="margin-top: 5px;",
									   materialSwitch(inputId = "simulation", label = "Configure Model", status = "success")) %>% helper(type = "inline", title = "Configure Model",content = c("When configure model mode is selected, the system will plot based on the configuration changes made to both the National and Local Metrics. Otherwise, system will plot based on predictions assuming metrics stay constant from week 41 onwards.")), 
									   materialSwitch(inputId = "nationalSetting", label = "National", status = "primary") %>% helper(type = "inline", title = "National",content = c("When National mode is not selected, any additional local authorities selected will result in the recalculation of the Local Metrics using the mean of the values for the selected local authorities. This would result in the user's configuration changes of Local Metrics to be lost. However, when National mode is selected, Local Metrics will not change upon changes to the selection of local authorities.")), 
									   materialSwitch(inputId = "policyScientist", label = "Policy Maker", status = "info") %>% helper(type = "inline", title = "Policy Maker",content = c("When selected, the policy maker mode is activated and presents only the predicted weeks that have not been covered with actual data. When not selected, scientist mode is activated and presents predictions for all weeks included weeks that contain actual data. This provides insight into the performance of prediction model.")), 
									   actionButton(inputId = "loadModelButtonId", label =  shiny::HTML("<span style='font-size: 95%;'>Load/Update Model</span>"), width= 135, style='padding:4px; font-size:90%'),
									   # sliderInput(inputId = "growthRate", label = "Growth Rate", min(jointWeekNCases_Mltd_time$Growth_Rate, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Growth_Rate, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$Growth_Rate, na.rm = TRUE), round = 2, animate = TRUE),
									   # sliderInput(inputId = "rNumber", label = "R Number", min(jointWeekNCases_Mltd_time$R_number, na.rm = TRUE), max(jointWeekNCases_Mltd_time$R_number, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$R_number, na.rm = TRUE), round = 2, animate = TRUE),
									   #hidden(div(id="processPlotText", h5(strong("Process Plot"))) ),
									   # hidden(div(id="TrendButtonCases", style="display:inline-block; margin-top: 5px;", actionButton(inputId = "trendProcessId", label =  shiny::HTML("<span style='font-size: 90%;'>Process Trend Model</span>"), width= 110, style='padding:4px; font-size:80%')) ), 
									   # hidden(div(id="MapButtonCases", style="display:inline-block; margin-top: 5px;", actionButton(inputId = "mapProcessId", label =  shiny::HTML("<span style='font-size: 90%;'>Process Map Model</span>"), width= 105, style='padding:4px; font-size:80%')) ), br(),
									   
									   div(style="margin-top: 30px;", h4(strong("National Metrics"))),
									   actionButton(inputId = "resetPrmtrButtonId", label =  shiny::HTML("<span style='font-size: 95%;'>Reset National Metrics</span>"), width= 135, style='padding:4px; font-size:90%'),br(),br(),
					   				   uiOutput("sliderSpain"), 
									   # sliderInput(inputId = "TravelSpain", label = "Travel To Spain", min(jointWeekNCases_Mltd_time$TravelSpainEstimate, na.rm = TRUE), max(jointWeekNCases_Mltd_time$TravelSpainEstimate, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$TravelSpainEstimate, na.rm = TRUE), round = 2, animate = TRUE),
									   uiOutput("sliderLockDown"),
									   # sliderInput(inputId = "LockDown", label = "Inverse Lock Down Score", min(jointWeekNCases_Mltd_time$LockdownScore, na.rm = TRUE), max(jointWeekNCases_Mltd_time$LockdownScore, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$LockdownScore, na.rm = TRUE), step = 0.1, animate = TRUE),
									   uiOutput("sliderQrntnRestrctn"),
									   # sliderInput(inputId = "travelQrntnRestrctn", label = "Inverse Travelling Quarantine Restrictions", min(jointWeekNCases_Mltd_time$QuarantineMeasures, na.rm = TRUE), max(jointWeekNCases_Mltd_time$QuarantineMeasures, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$QuarantineMeasures, na.rm = TRUE), round = 2, animate = TRUE),
									   uiOutput("sliderOpenSchool"),
									   # sliderInput(inputId = "openSchool", label = "Open School", min(jointWeekNCases_Mltd_time$School_Opening, na.rm = TRUE), max(jointWeekNCases_Mltd_time$School_Opening, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$School_Opening, na.rm = TRUE), round = 2, animate = TRUE),
									   h4(strong("Local Metrics")),
									   actionButton(inputId = "resetLocalPrmtrId", label =  shiny::HTML("<span style='font-size: 95%;'>Reset Local Metrics</span>"), width= 115, style='padding:4px; font-size:90%'),br(),br(),
									   uiOutput("sliderIntrntlInflow"),
									   # sliderInput(inputId = "intrntlInflow", label = "International Inflow", min(jointWeekNCases_Mltd_time$`International Migration Inflow`, na.rm = TRUE), max(jointWeekNCases_Mltd_time$`International Migration Inflow`, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$`International Migration Inflow`, na.rm = TRUE), round = 2, animate = TRUE),
									   uiOutput("sliderPubSize"),
									   # sliderInput(inputId = "pubSize", label = "Pub size", min(jointWeekNCases_Mltd_time$pub_count_2018, na.rm = TRUE), max(jointWeekNCases_Mltd_time$pub_count_2018, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$pub_count_2018, na.rm = TRUE), round = 2, animate = TRUE),
									   uiOutput("sliderRetailShops"),
									   # sliderInput(inputId = "retailShops", label = "Retail Shops", min(jointWeekNCases_Mltd_time$Retail, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Retail, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$Retail, na.rm = TRUE), round = 2, animate = TRUE),									
									   uiOutput("sliderInternalMigNet"),
									   # sliderInput(inputId = "internalMigNet", label = "Internal Migration Net", min(jointWeekNCases_Mltd_time$`Internal Migration Net`, na.rm = TRUE), max(jointWeekNCases_Mltd_time$`Internal Migration Net`, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$`Internal Migration Net`, na.rm = TRUE), round = 2, animate = TRUE),
									   uiOutput("sliderEductnSize"),
									   # sliderInput(inputId = "eductnSize", label = "Number of Education Establishments", min(jointWeekNCases_Mltd_time$Education, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Education, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$Education, na.rm = TRUE), round = 2, animate = TRUE),
									   uiOutput("sliderAccmNFoodSize"),
									   # sliderInput(inputId = "AccmNFoodSize", label = "Number of Accomodation and Food services", min(jointWeekNCases_Mltd_time$Accommodation_food_services, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Accommodation_food_services, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$Accommodation_food_services, na.rm = TRUE), round = 2, animate = TRUE),
									   uiOutput("sliderTransprtStrgSize"),
									   # sliderInput(inputId = "TransprtStrgSize", label = "Number of Transport and Storage services", min(jointWeekNCases_Mltd_time$Transport_Storage_inc_postal, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Transport_Storage_inc_postal, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$Transport_Storage_inc_postal, na.rm = TRUE), round = 2, animate = TRUE),
									   uiOutput("sliderArtEntertmntRcrtn"),
									   # sliderInput(inputId = "artEntertmntRcrtn", label = "Number of Art, Entertainment & Recreational Services", min(jointWeekNCases_Mltd_time$Arts_entertainment_recreation_other_services, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Arts_entertainment_recreation_other_services, na.rm = TRUE), value = median(jointWeekNCases_Mltd_time$Arts_entertainment_recreation_other_services, na.rm = TRUE), round = 2, animate = TRUE),
									   
									   									   
									   # div(style="display:inline-block; margin-top: 5px;", dropdownButton(
											# label = shiny::HTML("<span style='font-size: 90%;'>Outcome Type</span>"), status = 'default',
											# div(style = "margin-top: 5px; margin-bottom: 5px;", 
											# actionButton(inputId = "grpOutcome", label = "(Un)group type", width= 87, style='padding:4px; font-size:80%')
											# ), 
											# div(style = "width: 1px;", 
											# radioButtons(inputId = "checkOutcomeType", label = "Choose", choices = list("Overall Activity" = "Overall_Activity", "Mortality" ="death" , "SWI" = "SWI", "Postop CVA" = "post_opCVA", "Postop Dialysis" = "postop_dialysis"))
											# ) 											
											# ) ),
									   # div(style="display:inline-block; margin-top: 15px;", radioButtons(
                                         # inputId = "plot1",
                                         # label = "Choose a plot type to explore",
                                         # choices = c("Trend: expected and observed cases", "Heat Map: Number of Cases")) )
										 
										), 
								  mainPanel( 
									 tabsetPanel(id="tabs1",
										tabPanel("Overview", br(),textOutput(outputId = "message"), 				
											uiOutput("IntroText"), br(),
										),
									    tabPanel("Trend", br(),
										   hidden(div(id="TrendButtonCases", style="display:inline-block; margin-top: 5px;", actionButton(inputId = "trendProcessId", label =  shiny::HTML("<span style='font-size: 90%;'>Process Cases Trend</span>"), width= 110, style='padding:4px; font-size:80%')) ), 
										   plotOutput(outputId = "plot_access_web", height = "600px"), br(), br(),
										   uiOutput("MtlyInstrctn"), br(),
										   div(id="TrendButtonMtly", style="display:inline-block; margin-top: 5px;", actionButton(inputId = "trendProcessMtlyId", label =  shiny::HTML("<span style='font-size: 90%;'>Process Mortality Trend</span>"), width= 120, style='padding:4px; font-size:80%')),
										   plotOutput(outputId = "plot_mtly", height = "600px")

									    ),
									    tabPanel("Map: Cases", br(),
										   hidden(div(id="MapButtonCases", style="display:inline-block; margin-top: 5px;", actionButton(inputId = "mapProcessId", label =  shiny::HTML("<span style='font-size: 90%;'>Process Cases Map</span>"), width= 105, style='padding:4px; font-size:80%')) ), br(), br(),
										   div(leafletOutput(outputId='map'), div(textOutput(outputId = 'summary'))), br(), br(),
										   div(leafletOutput(outputId='mapSim'))
										   
										
										),
									    tabPanel("Map: Mortality", br(),
										   hidden(div(id="MapMtlyButtonCases", style="display:inline-block; margin-top: 5px;", actionButton(inputId = "mapMtlyProcessId", label =  shiny::HTML("<span style='font-size: 90%;'>Process Mortality Map</span>"), width= 115, style='padding:4px; font-size:80%')) ), br(), br(),
										   div(leafletOutput(outputId='mapMtly')), br(), br(),
										   div(leafletOutput(outputId='mapSimMtly'))
										   
										
										)
										
										
										

                                   )
									  
								   
								   
								   )
               
                        ))), position = c("fixed-top")

  ) }

#ui <- fluidPage(leafletOutput("map"))

server <- function(input, output, session) {
  
  
  observeEvent(input$travelSpainHelpId,{
    showModal(modalDialog(
      title = "Help!",
      "Information",
      textInput('text2', 'You can also put UI elements here')
    ))
  })

# output$res2 <- renderPrint({
				   # paste0(input$checkWeek, " ", class(input$checkWeek), unlist(input$checkWeek, use.names=FALSE) )
					
				  # })
				  		

#observeEvent(input$plot1, { 
#observe( { 

  #globalWk <- reactiveValues(weeks = weekList)
  # globalWk <- reactiveValues()
  # globalWk$weeks <- weekList
  #globalLA <- reactiveValues(lclAuth = NULL)
  globalPrty <- reactiveValues(prty = NULL)


  if (!is.null(lsoaGrpSF_longLat)) {
	  rm(lsoaGrpSF_longLat)
	  gc();
  }
  
  # Select all / Unselect all
  observeEvent(input$all, {
    if (is.null(input$check2)) {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", selected = locAuthList
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "check2", selected = ""
      )
    }
  })
  
  # Select all / Unselect all
  # observeEvent(input$allYr, {
    # if (is.null(input$checkWeek)) {
      # updateCheckboxGroupInput(
        # session = session, inputId = "checkWeek", selected = weekList
      # )
    # } else {
      # updateCheckboxGroupInput(
        # session = session, inputId = "checkWeek", selected = ""
      # )
    # }
  # })
  
  
  # Select all / Unselect all
  observeEvent(input$allPrty, {
    if (is.null(input$checkPrty)) {
      updateCheckboxGroupInput(
        session = session, inputId = "checkPrty", selected = priorityList
      )
    } else {
      updateCheckboxGroupInput(
        session = session, inputId = "checkPrty", selected = ""
      )
    }
  })

  
  flag <- reactiveValues(grpLocation = FALSE)
   
  # Select all / Unselect all
  observeEvent(input$grp, {
    if (is.null(input$check2)) {
      
    } else {
	  #toggle between conditions TRUE and false 
      flag$grpLocation <- !flag$grpLocation 
    }
  })
  
  flagType <- reactiveValues(grp_Type = FALSE)
   
  # Select all / Unselect all
  observeEvent(input$grpType, {
	if (is.null(input$check2)) {
      
    } else {
	  #toggle between conditions TRUE and false 
      flagType$grp_Type <- !flagType$grp_Type 
    }
  })
  
  #flagPr <- reactiveValues(processButton = FALSE)
   
  # Select all / Unselect all
  # observeEvent(input$processId, {
	 # if (is.null(input$check2)) {
      
    # } else {
	  # #toggle between conditions TRUE and false 
      # flagPr$processButton <- !flagPr$processButton 
	# }
  # })
  
  
   flagOutcome <- reactiveValues(grp_Outcome = FALSE)
   
   
   # Select all / Unselect all
   observeEvent(input$grpOutcome, {
  
	  #toggle between conditions TRUE and false 
      flagOutcome$grp_Outcome <- !flagOutcome$grp_Outcome 
    
   })
  
  # observeEvent(input$plot1, {
  # #clear space from empty leaflet map 
  # output$condition <- renderText({

		# #ifelse(input$plot1 == "Heat Map: Case Density", 1, 0)
	  
		# if(input$plot1 == "Heat Map: Case Density") {
			 # 1  
		# } else if (input$plot1 == "Heat Map: Number of Cases") {
			 # 1
		# } else if (input$plot1 == "Trend: total number of cases") {
			 # 2
		# } else if (input$plot1 == "Trend: outcome") {
			 # 2
		# } else {
			 # 0
		# }
	  
  # })
  
  # })


  #"Overall Activity", "Aortic Valve" = "AorticValveSurgery", "Mitral Valve" = "MitralValveSurgery", "Thoracic Aorta Surgery" = "Surgery_on_thoracic_aorta", "CABG"

  typeList <- list(
  Overall_Activity = "NA",
  AorticValveSurgery = list(
    "unselected" = "unselected", "AVR+No CABG" = "AVR_No_CABG", "AVR+CABG" = "AVR_CABG"),
  MitralValveSurgery = list(
    "unselected" = "unselected", "repair" = "repair", "replacement" = "replacement"),
  Surgery_on_thoracic_aorta = list(
    "unselected" = "unselected", "chronic aneurysm" = "chronic.aneurysm", "acute aortic dissection" = "acute.aortic.dissection"),
  isolatedCABG = list(
    "unselected" = "unselected", "bilatral ITA" = "bilatralITA", "Radial Artery" = "RadialArtery", "Off Pump CABG" = "offpumpCABG")
  )
  
  #update sub type 
  # observe({
    # updateRadioButtons(session, "checkSubType", choices = typeList[[input$checkSgryType]])
  # })

  outcomeList <- list(
  Overall_Activity = "NA",
  death = list(
    "NA" = "NA"),
  SWI = list(
    "NA" = "NA"),
  post_opCVA = list(
    "NA" = "NA"),
  postop_dialysis = list(
    "NA" = "NA")
  )
  
  
  lAuthFlag <- reactiveValues(grpLAuth = TRUE)
			
  # Select all / Unselect all
  observeEvent(input$grpRegion, {
	 #toggle between conditions TRUE and false 
	 lAuthFlag$grpLAuth <- !lAuthFlag$grpLAuth 
  }) 

  output$locAuthCheckbx <- renderUI({div(style = "width: 1px;", checkboxGroupInput(inputId = "check2", label = "Choose", choices = locAuthList, selected = NULL)) 
	  })
	  
  # output$wksCheckbx <- renderUI({div(style = "width: 1px;", checkboxGroupInput(inputId = "checkWeek", label = "Choose", choices = weekList, selected =  globalWk$weeks))
	  # })
	  
  output$prtyCheckbx <- renderUI({div(style = "width: 1px;", checkboxGroupInput(inputId = "checkPrty", label = "Choose", choices = priorityList, selected = priorityList)) 
	  })
  

  observeEvent(input$tabs1, {
   if (isolate(input$tabs1) == "Trend") { 

	hide("MapButtonCases")
	hide("MapMtlyButtonCases")
	show("TrendButtonCases")
	show("ResetPrmtrButton")
	show("processPlotText")
	show("metricsText")
  } else if (isolate(input$tabs1) == "Map: Cases") { 
    show("MapButtonCases")
	hide("MapMtlyButtonCases")
	hide("TrendButtonCases")
	show("ResetPrmtrButton")
	show("processPlotText")
	show("metricsText")
  } else if (isolate(input$tabs1) == "Overview") { 
    hide("TrendButtonCases")
	hide("MapButtonCases")
	hide("MapMtlyButtonCases")
	show("metricsText")
	hide("ResetPrmtrButton")
	hide("processPlotText")
  } else if (isolate(input$tabs1) == "Map: Mortality") { 
	hide("MapButtonCases")
	show("MapMtlyButtonCases")
	hide("TrendButtonCases")
	show("ResetPrmtrButton")
	show("processPlotText")
	show("metricsText")
	
  } else if (isolate(input$tabs1) == "Map: actual") { 
	
  }
  
  })
  
  #Resent national parameters button
  flagResetVar <- reactiveValues(reset_var = FALSE)
   
  #reset national parameters
  observeEvent(input$resetPrmtrButtonId, {

	  #toggle between conditions TRUE and false 
      flagResetVar$reset_var <<- !flagResetVar$reset_var 
    
  })
  
  #Resent national parameters button
  flagResetLocalVar <- reactiveValues(reset_localVar = FALSE)
  #
  #reset local parameters
  observeEvent(input$resetLocalPrmtrId, {

	  #toggle between conditions TRUE and false 
      flagResetLocalVar$reset_localVar <<- !flagResetLocalVar$reset_localVar 
    
  })
  
  
  output$IntroText <- renderUI({

		HTML(paste0("<h3 style=\"color:#A9A9A9;\">","<b>National Coronavirus Global Forecast System </b>","</h3>"
		
		,"<hr>"
		
		," <p>","The forecasting system for data and insights on Covid-19","</p> "
		
		," <h2>","<b>System Summary</b>","</h2> "
		
		,"<br>"
		
		," <p>","National Coronavirus Global Forecast System (NCGFS) will provide the predictions of the number of cases and mortality of covid-19 in the upcoming 5 weeks","</p> "
		
		# ," <p>","Surgery Type: ", input$checkSgryType, "","</p> "
		# ," <p>","Priority Levels Included: ", paste0(input$checkPrty, collapse=", "), "","</p> "
		
		," <p>","The system is an interactive dashboard that allows various parameters to be changed to simulate the effect of policy changes targeting covid-19. For example, users may interactively modify the number of facilities available for accommodation and food, pubs, retail shops, education, transport and storage, art, entertainment and recreational services, within each local authority region. In addition, the user may modify the amount of international migration inflow or internal migration inflow and outflow within UK to simulate policy changes that affect travel. This is because one would expect a correlation between migration and travel both international and domestically in the UK. Furthermore, the user can select the range of weeks to apply these modifications and the prediction will take these changes into account. Following the above-mentioned modifications, the user is then able to simultaneously observe the trend differences between the forecast with these modifications, without these modifications, as well as the historical numbers of actual cases and mortality. ","</p> "
		
		# ,"<br>"
		
		# ," <h4>","Cases by date, by nation","</h4> "
		
		# ," <p>","The UK cardiac surgery activity is summarised by trend across England, Wales and Scotland as well as the overall
		# number of cases (excluding northern Ireland). ", maxCountry, " was the country with the maximum number cases, with the highest figure of ", maxCountryCases, " in ", maxCaseDate, ". ", minCountry, " was the country with the minimum number cases, with the lowest figure of ", minCountryCases, " in ", minCaseDate, ". ","</p> "
		
		,collapse=""))
	})
  
  observe_helpers()
  
  output$sliderSpain <- renderUI({
			
			
			if(flagResetVar$reset_var == TRUE) {
			
				sliderInput(inputId = "TravelSpain", label = "Travel To Spain", min(jointWeekNCases_Mltd_time$TravelSpainEstimate, na.rm = TRUE), max(jointWeekNCases_Mltd_time$TravelSpainEstimate, na.rm = TRUE), value = unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int),]$TravelSpainEstimate), round = 2, animate = TRUE)  %>% helper(type = "inline", title = "Travel To Spain",content = c("This is the estimated number of tourists arriving in Spain from UK in millions. The values are calculated based on number of tourists from various countries arriving in Spain from Jan 2020 to July 2020 and then adjusting by multiplying this number by the proportion of UK tourists in Spain from the preceding year i.e. 2019. ", "The weeks not covered by the data available i.e. August to October are imputed based on the tourism industry recovery model for July opening of borders https://www.adpr.co.uk/blog/covid-19/travel-and-tourism-brands-can-recover-from-coronavirus/ .", "This metric should be used as a guidance for how the situation will vary when international travel is restricted."))
				
			} else {
			
				sliderInput(inputId = "TravelSpain", label = "Travel To Spain", min(jointWeekNCases_Mltd_time$TravelSpainEstimate, na.rm = TRUE), max(jointWeekNCases_Mltd_time$TravelSpainEstimate, na.rm = TRUE), value = unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int),]$TravelSpainEstimate), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Travel To Spain", content = c("This is the estimated number of tourists arriving in Spain from UK in millions. The values are calculated based on number of tourists from various countries arriving in Spain from Jan 2020 to July 2020 and then adjusting by multiplying this number by the proportion of UK tourists in Spain from the preceding year i.e. 2019. ", "The weeks not covered by the data available i.e. August to October are imputed based on the tourism industry recovery model for July opening of borders https://www.adpr.co.uk/blog/covid-19/travel-and-tourism-brands-can-recover-from-coronavirus/ .", "This metric should be used as a guidance for how the situation will vary when international travel is restricted."))
			
			}

  })
  

  
  observeEvent(input$LockDown, { 
  
		if (input$LockDown == 1) {
				
				#this triggers a redraw
				updateSliderInput(session, "TravelSpain", value=0) 
								
				updateSliderInput(session, "openSchool", value=0) 
				
				updateSliderInput(session, "intrntlInflow", value=0)
				
				updateSliderInput(session, "pubSize", value=0)						
				
				updateSliderInput(session, "retailShops", value=0)
				
				updateSliderInput(session, "internalMigNet", value=0)
				
				updateSliderInput(session, "eductnSize", value=0)
				
				updateSliderInput(session, "AccmNFoodSize", value=0)
				
				updateSliderInput(session, "TransprtStrgSize", value=0)
				
				updateSliderInput(session, "artEntertmntRcrtn", value=0)
				
		}
		
  })
  
  
  output$sliderLockDown <- renderUI({
			
			
			if(flagResetVar$reset_var == TRUE) {
			
				sliderInput(inputId = "LockDown", label = "Inverse Lock Down Score", min(jointWeekNCases_Mltd_time$LockdownScore, na.rm = TRUE), max(jointWeekNCases_Mltd_time$LockdownScore, na.rm = TRUE), value = unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int),]$LockdownScore), step = 1, animate = TRUE) %>% helper(type = "inline",
                                         title = "Inverse Lock Down Score",
                                         content = c("During model generation, the LockdownScore index were linked to the main dataset based on the weeks the lock down policies were implemented and the relative effects at each time period. Index definition: 1 = No lock down; 4 = Local lock down; 5 = Local lock down with social distancing; 10 = Full Lock Down.", 
										 "However, as the level of lock down implemented is typically highest when the number of cases or mortality is highest, when varying this parameter during model configuration mode, this metric will have an inverse effect. That is, changing the LockdownScore value to 1 will result in a full lock down, whilst changing to 10 will result in no lock down. 4 will result in Local lock down with social distancing and 5 will represent Local lock down.",
										 "Note, the effects pertain only to the local authorities selected."))
			
			} else {
			
				sliderInput(inputId = "LockDown", label = "Inverse Lock Down Score", min(jointWeekNCases_Mltd_time$LockdownScore, na.rm = TRUE), max(jointWeekNCases_Mltd_time$LockdownScore, na.rm = TRUE), value = unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int),]$LockdownScore), step = 1, animate = TRUE)%>% helper(type = "inline",
                                         title = "Inverse Lock Down Score",
                                         content = c("During model generation, the LockdownScore index were linked to the main dataset based on the weeks the lock down policies were implemented and the relative effects at each time period. Index definition: 1 = No lock down; 4 = Local lock down; 5 = Local lock down with social distancing; 10 = Full Lock Down.", 
										 "However, as the level of lock down implemented is typically highest when the number of cases or mortality is highest, when varying this parameter during model configuration mode, this metric will have an inverse effect. That is, changing the LockdownScore value to 1 will result in a full lock down, whilst changing to 10 will result in no lock down. 4 will result in Local lock down with social distancing and 5 will represent Local lock down.",
										 "Note, the effects pertain only to the local authorities selected."))	
			
			}
  })
  
  output$sliderQrntnRestrctn <- renderUI({
			
			
			if(flagResetVar$reset_var == TRUE) {
			
				sliderInput(inputId = "travelQrntnRestrctn", label = "Travelling Quarantine Restrictions", min(jointWeekNCases_Mltd_time$QuarantineMeasures, na.rm = TRUE), max(jointWeekNCases_Mltd_time$QuarantineMeasures, na.rm = TRUE), value = unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int),]$QuarantineMeasures), step = 0.5, round = 2, animate = TRUE) %>% helper(type = "inline",
                                         title = "Inverse Travelling Quarantine Restrictions",
                                         content = c("During model generation, the QuarantineMeasures index were linked to the main dataset based on the weeks the travel quarantine policies were implemented and the relative effects at each time period. Index definition: 1 = No quarantine; 10 = full quarantine of tourist from all countries; 5 = Removal of 59 countries from quarantine list; 5.5 = Adding Spain back to the quarantine list following removal of 59 countries from the list.", 
										 "As the level of travel quarantine restrictions implemented is dependent on the severity of covid-19 situation in other countries rather than the number of cases and mortality in UK, when varying this parameter during model configuration mode, this metric will have not an inverse effect.",
										 "Note, the effects pertain only to the local authorities selected."))
  
			
			} else {
			
				sliderInput(inputId = "travelQrntnRestrctn", label = "Travelling Quarantine Restrictions", min(jointWeekNCases_Mltd_time$QuarantineMeasures, na.rm = TRUE), max(jointWeekNCases_Mltd_time$QuarantineMeasures, na.rm = TRUE), value = unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int),]$QuarantineMeasures), step = 0.5, round = 2, animate = TRUE) %>% helper(type = "inline",
                                         title = "Travelling Quarantine Restrictions",
                                         content = c("During model generation, the QuarantineMeasures index were linked to the main dataset based on the weeks the travel quarantine policies were implemented and the relative effects at each time period. Index definition: 1 = No quarantine; 10 = full quarantine of tourist from all countries; 5 = Removal of 59 countries from quarantine list; 5.5 = Adding Spain back to the quarantine list following removal of 59 countries from the list.", 
										 "As the level of travel quarantine restrictions implemented is dependent on the severity of covid-19 situation in other countries rather than the number of cases and mortality in UK, when varying this parameter during model configuration mode, this metric will have not an inverse effect.",
										 "Note, the effects pertain only to the local authorities selected."))
  	
			
			}
  })
  
  output$sliderOpenSchool <- renderUI({
			
			
			if(flagResetVar$reset_var == TRUE) {
			
				sliderInput(inputId = "openSchool", label = "Open School", min(jointWeekNCases_Mltd_time$School_Opening, na.rm = TRUE), max(jointWeekNCases_Mltd_time$School_Opening, na.rm = TRUE), value = unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int),]$School_Opening), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Open School",
                                         content = c("During model generation, the SchoolOpening index was linked to the main dataset based on the weeks the school restriction policies were implemented and the relative effects at each time period. Index definition: 3 = No restrictions; 1.5 = School closing but remaining open to some pupils; 3 = Also used to represent school reopening; ", 
										 "Unlike the LockdownScore indices, the SchoolOpening index already takes into account of the factor that the level of school restriction policies implemented is typically highest when the number of cases or mortality is highest. Hence, in this case when varying the Open School parameter during model configuration mode, this metric will not have an inverse effect.",
										 "Note, the effects pertain only to the local authorities selected."))
  
			
			} else {
			
				sliderInput(inputId = "openSchool", label = "Open School", min(jointWeekNCases_Mltd_time$School_Opening, na.rm = TRUE), max(jointWeekNCases_Mltd_time$School_Opening, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int),]$School_Opening)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Open School",
                                         content = c("During model generation, the SchoolOpening index was linked to the main dataset based on the weeks the school restriction policies were implemented and the relative effects at each time period. Index definition: 3 = No restrictions; 1.5 = School closing but remaining open to some pupils; 3 = Also used to represent school reopening; ", 
										 "Unlike the LockdownScore indices, the SchoolOpening index already takes into account of the factor that the level of school restriction policies implemented is typically highest when the number of cases or mortality is highest. Hence, in this case when varying the Open School parameter during model configuration mode, this metric will not have an inverse effect.",
										 "Note, the effects pertain only to the local authorities selected."))
  				
			}
  })
  
  
  #Resent parameters button
  flagUpdtVarByLA <- reactiveValues(updateParamByLA = FALSE)
   
  # Select all / Unselect all
  observeEvent(input$check2, {

	  #toggle between conditions TRUE and false 
      flagUpdtVarByLA$updateParamByLA <<- !flagUpdtVarByLA$updateParamByLA 
    
  })
  
  observeEvent(input$nationalSetting, {
  if(!input$nationalSetting) {
  
  
  output$sliderIntrntlInflow <- renderUI({
			
			
			if((flagResetLocalVar$reset_localVar  == TRUE | flagUpdtVarByLA$updateParamByLA == TRUE) & !input$nationalSetting) {
			
				sliderInput(inputId = "intrntlInflow", label = "International Inflow", min(jointWeekNCases_Mltd_time$`International Migration Inflow`, na.rm = TRUE), max(jointWeekNCases_Mltd_time$`International Migration Inflow`, na.rm = TRUE), value =mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$`International Migration Inflow`)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "International Migration Inflow", content = c("The number of people migrating from other countries to the selected locations from mid-2018 to mid-2019. This is used as an indicator for the amount international travel each location is likely to recieve. Varying this metric can be used to model the effect of restrictions to international travel. ", "This metric is different from the travel to spain metric in that the former takes into account of international travel at the local authority level, whilst the latter metric takes into international travel into account at the national level"))
			
			} else if (input$nationalSetting) {
			
				sliderInput(inputId = "intrntlInflow", label = "International Inflow", min(jointWeekNCases_Mltd_time$`International Migration Inflow`, na.rm = TRUE), max(jointWeekNCases_Mltd_time$`International Migration Inflow`, na.rm = TRUE), value =input$intrntlInflow, round = 2, animate = TRUE) %>% helper(type = "inline", title = "International Migration Inflow", content = c("The number of people migrating from other countries to the selected locations from mid-2018 to mid-2019. This is used as an indicator for the amount international travel each location is likely to recieve. Varying this metric can be used to model the effect of restrictions to international travel. ", "This metric is different from the travel to spain metric in that the former takes into account of international travel at the local authority level, whilst the latter metric takes into international travel into account at the national level"
				 ))
			

			} else {
			
				sliderInput(inputId = "intrntlInflow", label = "International Inflow", min(jointWeekNCases_Mltd_time$`International Migration Inflow`, na.rm = TRUE), max(jointWeekNCases_Mltd_time$`International Migration Inflow`, na.rm = TRUE), value =mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$`International Migration Inflow`)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "International Migration Inflow", content = c("The number of people migrating from other countries to the selected locations from mid-2018 to mid-2019. This is used as an indicator for the amount international travel each location is likely to recieve. Varying this metric can be used to model the effect of restrictions to international travel. ", "This metric is different from the travel to spain metric in that the former takes into account of international travel at the local authority level, whilst the latter metric takes into international travel into account at the national level"))
				
			}
  })
  
  output$sliderPubSize <- renderUI({
			
			
			if((flagResetLocalVar$reset_localVar  == TRUE | flagUpdtVarByLA$updateParamByLA == TRUE) & !input$nationalSetting) {
			
				sliderInput(inputId = "pubSize", label = "Pub size", min(jointWeekNCases_Mltd_time$pub_count_2018, na.rm = TRUE), max(jointWeekNCases_Mltd_time$pub_count_2018, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$pub_count_2018)), round = 5, animate = TRUE) %>% helper(type = "inline", title = "Pub size",
                                         content = c("The number of pubs in the selected local authority."
										 
										 ))
  
			} else if (input$nationalSetting) {
			
				sliderInput(inputId = "pubSize", label = "Pub size", min(jointWeekNCases_Mltd_time$pub_count_2018, na.rm = TRUE), max(jointWeekNCases_Mltd_time$pub_count_2018, na.rm = TRUE), value = input$pubSize, round = 5, animate = TRUE) %>% helper(type = "inline", title = "Pub size",
                                         content = c("The number of pubs in the selected local authority."
										 
										 ))
				
			
			} else {
			
				sliderInput(inputId = "pubSize", label = "Pub size", min(jointWeekNCases_Mltd_time$pub_count_2018, na.rm = TRUE), max(jointWeekNCases_Mltd_time$pub_count_2018, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$pub_count_2018)), round = 5, animate = TRUE)  %>% helper(type = "inline", title = "Pub size",
                                         content = c("The number of pubs in the selected local authority."
										 
										 ))
  				
			}
  })
  
  output$sliderRetailShops <- renderUI({
			
			
			if((flagResetLocalVar$reset_localVar  == TRUE | flagUpdtVarByLA$updateParamByLA == TRUE) & !input$nationalSetting) {
			
				sliderInput(inputId = "retailShops", label = "Retail Shops", min(jointWeekNCases_Mltd_time$Retail, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Retail, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Retail)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Retail Shops",
                                         content = c("The number of available retail businesses in the selected local authority.",
										 "This metric appears to be inversely proportional to the number of cases. Although this appears counterintuitive at first, it is hypothesised that in local authorities with lower number of shops, people are less engaged locally and have a preference for exploring other local authorities, resulting in a greater likelihood of the disease spreading.",
										 "Although this metric does not appear to be readily incrementable by policies, it is expected that increasing people's engagement within their local areas of residence may result in decreased spread of the disease."
										 ))
  
			} else if (input$nationalSetting) {
				
				sliderInput(inputId = "retailShops", label = "Retail Shops", min(jointWeekNCases_Mltd_time$Retail, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Retail, na.rm = TRUE), value = input$retailShops, round = 2, animate = TRUE) %>% helper(type = "inline", title = "Retail Shops",
                                         content = c("The number of available retail businesses in the selected local authority.",
										 "This metric appears to be inversely proportional to the number of cases. Although this appears counterintuitive at first, it is hypothesised that in local authorities with lower number of shops, people are less engaged locally and have a preference for exploring other local authorities, resulting in a greater likelihood of the disease spreading.",
										 "Although this metric does not appear to be readily incrementable by policies, it is expected that increasing people's engagement within their local areas of residence may result in decreased spread of the disease."
										 ))
				
			
			} else {
			
				sliderInput(inputId = "retailShops", label = "Retail Shops", min(jointWeekNCases_Mltd_time$Retail, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Retail, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Retail)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Retail Shops",
                                         content = c("The number of available retail businesses in the selected local authority.",
										 "This metric appears to be inversely proportional to the number of cases. Although this appears counterintuitive at first, it is hypothesised that in local authorities with lower number of shops, people are less engaged locally and have a preference for exploring other local authorities, resulting in a greater likelihood of the disease spreading.",
										 "Although this metric does not appear to be readily incrementable by policies, it is expected that increasing people's engagement within their local areas of residence may result in decreased spread of the disease."
										 ))
  				
			}
  })

   
  output$sliderInternalMigNet <- renderUI({
			
			
			if((flagResetLocalVar$reset_localVar  == TRUE | flagUpdtVarByLA$updateParamByLA == TRUE) & !input$nationalSetting) {
			
				sliderInput(inputId = "internalMigNet", label = "Internal Migration Net", min(jointWeekNCases_Mltd_time$`Internal Migration Net`, na.rm = TRUE), max(jointWeekNCases_Mltd_time$`Internal Migration Net`, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$`Internal Migration Net`)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Internal Migration Net", content = c("The net number of people migrating to or from the selected local authority from mid-2018 to mid-2019. This is used as an indicator for the approximate amount UK domestic travel for each location. Varying this metric can be used to model the effect of movement restrictions within the UK. ",
										 "When internal migration inflow is less than outflow, this value becomes negative, indicating overall outflow and vice versa."
										 ))
  
			} else if (input$nationalSetting) {
  
				sliderInput(inputId = "internalMigNet", label = "Internal Migration Net", min(jointWeekNCases_Mltd_time$`Internal Migration Net`, na.rm = TRUE), max(jointWeekNCases_Mltd_time$`Internal Migration Net`, na.rm = TRUE), value = input$internalMigNet, round = 2, animate = TRUE) %>% helper(type = "inline", title = "Internal Migration Net", content = c("The net number of people migrating to or from the selected local authority from mid-2018 to mid-2019. This is used as an indicator for the approximate amount UK domestic travel for each location. Varying this metric can be used to model the effect of movement restrictions within the UK. ",
										 "When internal migration inflow is less than outflow, this value becomes negative, indicating overall outflow and vice versa."
										 ))
										 
			} else {
			
				sliderInput(inputId = "internalMigNet", label = "Internal Migration Net", min(jointWeekNCases_Mltd_time$`Internal Migration Net`, na.rm = TRUE), max(jointWeekNCases_Mltd_time$`Internal Migration Net`, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$`Internal Migration Net`)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Internal Migration Net", content = c("The net number of people migrating to or from the selected local authority from mid-2018 to mid-2019. This is used as an indicator for the approximate amount UK domestic travel for each location. Varying this metric can be used to model the effect of movement restrictions within the UK. ",
										 "When internal migration inflow is less than outflow, this value becomes negative, indicating overall outflow and vice versa."
										 ))
  				
			}
  })

  output$sliderEductnSize <- renderUI({
			
			
			if((flagResetLocalVar$reset_localVar  == TRUE | flagUpdtVarByLA$updateParamByLA == TRUE) & !input$nationalSetting) {
			
				sliderInput(inputId = "eductnSize", label = "Number of Education Establishments", min(jointWeekNCases_Mltd_time$Education, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Education, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Education)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Number of Education Establishments",
                                         content = c("The number of Education VAT and/or PAYE based local units in the selected local authority."
										 ))
										 
			} else if (input$nationalSetting) {
  
				sliderInput(inputId = "eductnSize", label = "Number of Education Establishments", min(jointWeekNCases_Mltd_time$Education, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Education, na.rm = TRUE), value = input$eductnSize, round = 2, animate = TRUE) %>% helper(type = "inline", title = "Number of Education Establishments",
                                         content = c("The number of Education VAT and/or PAYE based local units in the selected local authority."
										 ))
										 
			} else {
			
				sliderInput(inputId = "eductnSize", label = "Number of Education Establishments", min(jointWeekNCases_Mltd_time$Education, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Education, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Education)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Number of Education Establishments",
                                         content = c("The number of Education (VAT and/or PAYE based) local units in the selected local authority."
										 ))
  				
			}
  })
  
  output$sliderAccmNFoodSize <- renderUI({
			
			
			if((flagResetLocalVar$reset_localVar  == TRUE | flagUpdtVarByLA$updateParamByLA == TRUE) & !input$nationalSetting) {
			
				sliderInput(inputId = "AccmNFoodSize", label = "Number of Accomodation and Food services", min(jointWeekNCases_Mltd_time$Accommodation_food_services, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Accommodation_food_services, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Education)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Number of Accomodation and Food services",
                                         content = c("The number of Accomodation and Food services (VAT and/or PAYE based) local units in the selected local authority.",
										 ""
										 ))
			
			} else if (input$nationalSetting) {
				
				sliderInput(inputId = "AccmNFoodSize", label = "Number of Accomodation and Food services", min(jointWeekNCases_Mltd_time$Accommodation_food_services, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Accommodation_food_services, na.rm = TRUE), value = input$AccmNFoodSize, round = 2, animate = TRUE) %>% helper(type = "inline", title = "Number of Accomodation and Food services",
                                         content = c("The number of Accomodation and Food services (VAT and/or PAYE based) local units in the selected local authority.",
										 ""
										 ))
			
			
			} else {
			
				sliderInput(inputId = "AccmNFoodSize", label = "Number of Accomodation and Food services", min(jointWeekNCases_Mltd_time$Accommodation_food_services, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Accommodation_food_services, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Education)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Number of Accomodation and Food services",
                                         content = c("The number of Accomodation and Food services (VAT and/or PAYE based) local units in the selected local authority."
										 ))
  				
			}
  })
  
  output$sliderTransprtStrgSize <- renderUI({
			
			
			if((flagResetLocalVar$reset_localVar  == TRUE | flagUpdtVarByLA$updateParamByLA == TRUE) & !input$nationalSetting) {
			
				sliderInput(inputId = "TransprtStrgSize", label = "Number of Transport and Storage services", min(jointWeekNCases_Mltd_time$Transport_Storage_inc_postal, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Transport_Storage_inc_postal, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Transport_Storage_inc_postal)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Number of Transport and Storage services", content = c("The number of Transport and Storage services (VAT and/or PAYE based) local units in the selected local authority." ))
  
			} else if (input$nationalSetting) {
				
				sliderInput(inputId = "TransprtStrgSize", label = "Number of Transport and Storage services", min(jointWeekNCases_Mltd_time$Transport_Storage_inc_postal, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Transport_Storage_inc_postal, na.rm = TRUE), value = input$TransprtStrgSize, round = 2, animate = TRUE) %>% helper(type = "inline", title = "Number of Transport and Storage services", content = c("The number of Transport and Storage services (VAT and/or PAYE based) local units in the selected local authority." ))
			
			
			} else {
			
				sliderInput(inputId = "TransprtStrgSize", label = "Number of Transport and Storage services", min(jointWeekNCases_Mltd_time$Transport_Storage_inc_postal, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Transport_Storage_inc_postal, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Transport_Storage_inc_postal)), round = 2, animate = TRUE) %>% helper(type = "inline", title = "Number of Transport and Storage services", content = c("The number of Transport and Storage services (VAT and/or PAYE based) local units in the selected local authority." ))
  				
			}
  })
  
  
  output$sliderArtEntertmntRcrtn <- renderUI({
			
			
			if((flagResetLocalVar$reset_localVar  == TRUE | flagUpdtVarByLA$updateParamByLA == TRUE) & !input$nationalSetting) {
			
				 sliderInput(inputId = "artEntertmntRcrtn", label = "Number of Art, Entertainment & Recreational Services", min(jointWeekNCases_Mltd_time$Arts_entertainment_recreation_other_services, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Arts_entertainment_recreation_other_services, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Arts_entertainment_recreation_other_services)), round = 2, animate = TRUE) %>% helper(type = "inline",  title = "Number of Art, Entertainment & Recreational Services", content = c("The number of Art, Entertainment & Recreational Services (VAT and/or PAYE based) local units in the selected local authority." ))
  
			} else if (input$nationalSetting) {
			
				sliderInput(inputId = "artEntertmntRcrtn", label = "Number of Art, Entertainment & Recreational Services", min(jointWeekNCases_Mltd_time$Arts_entertainment_recreation_other_services, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Arts_entertainment_recreation_other_services, na.rm = TRUE), value = input$artEntertmntRcrtn, round = 2, animate = TRUE) %>% helper(type = "inline",  title = "Number of Art, Entertainment & Recreational Services", content = c("The number of Art, Entertainment & Recreational Services (VAT and/or PAYE based) local units in the selected local authority." ))
			
			
			} else {
			
				sliderInput(inputId = "artEntertmntRcrtn", label = "Number of Art, Entertainment & Recreational Services", min(jointWeekNCases_Mltd_time$Arts_entertainment_recreation_other_services, na.rm = TRUE), max(jointWeekNCases_Mltd_time$Arts_entertainment_recreation_other_services, na.rm = TRUE), value = mean(unique(jointWeekNCases_Mltd_time[jointWeekNCases_Mltd_time$wk_int == max(jointWeekNCases_Mltd_time$wk_int) & jointWeekNCases_Mltd_time$lad19_nm %in% input$check2,]$Arts_entertainment_recreation_other_services)), round = 2, animate = TRUE) %>% helper(type = "inline",  title = "Number of Art, Entertainment & Recreational Services", content = c("The number of Art, Entertainment & Recreational Services (VAT and/or PAYE based) local units in the selected local authority." ))
  				
			}
  })
  
  }  
  
  }) #nationalSetting
  
  
  #process shared model =================================================================================
 
 
  #Load or update model button
  loadModelVar <- reactiveValues(loadMdl = FALSE)
   
  observeEvent(input$loadModelButtonId, {

	  #toggle between conditions TRUE and false 
      loadModelVar$loadMdl <<- !loadModelVar$loadMdl 
    
 
 
				subsetDt <- jointWeekNCases_Mltd_timeSpSortedScaled
									
				
				#filter by local authority lad19_nm 
				subsetDt <- subsetDt[subsetDt$lad19_nm %in% input$check2, ]  #input$check2
						
								
				 # output$res2 <- renderPrint({
							# #paste0(input$growthRate, " ", sample_sim[input$checkWeek,,grwthRIndx])
					  # })
				
				if (!nrow(subsetDt) == 0) {
			
				output$message <- NULL
				
				output$summary <- NULL
					  
				output$summary2 <- NULL
				
						
				
				slctnCmbnd <<- NULL
				
				set.seed(1)

				for (name in unique(subsetDt$lad19_nm))  { 
				
				subsetDt_indiv <- subsetDt[subsetDt$lad19_nm == name, ]
				
				# remove duplicate weeks
				subsetDt_indiv <- subsetDt_indiv[!duplicated(subsetDt_indiv$week),]
				
				nums <- unlist(lapply(subsetDt_indiv, is.numeric))  

								
				data_indiv <- subsetDt_indiv[ , nums]
				
				samplesActual <- array(0, dim = c(length(subsetDt_indiv$week) - 1,
												2,
												dim(data_indiv)[[-1]]))  
				

				for (i in (seq_along(1: (length(subsetDt_indiv$week) - 1))) ) {  # start from wk 5 to 41 (length 37) - 2
					
					#subsetDt <- data[data$lad19_cd == randomLACode ]  #get only data for this Local Authority code 


					rows <- i
					
					indices <- seq(rows, rows + 1, length.out = dim(samplesActual)[[2]]) #get consecutive 2 timestep values
					
					samplesActual[i,,] <- as.matrix(data_indiv[indices,]) 
					

				}
												
				
				if (isolate(!input$simulation)) {
				
				# lookback 4. For training only 
				generator <- function(data, lookback = 3, delay = 1, min_index = 1, max_index = 10,
                      shuffle = FALSE, batch_size = 7, step = 1) {
  
					  function() {  #laInd has to be within inner function for it to increment 
					   
					  dataNum <- data

					  if (is.null(max_index))
						max_index <- nrow(dataNum) - delay - 1
					  #i <- min_index + lookback
					  i <- min_index 
					 
						if (shuffle) {
						  rows <- sample(c((min_index+lookback):max_index), size = batch_size)  #+lookback ensures that index does not go before 1 when we are looking back
						  #This generates a different sample index of batch size each time function is used 
						  #rows <- sample(c((1+3):10), size = 5)
						} else {
						  if (i + batch_size > max_index + 2)
							i <<- min_index + lookback
						  rows <- c(i:min(i+batch_size-1, max_index))
						  i <<- i + length(rows) - 1
						  
						  #rows <- (lookback+1):(nrow(dataNum) - 6)   #exclude last three wks from training 
						  exclParam <- delay
						  rows <- (lookback+1):(nrow(dataNum) - exclParam)   #exclParam = 6 to exclude last three wks from training when lookforward or delay is 3. exclParam = lookforward or delay, when no data is excluded from training 
						}
						
						#samples <- array(0, dim = c(length(rows), 3 / 1, dim(dataNum)[[-1]])) 
						samples <- array(0, dim = c(length(rows),
													2 / step,   #lookback /step
													dim(dataNum)[[-1]]))  
						#targets <- array(0, dim = c(length(rows), 2))   #column 1 for cases; column 2 for mtly
										
						target1 <- array(0, dim = c(length(rows)))  
						target2 <- array(0, dim = c(length(rows)))  	
										
						#get column index of cvdCaseCount				
						cvdCasesColIndex <- grep("cvdCaseCount", colnames(dataNum))
						
						#get column index of cvdMtlyCount				
						cvdMtlyColIndex <- grep("cvdMtlyCount", colnames(dataNum))
						
						for (j in 1:length(rows)) {
						  indices <- seq(rows[[j]] - lookback, rows[[j]],
										 length.out = dim(samples)[[2]])  # obtains indices from a random value in between lookback and max_index take away lookback in steps up to that value #changed from dim(samples)[[2]] to 3
						  samples[j,,] <- as.matrix(dataNum[indices,])   #change dataframe to matrix i.e. dataNum[indices,] to as.matrix(dataNum[indices,])
						  # targets[j] <- dataNum[rows[[j]] + delay, cvdCasesColIndex]  # indice of random value plus look forward (delay e.g. 24hrs) obtains column value for look forward dataNum # cvdCases is column 128
						  
						  target1[j] <- dataNum[rows[[j]] + delay, cvdCasesColIndex]  # indice of random value plus look forward (delay e.g. 24hrs) obtains column value for look forward dataNum # cvdCases is column 128
						  
						  target2[j] <- dataNum[rows[[j]] + delay, cvdMtlyColIndex]
						} 
						targets <- list(target1, target2)
						list(samples, targets)
					  }
					}
				
				#training generator looks at the first half of the timesteps i.e. 37 / 2 = 18
				train_gen <- generator(
				  data_indiv,
				  lookback = lookback,
				  delay = delay,
				  min_index = 1,
				  max_index = 36,		#increase from 10 to 18 # changed from 26 to 36 for implementation rather than evaluation
				  shuffle = FALSE,
				  step = step, 
				  batch_size = batch_size	#increase from 5 to 12 to increase sample size 
				)

				#validation generator looks at the following 1 / 4 of timesteps i.e. 37 / 4 = 9.25 i.e 19 to 28 # changed to from 27 to 37 as test is not needed 
				val_gen = generator(
				  data_indiv,
				  lookback = lookback,
				  delay = delay,
				  min_index = 1,
				  max_index = 36,
				  step = step,
				  batch_size = batch_size
				)
				
				num_samples <- 37
				stepsPerEpoch <- ceiling(num_samples / batch_size)

				new_gen <- keras:::as_generator.function(train_gen)
				new_genVal <- keras:::as_generator.function(val_gen)
				
			
				
				#temp: comment out during deploy to io cloud 
				history <- isolate(model) %>% fit_generator(
				  new_gen,
				  steps_per_epoch = stepsPerEpoch,
				  callbacks = list(
				  callback_early_stopping(monitor = "val_loss", mode = "min", patience=10, restore_best_weights = TRUE)),
				  # callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, cooldown= 1, patience = 10, mode = "min")),
				  epochs = 1,
				  validation_data = new_genVal,
				  validation_steps = val_steps
				)
				
				}
				
				
				
				#simulation mode uses no training and generalisation can become worse than master model when model is trained in non simulation mode for specific local authorities, even though accuracy becomes better for individual local authorities 
				if (isolate(input$simulation) & length(input$check2) > 1) {
				
					prdctOutpt <- predict(modelMaster, samplesActual, steps=NULL)
				
				} else {
				
					prdctOutpt <- predict(model, samplesActual, steps=NULL)
					
				}
				
				# output$res2 <- renderPrint({
							# paste0(samplesActual, " ", input$checkWeek, " ", "")
					  # })
				
				
				#Cases section ==============================================================================
				
				outputCasesUnscld <- prdctOutpt[[1]][,,1][,2] * stdTrain[["daily"]] + meanTrain[["daily"]]	
				outputCasesUnscld <- as.data.frame(outputCasesUnscld, drop=FALSE)

				#Cases section ==============================================================================
				
				
				#Mtly section ==============================================================================
				
				outputMtlyUnscld <- prdctOutpt[[2]][,,1][,1] * stdTrain[["dailyMtly"]] + meanTrain[["dailyMtly"]]
				outputMtlyUnscld <- as.data.frame(outputMtlyUnscld, drop=FALSE)
				
				#Mtly section ==============================================================================
				
				
				#Cases Simulation section ==============================================================================
				
				if (isolate(input$simulation)) {
				
				sample_sim <- duplicate(samplesActual, shallow = FALSE)									

				wkColNames <- subsetDt_indiv$week[1:(length(subsetDt_indiv$week) - lookback)]  #lookback currently 1
				
				dimnames(sample_sim) <- list(wkColNames)
				
					
				wkRange <- sprintf("wk_%02d", min(input$checkWeek):max(input$checkWeek))
				
				
				#modify spain travel 
				travelSpainIndx <- grep("TravelSpainEstimate", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,travelSpainIndx] <- scale(input$TravelSpain, center = meanTrain[["TravelSpainEstimate"]], scale = stdTrain[["TravelSpainEstimate"]])
				
				#lockdown
				lockDownIndx <- grep("LockdownScore", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,lockDownIndx] <- scale(input$LockDown, center = meanTrain[["LockdownScore"]], scale = stdTrain[["LockdownScore"]])
				
				#travelQrntnRestrctn
				travelQuarantnRstrctnIndx <- grep("QuarantineMeasures", colnames(data_indiv)) 
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,travelQuarantnRstrctnIndx] <- scale(input$travelQrntnRestrctn, center = meanTrain[["QuarantineMeasures"]], scale = stdTrain[["QuarantineMeasures"]])
				
				#internation inflow
				internatnlInflowIndx <- grep("International Migration Inflow", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,internatnlInflowIndx] <- scale(input$intrntlInflow, center = meanTrain[["International Migration Inflow"]], scale = stdTrain[["International Migration Inflow"]])
				
				#Pub size 
				pubSizeIndx <- grep("pub_count_2018", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,pubSizeIndx] <- scale(input$pubSize, center = meanTrain[["pub_count_2018"]], scale = stdTrain[["pub_count_2018"]])
				
				#retailShops
				pubSizeIndx <- grep("Retail", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,pubSizeIndx] <- scale(input$retailShops, center = meanTrain[["Retail"]], scale = stdTrain[["Retail"]])
				
				#open school
				schoolOpenIndx <- grep("School_Opening", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,schoolOpenIndx] <- scale(input$openSchool, center = meanTrain[["School_Opening"]], scale = stdTrain[["School_Opening"]])
				
				#Internal Migration Net flow
				InternalMigrationNetIndx <- grep("Internal Migration Net", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,InternalMigrationNetIndx] <- scale(input$internalMigNet, center = meanTrain[["Internal Migration Net"]], scale = stdTrain[["Internal Migration Net"]])
				
				#Number of Education establishment in 2020
				educatnEstblshmIndx <- grep("Education", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,educatnEstblshmIndx] <- scale(input$eductnSize, center = meanTrain[["Education"]], scale = stdTrain[["Education"]])
				
				#Number of Accommodation_food_services establishment in 2020
				accmdNFoodSrvcIndx <- grep("Accommodation_food_services", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,accmdNFoodSrvcIndx] <- scale(input$AccmNFoodSize, center = meanTrain[["Accommodation_food_services"]], scale = stdTrain[["Accommodation_food_services"]])
				
				#Number of Transport_Storage_inc_postal establishment in 2020
				accmdNFoodSrvcIndx <- grep("Transport_Storage_inc_postal", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,accmdNFoodSrvcIndx] <- scale(input$TransprtStrgSize, center = meanTrain[["Transport_Storage_inc_postal"]], scale = stdTrain[["Transport_Storage_inc_postal"]])
				
				
				#Number of Arts_entertainment_recreation_other_services 
				artEntrnmntIndx <- grep("Arts_entertainment_recreation_other_services", colnames(data_indiv))
				
				sample_sim[dimnames(sample_sim)[[1]] %in% wkRange,,artEntrnmntIndx] <- scale(input$artEntertmntRcrtn, center = meanTrain[["Arts_entertainment_recreation_other_services"]], scale = stdTrain[["Arts_entertainment_recreation_other_services"]])
				
				
				#do prediction on simulation data sample_sim				
				if (length(input$check2) > 1) { #modelMaster has the best generalisation capability when reinforcement is switched off for simulation mode 
					
					prdctOutpt_sim <- predict(modelMaster, sample_sim, steps=NULL)
				
				} else {
				
					prdctOutpt_sim <- predict(model, sample_sim, steps=NULL)
					
				} 
				
				#Cases section ==============================================================================
				
				outputCasesUnscld_Sim <- prdctOutpt_sim[[1]][,,1][,2] * stdTrain[["daily"]] + meanTrain[["daily"]]
				outputCasesUnscld_Sim <- as.data.frame(outputCasesUnscld_Sim, drop=FALSE)
					
				#Cases section ==============================================================================
				
				#Mtly section ==============================================================================	
				
				outputMtlyUnscld_Sim <- prdctOutpt_sim[[2]][,,1][,1] * stdTrain[["dailyMtly"]] + meanTrain[["dailyMtly"]]
				outputMtlyUnscld_Sim <- as.data.frame(outputMtlyUnscld_Sim, drop=FALSE)
				
				#Mtly section ==============================================================================
				
				}
				
				#Cases Simulation section ==============================================================================
				
	
				#prdctdWks <- c("wk_09", sprintf("wk_%s",10:43))  # up to here 
				library(readr)
				
				#predicted weeks are four wks ahead of sample week in first column i.e. also of original data 
				#predicted weeks = delay or look ahead + 1 ahead of sample week in first column i.e. also of original data 
				minPrdctdWk <- parse_number(min(subsetDt_indiv$week)) + (delay + 1)
				maxPrdctdWk <- parse_number(max(subsetDt_indiv$week)) -1 + (delay + 1)  # -1 because last week is part of preceding index row in samples 
				
				#add wk and 0s for single digits
				prdctdWks <- c(sprintf("wk_%02d", minPrdctdWk: maxPrdctdWk))
				# prdctdWks <- as.data.frame(prdctdWks)
				
				if (isolate(input$simulation)){
				
					outputUnscldWk <- cbind(prdctdWks, outputCasesUnscld, outputCasesUnscld_Sim, outputMtlyUnscld, outputMtlyUnscld_Sim)

					colnames(outputUnscldWk)[which(names(outputUnscldWk) == "outputCasesUnscld_Sim")]  <- c("Simulated Predicted Cases")

					colnames(outputUnscldWk)[which(names(outputUnscldWk) == "outputMtlyUnscld_Sim")]  <- c("Simulated Predicted Mortality")	

				} else {
				
					outputUnscldWk <- cbind(prdctdWks, outputCasesUnscld, outputMtlyUnscld)
				
				}
				
				colnames(outputUnscldWk)[which(names(outputUnscldWk) == "prdctdWks")]  <- c("week")
									
				colnames(outputUnscldWk)[which(names(outputUnscldWk) == "outputCasesUnscld")]  <- c("Predicted Cases")

				colnames(outputUnscldWk)[which(names(outputUnscldWk) == "outputMtlyUnscld")]  <- c("Predicted Mortality")				
				
				colnames(subsetDt_indiv)[which(names(subsetDt_indiv) == "lad19_nm")]  <- c("lad20_nm")
				
				#outputUnscldWk$lad19_nm <- unique(subsetDt_indiv$lad19_nm)

				outputUnscldWk$lad20_nm <- unique(subsetDt_indiv$lad20_nm)
				outputUnscldWk$lad20_cd <- unique(subsetDt_indiv$lad20_cd)
				
				outputUnscldWk$PHEC19CD <- unique(subsetDt_indiv$PHEC19CD)
				outputUnscldWk$PHEC19NM <- unique(subsetDt_indiv$PHEC19NM)
								
				
				data_preJntPrdct <- merge(x = subsetDt_indiv, y = outputUnscldWk, by = c("week", "lad20_nm", "PHEC19CD", "lad20_cd", "PHEC19NM"), all = TRUE)  #using outer join 
				
				#Cases section ==============================================================================
				
				data_preJntPrdct$cvdCaseCountUnscld <- data_preJntPrdct$cvdCaseCount * stdTrain[["daily"]] + meanTrain[["daily"]] 
				
				#Cases section ==============================================================================
				
				#Mtly section ==============================================================================
				
				data_preJntPrdct$cvdMtlyCountUnscld <- data_preJntPrdct$cvdMtlyCount * stdTrain[["dailyMtly"]] + meanTrain[["dailyMtly"]] 
				
				#Mtly section ==============================================================================
				
				
				#hide prediction values that are covered by weeks containing actual values 
				if (input$policyScientist) {
						
						#cases 
						wkActualMax <- max(data_preJntPrdct[!is.na(data_preJntPrdct$cvdCaseCountUnscld), ]$week)
					
						maxActualWkValue <- data_preJntPrdct[data_preJntPrdct$week == wkActualMax, "cvdCaseCountUnscld"]
						
						data_preJntPrdct[data_preJntPrdct$week <= wkActualMax, ]$`Predicted Cases` <- NA
						
						data_preJntPrdct[data_preJntPrdct$week == wkActualMax, ]$`Predicted Cases` <- maxActualWkValue
						
						#mtly
						wkActualMtlyMax <- max(data_preJntPrdct[!is.na(data_preJntPrdct$cvdMtlyCountUnscld), ]$week)
						
						maxActualWkMtlyValue <- data_preJntPrdct[data_preJntPrdct$week == wkActualMtlyMax, "cvdMtlyCountUnscld"]
						
						data_preJntPrdct[data_preJntPrdct$week <= wkActualMtlyMax, ]$`Predicted Mortality` <- NA
						
						data_preJntPrdct[data_preJntPrdct$week == wkActualMtlyMax, ]$`Predicted Mortality` <- maxActualWkMtlyValue
						
						
						#cases simulated 						
						data_preJntPrdct[data_preJntPrdct$week <= wkActualMax, ]$`Simulated Predicted Cases` <- NA
						
						data_preJntPrdct[data_preJntPrdct$week == wkActualMax, ]$`Simulated Predicted Cases` <- maxActualWkValue
						
						
						#mtly simulated 						
						data_preJntPrdct[data_preJntPrdct$week <= wkActualMtlyMax, ]$`Simulated Predicted Mortality` <- NA
						
						data_preJntPrdct[data_preJntPrdct$week == wkActualMtlyMax, ]$`Simulated Predicted Mortality` <- maxActualWkMtlyValue
						
				}
						
				
				slctnCmbnd <<- rbind(data_preJntPrdct,slctnCmbnd)

				
				}  
				
				} else {
					#no data 
					output$message <- renderText({
							"No results to display for selection"
					})
					
					output$summary <- NULL
						  
					output$summary2 <- NULL
					
				} 
	
		#reset button to FALSE after processing click is complete 
		loadModelVar <<- reactiveValues(loadMdl = FALSE)
	
				rm(data_preJntPrdct)	
				rm(outputUnscldWk)	
				#rm(nhsLclOffcBndryLn)		
				rm(minPrdctdWk)	
				rm(maxPrdctdWk)	
				rm(prdctdWks)
				rm(outputCasesUnscld)
				rm(prdctOutpt)
				rm(samplesActual)
				rm(subsetDt)
				rm(subsetDt_indiv)
	
				gc() 
	 })
	
	#process shared model =================================================================================
  
  
  
  observeEvent(input$trendProcessId, {

  if (isolate(input$tabs1) == "Trend") { 
 
   
	plotData <- eventReactive(input$trendProcessId, {
				
		par(mar=c(10,4.5,7,7))
			

			if (isolate(!lAuthFlag$grpLAuth)) {  #Default to use PHE centres 

								
			    # add PHE centre --------------------------------------------------------------
							
				if (input$simulation) {
			
					slctnCmbnd_Slctn <- slctnCmbnd[,c("Predicted Cases", "cvdCaseCountUnscld", "PHEC19CD", "PHEC19NM", "week", "Simulated Predicted Cases")]
					
					#slctnCmbndGrp <- aggregate(`Predicted Cases`~PHEC19CD+PHEC19NM+week, slctnCmbnd, sum, na.rm = FALSE)
					slctnCmbnd <- aggregate(.~PHEC19CD+PHEC19NM+week, slctnCmbnd_Slctn, sum, na.action = na.pass)
				
				} else {
					
					slctnCmbnd_simSlctn <- slctnCmbnd[,c("Predicted Cases", "cvdCaseCountUnscld", "PHEC19CD", "PHEC19NM", "week")]
					
					#slctnCmbndGrp <- aggregate(`Predicted Cases`~PHEC19CD+PHEC19NM+week, slctnCmbnd, sum, na.rm = FALSE)
					slctnCmbnd <- aggregate(.~PHEC19CD+PHEC19NM+week, slctnCmbnd_simSlctn, sum, na.action = na.pass)
				
				}
				
			} else {
			
				#no action 
				
			}				
				
								
				if (flag$grpLocation) {
				
					
				if (!input$simulation) {
				
				#filter by weeks  
				#slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% input$checkWeek, ]  #input$checkWeek
				
				} else {
				#no actions
				
		
				}
				
				
				if (isolate(!lAuthFlag$grpLAuth)) {  #Default to use PHE centres 
				
				
					if (length(unique(slctnCmbnd$weekNumber)) != 1 ) {
					
						g <- ggplot(slctnCmbnd, aes(week, group = as.factor(PHEC19NM))) + theme_bw() + geom_line(size = 1.5, na.rm=TRUE, aes(y = `Predicted Cases`, linetype = "Predicted Case Count", colour = as.factor(PHEC19NM))) + geom_line(na.rm=TRUE, aes(y = cvdCaseCountUnscld, linetype = "Actual Case Count", colour = as.factor(PHEC19NM))) + scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'solid')) + labs(color='Location Id') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Week") + ylab("Count") + ggtitle("Trend: number of actual and predicted cases from wk 5 to 43") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5)))
							
						if (input$simulation) {
						g <- g + geom_line(size = 1, na.rm=TRUE, aes(y = `Simulated Predicted Cases`, linetype = "Simulated Prediction Case Count", colour = as.factor(PHEC19NM)))
						
						g					
							
						} else {
						
						g 
						
						
						}
						
					} 
				
				} else {
				
					if (length(unique(slctnCmbnd$weekNumber)) != 1 ) {
					
						g <- ggplot(slctnCmbnd, aes(week, group = as.factor(lad20_nm))) + theme_bw() + geom_line(size = 1.5, na.rm=TRUE, aes(y = `Predicted Cases`, linetype = "Predicted Case Count", colour = as.factor(lad20_nm))) + geom_line(na.rm=TRUE, aes(y = cvdCaseCountUnscld, linetype = "Actual Case Count", colour = as.factor(lad20_nm))) + scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'solid')) + labs(color='Location Id') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Week") + ylab("Count") + ggtitle("Trend: number of actual and predicted cases from wk 5 to 43") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5)))
							
						if (input$simulation) {
						g <- g + geom_line(size = 1, na.rm=TRUE, aes(y = `Simulated Predicted Cases`, linetype = "Simulated Prediction Case Count", colour = as.factor(lad20_nm)))
						
						g					
							
						} else {
						
						g 
						
						
						}
						
					}
				
				
				}
					
						
				} else {  #group count for selected regions 
				
				
				#slctnCmbnd$one <- 1
				
				if (!input$simulation) {
				
				#filter by weeks  
				#slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% input$checkWeek, ]  #input$checkWeek
				
				
				} else {
				
				#no actions
				slctnCmbnd_simSlctn <- slctnCmbnd[,c("cvdCaseCountUnscld", "week", "Simulated Predicted Cases")]
				
				casesByLauthDstrctWk_Sim <- aggregate(.~week, slctnCmbnd_simSlctn, sum, na.action = na.pass)						
				
				casesByLauthDstrctWk_Sim$one <- 1
				
				}
				
				slctnCmbndSlctn <- slctnCmbnd[,c("cvdCaseCountUnscld", "week", "Predicted Cases")]
					
			    
				casesByLauthDstrctWk <- aggregate(.~week, slctnCmbndSlctn, sum, na.action = na.pass)				
				
				casesByLauthDstrctWk$one <- 1
				
			
				
				if (length(unique(casesByLauthDstrctWk$weekNumber)) != 1 ) {
																		
					g <- ggplot(casesByLauthDstrctWk, aes(week)) + theme_bw() + geom_line(na.rm=TRUE, aes(y = `Predicted Cases`,  linetype = "Predicted Case Count", group = 1, colour = as.factor(one)), size = 1.5) + geom_line(na.rm=TRUE, aes(y = cvdCaseCountUnscld, linetype = "Actual Case Count", group = 1, colour = as.factor(one))) + scale_linetype_manual(values = c('solid', 'dotted', 'dashed')) + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Wk (week number)") + ylab("Count") + ggtitle("Trend: number of actual and predicted cases from wk 5 to 43") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(labels = c("All Selected Regions"), values = "darkorange") + guides(color = guide_legend(override.aes = list(size = 1.5)), linetype = guide_legend(override.aes = list(size = 0.5)) ) 
					
									
					if (input$simulation) {
					g <- g + geom_line(size = 1, aes(y = casesByLauthDstrctWk_Sim$`Simulated Predicted Cases`, group = 1, linetype = "Simulated Prediction Case Count", colour = as.factor(casesByLauthDstrctWk_Sim$one)), na.rm=TRUE)
					
					g 
													
					
					} else {
	
					
					g 
					
					}
					
				
				} 
				
								
				}
						

	} )
	
	
	if (isolate(input$tabs1) == "Trend") {
		plotDataImp <<- isolate(plotData)
	
	
		output$plot_access_web <<- renderPlot({
			plotDataImp()
		})
		
		outputOptions(output, "plot_access_web", suspendWhenHidden=FALSE)
	
	}
    
  }

  }) #observe input tabs1 
  
  observeEvent(input$trendProcessMtlyId, {

  if (isolate(input$tabs1) == "Trend") { 
   
	plotDataMtly <- eventReactive(input$trendProcessMtlyId, {
				
		par(mar=c(10,4.5,7,7))
			
			
			if (isolate(!lAuthFlag$grpLAuth)) {  #Default to use PHE centres 

								
			    # add PHE centre --------------------------------------------------------------
							
				if (input$simulation) {
			
					slctnCmbnd_Slctn <- slctnCmbnd[,c("Predicted Mortality", "cvdMtlyCountUnscld", "PHEC19CD", "PHEC19NM", "week", "Simulated Predicted Mortality")]
					
					#slctnCmbndGrp <- aggregate(`Predicted Cases`~PHEC19CD+PHEC19NM+week, slctnCmbnd, sum, na.rm = FALSE)
					slctnCmbnd <- aggregate(.~PHEC19CD+PHEC19NM+week, slctnCmbnd_Slctn, sum, na.action = na.pass)
				
				} else {
					
					slctnCmbnd_simSlctn <- slctnCmbnd[,c("Predicted Mortality", "cvdMtlyCountUnscld", "PHEC19CD", "PHEC19NM", "week")]
					
					#slctnCmbndGrp <- aggregate(`Predicted Cases`~PHEC19CD+PHEC19NM+week, slctnCmbnd, sum, na.rm = FALSE)
					slctnCmbnd <- aggregate(.~PHEC19CD+PHEC19NM+week, slctnCmbnd_simSlctn, sum, na.action = na.pass)
				
				}
				
			} else {
			
				#no action 
				
			}					
								
								
				if (flag$grpLocation) {
				
					
					if (!input$simulation) {
					
					#filter by weeks  
					#slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% input$checkWeek, ]  #input$checkWeek
					
					} else {
					#no actions
					
			
					}
					
					if (isolate(!lAuthFlag$grpLAuth)) {  #Default to use PHE centres 
					
						if (length(unique(slctnCmbnd$weekNumber)) != 1 ) {
						
							g <- ggplot(slctnCmbnd, aes(week, group = as.factor(PHEC19NM))) + theme_bw() + geom_line(size = 1.5, na.rm=TRUE, aes(y = `Predicted Mortality`, linetype = "Predicted Mortality Count", colour = as.factor(PHEC19NM))) + geom_line(na.rm=TRUE, aes(y = cvdMtlyCountUnscld, linetype = "Actual Mortality Count", colour = as.factor(PHEC19NM))) + scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'solid')) + labs(color='Location Id') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Week") + ylab("Count") + ggtitle("Trend: number of actual and predicted Mortality from wk 5 to 49") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5)))
										
							
							if (input$simulation) {
							g <- g + geom_line(size = 1, na.rm=TRUE, aes(y = `Simulated Predicted Mortality`, linetype = "Simulated Prediction Mortality Count", colour = as.factor(PHEC19NM)))
							
							g
											
							
							} else {
							
							g 
												
							
							}
							
						} 
					
					} else {
					
						
						if (length(unique(slctnCmbnd$weekNumber)) != 1 ) {
						
							g <- ggplot(slctnCmbnd, aes(week, group = as.factor(lad20_nm))) + theme_bw() + geom_line(size = 1.5, na.rm=TRUE, aes(y = `Predicted Mortality`, linetype = "Predicted Mortality Count", colour = as.factor(lad20_nm))) + geom_line(na.rm=TRUE, aes(y = cvdMtlyCountUnscld, linetype = "Actual Mortality Count", colour = as.factor(lad20_nm))) + scale_linetype_manual(values = c('solid', 'dotted', 'dashed', 'solid')) + labs(color='Location Id') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Week") + ylab("Count") + ggtitle("Trend: number of actual and predicted Mortality from wk 5 to 49") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5)))
										
							
							if (input$simulation) {
							g <- g + geom_line(size = 1, na.rm=TRUE, aes(y = `Simulated Predicted Mortality`, linetype = "Simulated Prediction Mortality Count", colour = as.factor(lad20_nm)))
							
							g
											
							
							} else {
							
							g 
												
							
							}
							
						}
						
					
					}
					
						
				} else {  #group count for selected regions 
				
				
				#slctnCmbnd$one <- 1
				
					if (!input$simulation) {
					
					#filter by weeks  
					#slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% input$checkWeek, ]  #input$checkWeek
					
					
					} else {
					
						#no actions
						slctnCmbnd_simSlctn <- slctnCmbnd[,c("cvdMtlyCountUnscld", "week", "Simulated Predicted Mortality")]
						
						casesByLauthDstrctWk_Sim <- aggregate(.~week, slctnCmbnd_simSlctn, sum, na.action = na.pass)						
						
						casesByLauthDstrctWk_Sim$one <- 1
						
					}
					
					slctnCmbndSlctn <- slctnCmbnd[,c("cvdMtlyCountUnscld", "week", "Predicted Mortality")]				
				   
					
					casesByLauthDstrctWk <- aggregate(.~week, slctnCmbndSlctn, sum, na.action = na.pass)				
					
					casesByLauthDstrctWk$one <- 1
									
					
					if (length(unique(casesByLauthDstrctWk$weekNumber)) != 1 ) {
																			
						g <- ggplot(casesByLauthDstrctWk, aes(week)) + theme_bw() + geom_line(na.rm=TRUE, aes(y = `Predicted Mortality`,  linetype = "Predicted Mortality Count", group = 1, colour = as.factor(one)), size = 1.5) + geom_line(na.rm=TRUE, aes(y = cvdMtlyCountUnscld, linetype = "Actual Mortality Count", group = 1, colour = as.factor(one))) + scale_linetype_manual(values = c('solid', 'dotted', 'dashed')) + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Wk (week number)") + ylab("Count") + ggtitle("Trend: number of actual and predicted Mortality from wk 5 to 43") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(labels = c("All Selected Regions"), values = "darkorange") + guides(color = guide_legend(override.aes = list(size = 1.5)), linetype = guide_legend(override.aes = list(size = 0.5)) ) 
						
											
						if (input$simulation) {
						g <- g + geom_line(size = 1, aes(y = casesByLauthDstrctWk_Sim$`Simulated Predicted Mortality`, group = 1, linetype = "Simulated Prediction Mortality Count", colour = as.factor(casesByLauthDstrctWk_Sim$one)), na.rm=TRUE)
						
						g 
						
											
						} else {
							
						g 
						
						}
						
					
					} 
				
				#}
								
				}
						

					

	} )
	
	
		if (isolate(input$tabs1) == "Trend") {
			plotDataMtlyImp <<- isolate(plotDataMtly)
		
		
			output$plot_mtly <<- renderPlot({
				plotDataMtlyImp()
			})
			
			outputOptions(output, "plot_mtly", suspendWhenHidden=FALSE)
		
		}
    
	}

  })
 
  
  
 observeEvent(input$mapProcessId, { 
  

  if (isolate(input$tabs1) == "Map: Cases") {
	
    
		if (!input$simulation) {
		    
			plotDataLeaflet <- eventReactive(input$mapProcessId, {
			
												
				library(leaflet)
				
				
				if (isolate(!lAuthFlag$grpLAuth)) {  #Default to use PHE centres 

				
				
			
				rm(data_preJntPrdct)	
				rm(outputUnscldWk)	
				#rm(nhsLclOffcBndryLn)		
				rm(minPrdctdWk)	
				rm(maxPrdctdWk)	
				rm(prdctdWks)
				rm(outputCasesUnscld)
				rm(prdctOutpt)
				rm(samplesActual)
				rm(subsetDt)
				rm(subsetDt_indiv)
				
				gc() 
				
			    # add PHE centre spatial data --------------------------------------------------------------
				
				#not be used for model generation as Deprivation indices e.g. rank should not be summed. Hence, only counts are applicable e.g. age count. Only used for prediction purposes 
				
				#PHE_centreGrp <- aggregate(Predicted Cases~PHEC19CD+PHEC19NM+weekNumber, slctnCmbnd, sum, na.rm = FALSE)
				
				
				#filter by weeks  
				#slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% input$checkWeek, ]  #input$checkWeek
				slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% max(slctnCmbnd$week), ] 
				
				slctnCmbndGrp <- aggregate(`Predicted Cases`~PHEC19CD+PHEC19NM+week, slctnCmbnd, sum, na.rm = FALSE)
				
				
				#slctnCmbndGrp <- aggregate(`Predicted Cases`~PHEC19CD+PHEC19NM, slctnCmbnd, sum, na.rm = FALSE)

				#do this latter on within plot 
				phe_CentresBndryLnSF <- sf::st_as_sf(phe_CentresBndryLn)
								
				phe_CentresBndryLnSF_poly <- st_cast(phe_CentresBndryLnSF,"POLYGON")

				# remove PHE centre codes not in slctnCmbndGrp centre codes 
				phe_CentresBndryLnSF_poly <- phe_CentresBndryLnSF_poly[phe_CentresBndryLnSF_poly$phec16cd %in% as.character(slctnCmbndGrp$PHEC19CD),]

				# remove duplicate PHE centre codes 
				phe_CentresBndryLnSF_polyUniq <- phe_CentresBndryLnSF_poly[!duplicated(phe_CentresBndryLnSF_poly$phec16cd),]

				PHE_centreGrpSp <- merge(slctnCmbndGrp, phe_CentresBndryLnSF_polyUniq, by.x="PHEC19CD",by.y="phec16cd")

				PHE_centreGrpSpSF <- sf::st_as_sf(PHE_centreGrpSp)
				
				
				
				PHE_centreGrpSpSF_longLatCase <- st_transform(PHE_centreGrpSpSF, "+init=epsg:4326")				
						
				
				#colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
				
				IntvlBins <-c(0, 250, 500, 750, 1000, 1250, 1500)
				
				mypal <- colorBin(palette = colorSet2, domain = PHE_centreGrpSpSF_longLatCase$`Predicted Cases`, bins = IntvlBins, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = FALSE, right = FALSE)

				
				#Number of cases 
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936,, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = PHE_centreGrpSpSF_longLatCase,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(PHE_centreGrpSpSF_longLatCase$`Predicted Cases`),
									popup = paste("Region: ", PHE_centreGrpSpSF_longLatCase$PHEC19NM, "<br>",
												  "Value: ", PHE_centreGrpSpSF_longLatCase$`Predicted Cases`, "<br>",
												  "Week: ", PHE_centreGrpSpSF_longLatCase$week, "<br>"),
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"), pal = mypal, values = PHE_centreGrpSpSF_longLatCase$`Predicted Cases`,
								  title = "Number of cases",
								  opacity = 1)
				
								

				} else {	  
				
				# -----------------  only for local authority but not for PHE centres -----------------------------------
			
								
				#filter by weeks  
				#slctnCmbnd <- slctnCmbnd[slctnCmbnd$week%in% input$checkWeek, ]  #input$checkWeek
				slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% max(slctnCmbnd$week), ] 
				
				if (length(unique(slctnCmbnd$week)) > 1) {
					slctnCmbndGrp <- aggregate(`Predicted Cases`~lad20_nm+lad20_cd+week, slctnCmbnd, sum, na.rm = FALSE)
				} else {
					slctnCmbndGrp <- slctnCmbnd[, c("lad20_nm", "lad20_cd", "week", "Predicted Cases")]
				}
				
				# combine with local authority spatial data ----------------------------------------------
				
				# remove LA codes not in jointWeekNCases_Mltd_time 
				engWLAuthBndryLnSF_poly <- engWLAuthBndryLnSF_poly[engWLAuthBndryLnSF_poly$lad19cd %in% as.character(slctnCmbndGrp$lad20_cd),]

				# remove duplicate LA codes 
				engWLAuthBndryLnSF_polyUniq <- engWLAuthBndryLnSF_poly[!duplicated(engWLAuthBndryLnSF_poly$lad19cd),]

				# merge slctnCmbndGrp LA code with engWLAuthBndryLnSF_polyUniq LA code 
				slctnCmbndGrp <- merge(slctnCmbndGrp, engWLAuthBndryLnSF_polyUniq, by.x="lad20_cd",by.y="lad19cd")

				# combine with local authority spatial data ----------------------------------------------


				#slctnCmbnd <- slctnCmbnd[, c("lad19_nm", "lad19_cd", "Predicted Cases", "long", "lat", "geometry")]
				
				slctnCmbndSpSF <- sf::st_as_sf(slctnCmbndGrp)
				
				
				
				rm(slctnCmbndGrp)	
				#rm(engCCGBndryLn)
				#rm(subsetDt)
				#rm(slctnCmbnd)
				rm(engWLAuthBndryLnSF)
				rm(engWLAuthBndryLnSF_poly)
				rm(engWLAuthBndryLnSF_polyUniq)
				rm(subsetDt)
				rm(subsetDt_indiv)
								
				gc() 
				
				slctnCmbndSpSF_longLatCase <- st_transform(slctnCmbndSpSF, "+init=epsg:4326")
				
				#uk <- getData('GADM', country='GBR', level = 2)  								
				
				
				#colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
				
				IntvlBins <-c(0, 250, 500, 750, 1000, 1250, 1500)
				
				mypal <- colorBin(palette = colorSet2, domain = slctnCmbndSpSF_longLatCase$`Predicted Cases`, bins = IntvlBins, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = FALSE, right = FALSE)
				
				
				#Number of cases 
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936,, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = slctnCmbndSpSF_longLatCase,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(slctnCmbndSpSF_longLatCase$`Predicted Cases`),
									popup = paste("Region: ", slctnCmbndSpSF_longLatCase$lad20_nm, "<br>",
												  "Value: ", slctnCmbndSpSF_longLatCase$`Predicted Cases`, "<br>",
												  "Week: ", slctnCmbndSpSF_longLatCase$week, "<br>"),
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"),  pal = mypal, values = slctnCmbndSpSF_longLatCase$`Predicted Cases`,
								  title = "Number of cases",
								  opacity = 1)
				
				
				
				# Local office data -----------------------------------------------------
							
									
								
				}
						


			})
			
			
			plotDataLeafletImp <<- isolate(plotDataLeaflet) %>% debounce(500)
			
			output$map <<- renderLeaflet({
				plotDataLeafletImp()
			}) 
			
			outputOptions(output, "map", suspendWhenHidden=FALSE)
		
		} else {  #in simulation mode 
		
			plotDataLeafletSim <- eventReactive(input$mapProcessId, {
					
							
				
				library(leaflet)
				
				
				if (isolate(!lAuthFlag$grpLAuth)) {  #Default to use PHE centres 

				
				
			
				rm(data_preJntPrdct)	
				rm(subsetDt_indiv)	
				rm(outputUnscldWk)		
				rm(minPrdctdWk)	
				rm(maxPrdctdWk)	
				rm(prdctdWks)
				rm(outputCasesUnscld)
				rm(prdctOutpt)
				rm(wkColNames)
				rm(samplesActual)
				rm(subsetDt)
				rm(subsetDt_indiv)
				
				gc() 
				
			    # add PHE centre spatial data --------------------------------------------------------------
				
				#not be used for model generation as Deprivation indices e.g. rank should not be summed. Hence, only counts are applicable e.g. age count. Only used for prediction purposes 
				
				#PHE_centreGrp <- aggregate(Predicted Cases~PHEC19CD+PHEC19NM+weekNumber, slctnCmbnd, sum, na.rm = FALSE)
				
				#no actions
				
				slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% max(slctnCmbnd$week), ] 
				
				slctnCmbndGrp <- aggregate(`Simulated Predicted Cases`~PHEC19CD+PHEC19NM+week, slctnCmbnd, sum, na.rm = FALSE)
				
		
				
				#slctnCmbndGrp <- aggregate(`Predicted Cases`~PHEC19CD+PHEC19NM, slctnCmbnd, sum, na.rm = FALSE)

				#do this latter on within plot 
				phe_CentresBndryLnSF <- sf::st_as_sf(phe_CentresBndryLn)
								
				phe_CentresBndryLnSF_poly <- st_cast(phe_CentresBndryLnSF,"POLYGON")

				# remove PHE centre codes not in slctnCmbndGrp centre codes 
				phe_CentresBndryLnSF_poly <- phe_CentresBndryLnSF_poly[phe_CentresBndryLnSF_poly$phec16cd %in% as.character(slctnCmbndGrp$PHEC19CD),]

				# remove duplicate PHE centre codes 
				phe_CentresBndryLnSF_polyUniq <- phe_CentresBndryLnSF_poly[!duplicated(phe_CentresBndryLnSF_poly$phec16cd),]

				PHE_centreGrpSp <- merge(slctnCmbndGrp, phe_CentresBndryLnSF_polyUniq, by.x="PHEC19CD",by.y="phec16cd")

				PHE_centreGrpSpSF <- sf::st_as_sf(PHE_centreGrpSp)
				
				
				
				PHE_centreGrpSpSF_longLatCase <- st_transform(PHE_centreGrpSpSF, "+init=epsg:4326")				
						
				
				#colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
				
				IntvlBins <-c(0, 250, 500, 750, 1000, 1250, 1500)
						
				mypal <- colorBin(palette = colorSet2, domain = PHE_centreGrpSpSF_longLatCase$`Simulated Predicted Cases`, bins = IntvlBins, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = FALSE, right = FALSE)

				
				#Number of cases 
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936,, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = PHE_centreGrpSpSF_longLatCase,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(PHE_centreGrpSpSF_longLatCase$`Simulated Predicted Cases`),
									popup = paste("Region: ", PHE_centreGrpSpSF_longLatCase$PHEC19NM, "<br>",
												  "Value: ", PHE_centreGrpSpSF_longLatCase$`Simulated Predicted Cases`, "<br>",
												  "Week: ", PHE_centreGrpSpSF_longLatCase$week, "<br>"),
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"), pal = mypal, values = PHE_centreGrpSpSF_longLatCase$`Simulated Predicted Cases`,
								  title = "Number of cases",
								  opacity = 1)
				
								

				} else {	  
				
				# -----------------  only for local authority but not for PHE centres -----------------------------------

											
						
				slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% max(slctnCmbnd$week), ] 
				
				if (length(unique(slctnCmbnd$week)) > 1) {
					slctnCmbndGrp_sim <- aggregate(`Simulated Predicted Cases`~lad20_nm+lad20_cd+week, slctnCmbnd, sum, na.rm = FALSE)
				} else {
					slctnCmbndGrp_sim <- slctnCmbnd[, c("lad20_nm", "lad20_cd", "week", "Simulated Predicted Cases")]
				}
				
				# combine with local authority spatial data ----------------------------------------------
				
				# remove LA codes not in jointWeekNCases_Mltd_time 
				engWLAuthBndryLnSF_poly <- engWLAuthBndryLnSF_poly[engWLAuthBndryLnSF_poly$lad19cd %in% as.character(slctnCmbndGrp_sim$lad20_cd),]

				# remove duplicate LA codes 
				engWLAuthBndryLnSF_polyUniq <- engWLAuthBndryLnSF_poly[!duplicated(engWLAuthBndryLnSF_poly$lad19cd),]

				# merge slctnCmbndGrp_sim LA code with engWLAuthBndryLnSF_polyUniq LA code 
				slctnCmbndGrp_sim <- merge(slctnCmbndGrp_sim, engWLAuthBndryLnSF_polyUniq, by.x="lad20_cd",by.y="lad19cd")

				# combine with local authority spatial data ----------------------------------------------


				#slctnCmbnd <- slctnCmbnd[, c("lad19_nm", "lad19_cd", "Predicted Cases", "long", "lat", "geometry")]
				
				slctnCmbndSpSF_sim <- sf::st_as_sf(slctnCmbndGrp_sim)
				
				
				
				rm(slctnCmbndGrp)	
				rm(engWLAuthBndryLnSF_polyUniq)	
				rm(engWLAuthBndryLnSF_poly)	
				#rm(nhsLclOffcBndryLn)		
				rm(subsetDt)
				rm(subsetDt_indiv)
								
				
				gc() 
				
				slctnCmbndSpSF_longLatCase_sim <- st_transform(slctnCmbndSpSF_sim, "+init=epsg:4326")
				
				#uk <- getData('GADM', country='GBR', level = 2)  								
				
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
				
				IntvlBins <-c(0, 250, 500, 750, 1000, 1250, 1500)
				
				#pretty = FALSE result in exact bin numbers 
				mypal <- colorBin(palette = colorSet2, domain = slctnCmbndSpSF_longLatCase_sim$`Simulated Predicted Cases`, bins = IntvlBins, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = FALSE, right = FALSE )
				
				
				#Number of cases 
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936,, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = slctnCmbndSpSF_longLatCase_sim,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(slctnCmbndSpSF_longLatCase_sim$`Simulated Predicted Cases`),
									popup = paste("Region: ", slctnCmbndSpSF_longLatCase_sim$lad20_nm, "<br>",
												  "Value: ", slctnCmbndSpSF_longLatCase_sim$`Simulated Predicted Cases`, "<br>",
												  "Week: ", slctnCmbndSpSF_longLatCase_sim$week, "<br>"),
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"), pal = mypal, values = slctnCmbndSpSF_longLatCase_sim$`Simulated Predicted Cases`,
								  title = "Number of cases",
								  opacity = 1)
				
				
				
				# Local office data -----------------------------------------------------
				
							
				
				
				}
					
		})
		
			plotDataLeafletImpSim <<- isolate(plotDataLeafletSim) %>% debounce(500)
			
			output$mapSim <<- renderLeaflet({
				plotDataLeafletImpSim()
			}) 
			
			outputOptions(output, "mapSim", suspendWhenHidden=FALSE)
		}
	
	#}
  
  } 
  
  })
  
  
  observeEvent(input$mapMtlyProcessId, { 
  

  if (isolate(input$tabs1) == "Map: Mortality") {
	
    
		if (!input$simulation) {
		    
			plotDataLeafletMtly <- eventReactive(input$mapMtlyProcessId, {
			
												
				library(leaflet)
				
				
				if (isolate(!lAuthFlag$grpLAuth)) {  #Default to use PHE centres 

				
				
			
				rm(data_preJntPrdct)	
				rm(outputUnscldWk)	
				#rm(nhsLclOffcBndryLn)		
				rm(minPrdctdWk)	
				rm(maxPrdctdWk)	
				rm(prdctdWks)
				rm(outputCasesUnscld)
				rm(prdctOutpt)
				rm(samplesActual)
				rm(subsetDt)
				rm(subsetDt_indiv)
				
				gc() 
				
			    # add PHE centre spatial data --------------------------------------------------------------
				
				#not be used for model generation as Deprivation indices e.g. rank should not be summed. Hence, only counts are applicable e.g. age count. Only used for prediction purposes 
				
				#PHE_centreGrp <- aggregate(Predicted Mortality~PHEC19CD+PHEC19NM+weekNumber, slctnCmbnd, sum, na.rm = FALSE)
				
				
				#filter by weeks  
				#slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% input$checkWeek, ]  #input$checkWeek
				slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% max(slctnCmbnd$week), ] 
				
				slctnCmbndGrp <- aggregate(`Predicted Mortality`~PHEC19CD+PHEC19NM+week, slctnCmbnd, sum, na.rm = FALSE)
				
				
				#slctnCmbndGrp <- aggregate(`Predicted Mortality`~PHEC19CD+PHEC19NM, slctnCmbnd, sum, na.rm = FALSE)

				#do this latter on within plot 
				phe_CentresBndryLnSF <- sf::st_as_sf(phe_CentresBndryLn)
								
				phe_CentresBndryLnSF_poly <- st_cast(phe_CentresBndryLnSF,"POLYGON")

				# remove PHE centre codes not in slctnCmbndGrp centre codes 
				phe_CentresBndryLnSF_poly <- phe_CentresBndryLnSF_poly[phe_CentresBndryLnSF_poly$phec16cd %in% as.character(slctnCmbndGrp$PHEC19CD),]

				# remove duplicate PHE centre codes 
				phe_CentresBndryLnSF_polyUniq <- phe_CentresBndryLnSF_poly[!duplicated(phe_CentresBndryLnSF_poly$phec16cd),]

				PHE_centreGrpSp <- merge(slctnCmbndGrp, phe_CentresBndryLnSF_polyUniq, by.x="PHEC19CD",by.y="phec16cd")

				PHE_centreGrpSpSF <- sf::st_as_sf(PHE_centreGrpSp)
				
				
				
				PHE_centreGrpSpSF_longLatCase <- st_transform(PHE_centreGrpSpSF, "+init=epsg:4326")				
						
				
				#colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
				
				IntvlBins <-c(0, 250, 500, 750, 1000, 1250, 1500)
				
				mypal <- colorBin(palette = colorSet2, domain = PHE_centreGrpSpSF_longLatCase$`Predicted Mortality`, bins = IntvlBins, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = FALSE, right = FALSE)

				
				#Number of Mortality 
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936,, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = PHE_centreGrpSpSF_longLatCase,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(PHE_centreGrpSpSF_longLatCase$`Predicted Mortality`),
									popup = paste("Region: ", PHE_centreGrpSpSF_longLatCase$PHEC19NM, "<br>",
												  "Value: ", PHE_centreGrpSpSF_longLatCase$`Predicted Mortality`, "<br>",
												  "Week: ", PHE_centreGrpSpSF_longLatCase$week, "<br>"),
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"), pal = mypal, values = PHE_centreGrpSpSF_longLatCase$`Predicted Mortality`,
								  title = "Number of Mortality",
								  opacity = 1)
				
								

				} else {	  
				
				# -----------------  only for local authority but not for PHE centres -----------------------------------
			
								
				#filter by weeks  
				#slctnCmbnd <- slctnCmbnd[slctnCmbnd$week%in% input$checkWeek, ]  #input$checkWeek
				slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% max(slctnCmbnd$week), ] 
				
				if (length(unique(slctnCmbnd$week)) > 1) {
					slctnCmbndGrp <- aggregate(`Predicted Mortality`~lad20_nm+lad20_cd+week, slctnCmbnd, sum, na.rm = FALSE)
				} else {
					slctnCmbndGrp <- slctnCmbnd[, c("lad20_nm", "lad20_cd", "week", "Predicted Mortality")]
				}
				
				# combine with local authority spatial data ----------------------------------------------
				
				# remove LA codes not in jointWeekNCases_Mltd_time 
				engWLAuthBndryLnSF_poly <- engWLAuthBndryLnSF_poly[engWLAuthBndryLnSF_poly$lad19cd %in% as.character(slctnCmbndGrp$lad20_cd),]

				# remove duplicate LA codes 
				engWLAuthBndryLnSF_polyUniq <- engWLAuthBndryLnSF_poly[!duplicated(engWLAuthBndryLnSF_poly$lad19cd),]

				# merge slctnCmbndGrp LA code with engWLAuthBndryLnSF_polyUniq LA code 
				slctnCmbndGrp <- merge(slctnCmbndGrp, engWLAuthBndryLnSF_polyUniq, by.x="lad20_cd",by.y="lad19cd")

				# combine with local authority spatial data ----------------------------------------------
			
				
				slctnCmbndSpSF <- sf::st_as_sf(slctnCmbndGrp)
				
				
				
				rm(slctnCmbndGrp)	
				#rm(engCCGBndryLn)
				#rm(subsetDt)
				#rm(slctnCmbnd)
				rm(engWLAuthBndryLnSF)
				rm(engWLAuthBndryLnSF_poly)
				rm(engWLAuthBndryLnSF_polyUniq)
				rm(subsetDt)
				rm(subsetDt_indiv)
								
				gc() 
				
				slctnCmbndSpSF_longLatCase <- st_transform(slctnCmbndSpSF, "+init=epsg:4326")
				
				#uk <- getData('GADM', country='GBR', level = 2)  								
				
				
				#colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
				
				IntvlBins <-c(0, 250, 500, 750, 1000, 1250, 1500)
				
				mypal <- colorBin(palette = colorSet2, domain = slctnCmbndSpSF_longLatCase$`Predicted Mortality`, bins = IntvlBins, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = FALSE, right = FALSE)
				
				
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936,, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = slctnCmbndSpSF_longLatCase,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(slctnCmbndSpSF_longLatCase$`Predicted Mortality`),
									popup = paste("Region: ", slctnCmbndSpSF_longLatCase$lad20_nm, "<br>",
												  "Value: ", slctnCmbndSpSF_longLatCase$`Predicted Mortality`, "<br>",
												  "Week: ", slctnCmbndSpSF_longLatCase$week, "<br>"),
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"),  pal = mypal, values = slctnCmbndSpSF_longLatCase$`Predicted Mortality`,
								  title = "Number of Mortality",
								  opacity = 1)
				
				
				
				# Local office data -----------------------------------------------------
							
									
								
				}
						


			})
			
			
			plotDataLeafletImpMtly <<- isolate(plotDataLeafletMtly) %>% debounce(500)
			
			output$mapMtly <<- renderLeaflet({
				plotDataLeafletImpMtly()
			}) 
			
			outputOptions(output, "mapMtly", suspendWhenHidden=FALSE)
		
		} else {  #in simulation mode 
		
			plotDataLeafletSimMtly <- eventReactive(input$mapMtlyProcessId, {
					
							
				
				library(leaflet)
				
				
				if (isolate(!lAuthFlag$grpLAuth)) {  #Default to use PHE centres 

				
				
			
				rm(data_preJntPrdct)	
				rm(subsetDt_indiv)	
				rm(outputUnscldWk)		
				rm(minPrdctdWk)	
				rm(maxPrdctdWk)	
				rm(prdctdWks)
				rm(outputCasesUnscld)
				rm(prdctOutpt)
				rm(wkColNames)
				rm(samplesActual)
				rm(subsetDt)
				rm(subsetDt_indiv)
				
				gc() 
				
			    # add PHE centre spatial data --------------------------------------------------------------
								
				
				#no actions
				
				slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% max(slctnCmbnd$week), ] 
				
				slctnCmbndGrp <- aggregate(`Simulated Predicted Mortality`~PHEC19CD+PHEC19NM+week, slctnCmbnd, sum, na.rm = FALSE)
				
	
				#do this latter on within plot 
				phe_CentresBndryLnSF <- sf::st_as_sf(phe_CentresBndryLn)
								
				phe_CentresBndryLnSF_poly <- st_cast(phe_CentresBndryLnSF,"POLYGON")

				# remove PHE centre codes not in slctnCmbndGrp centre codes 
				phe_CentresBndryLnSF_poly <- phe_CentresBndryLnSF_poly[phe_CentresBndryLnSF_poly$phec16cd %in% as.character(slctnCmbndGrp$PHEC19CD),]

				# remove duplicate PHE centre codes 
				phe_CentresBndryLnSF_polyUniq <- phe_CentresBndryLnSF_poly[!duplicated(phe_CentresBndryLnSF_poly$phec16cd),]

				PHE_centreGrpSp <- merge(slctnCmbndGrp, phe_CentresBndryLnSF_polyUniq, by.x="PHEC19CD",by.y="phec16cd")

				PHE_centreGrpSpSF <- sf::st_as_sf(PHE_centreGrpSp)
				
				
				
				PHE_centreGrpSpSF_longLatCase <- st_transform(PHE_centreGrpSpSF, "+init=epsg:4326")				
						
				
				#colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026')
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
				
				IntvlBins <-c(0, 250, 500, 750, 1000, 1250, 1500)
						
				mypal <- colorBin(palette = colorSet2, domain = PHE_centreGrpSpSF_longLatCase$`Simulated Predicted Mortality`, bins = IntvlBins, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = FALSE, right = FALSE)

				
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936,, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = PHE_centreGrpSpSF_longLatCase,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(PHE_centreGrpSpSF_longLatCase$`Simulated Predicted Mortality`),
									popup = paste("Region: ", PHE_centreGrpSpSF_longLatCase$PHEC19NM, "<br>",
												  "Value: ", PHE_centreGrpSpSF_longLatCase$`Simulated Predicted Mortality`, "<br>",
												  "Week: ", PHE_centreGrpSpSF_longLatCase$week, "<br>"),
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"), pal = mypal, values = PHE_centreGrpSpSF_longLatCase$`Simulated Predicted Mortality`,
								  title = "Number of Mortality",
								  opacity = 1)
				
								

				} else {	  
				
				# -----------------  only for local authority but not for PHE centres -----------------------------------

											
						
				slctnCmbnd <- slctnCmbnd[slctnCmbnd$week %in% max(slctnCmbnd$week), ] 
				
				if (length(unique(slctnCmbnd$week)) > 1) {
					slctnCmbndGrp_sim <- aggregate(`Simulated Predicted Mortality`~lad20_nm+lad20_cd+week, slctnCmbnd, sum, na.rm = FALSE)
				} else {
					slctnCmbndGrp_sim <- slctnCmbnd[, c("lad20_nm", "lad20_cd", "week", "Simulated Predicted Mortality")]
				}
				
				# combine with local authority spatial data ----------------------------------------------
				
				# remove LA codes not in jointWeekNCases_Mltd_time 
				engWLAuthBndryLnSF_poly <- engWLAuthBndryLnSF_poly[engWLAuthBndryLnSF_poly$lad19cd %in% as.character(slctnCmbndGrp_sim$lad20_cd),]

				# remove duplicate LA codes 
				engWLAuthBndryLnSF_polyUniq <- engWLAuthBndryLnSF_poly[!duplicated(engWLAuthBndryLnSF_poly$lad19cd),]

				# merge slctnCmbndGrp_sim LA code with engWLAuthBndryLnSF_polyUniq LA code 
				slctnCmbndGrp_sim <- merge(slctnCmbndGrp_sim, engWLAuthBndryLnSF_polyUniq, by.x="lad20_cd",by.y="lad19cd")

				# combine with local authority spatial data ----------------------------------------------

				
				slctnCmbndSpSF_sim <- sf::st_as_sf(slctnCmbndGrp_sim)
				
				
				
				rm(slctnCmbndGrp)	
				rm(engWLAuthBndryLnSF_polyUniq)	
				rm(engWLAuthBndryLnSF_poly)	
				#rm(nhsLclOffcBndryLn)		
				rm(subsetDt)
				rm(subsetDt_indiv)
								
				
				gc() 
				
				slctnCmbndSpSF_longLatCase_sim <- st_transform(slctnCmbndSpSF_sim, "+init=epsg:4326")
				
				#uk <- getData('GADM', country='GBR', level = 2)  								
				
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
				
				IntvlBins <-c(0, 250, 500, 750, 1000, 1250, 1500)
				
				#pretty = FALSE result in exact bin numbers 
				mypal <- colorBin(palette = colorSet2, domain = slctnCmbndSpSF_longLatCase_sim$`Simulated Predicted Mortality`, bins = IntvlBins, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = FALSE, right = FALSE )
				
			
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936,, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = slctnCmbndSpSF_longLatCase_sim,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(slctnCmbndSpSF_longLatCase_sim$`Simulated Predicted Mortality`),
									popup = paste("Region: ", slctnCmbndSpSF_longLatCase_sim$lad20_nm, "<br>",
												  "Value: ", slctnCmbndSpSF_longLatCase_sim$`Simulated Predicted Mortality`, "<br>",
												  "Week: ", slctnCmbndSpSF_longLatCase_sim$week, "<br>"),
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"), pal = mypal, values = slctnCmbndSpSF_longLatCase_sim$`Simulated Predicted Mortality`,
								  title = "Number of Mortality",
								  opacity = 1)
				
				
				
				# Local office data -----------------------------------------------------
				
							
				
				
				}
					
		})
		
			plotDataLeafletImpSimMtly <<- isolate(plotDataLeafletSimMtly) %>% debounce(500)
			
			output$mapSimMtly <<- renderLeaflet({
				plotDataLeafletImpSimMtly()
			}) 
			
			outputOptions(output, "mapSimMtly", suspendWhenHidden=FALSE)
		}
	
	#}
  
  } 
  
  })
  
  
  observeEvent(input$trendProcessId, {
  
  if (input$tabs1 == "Heat Map: Case Density") {
  
  output$map <<- NULL
  output$plot_access_web <<- NULL

  output$map <- renderLeaflet({
			
			subsetDt <- cardioData
			
				#filter by hospital 
				subsetDt <- subsetDt[subsetDt$hospital %in% input$check2, ]  #input$check2
				#It should be okay to filter by hospital as lsoa is a region and no particular patient is identifiable
				
				#filter by year 
				subsetDt <- subsetDt[subsetDt$year %in% input$checkYear, ]  #input$checkYear
	
				if (!is.null(input$checkPrty)) {
					#filter by priority 
					subsetDt <- subsetDt[subsetDt$X2.35.Operative.Urgency %in% input$checkPrty, ]  #input$checkPrty
				}
				
				#filter by type  
				if ( "AorticValveSurgery" %in% input$checkSgryType ) {
					
					subsetDt <- subsetDt[subsetDt$AorticValveSurgery == 1, ] 
					
					if (input$checkSubType == "AVR_No_CABG") {
						subsetDt <- subsetDt[subsetDt$AVR_No_CABG == "1", ]
						subsetDt <- subsetDt[!is.na(subsetDt$AVR_No_CABG),]						
					} else if (input$checkSubType == "AVR_CABG") {
						subsetDt <- subsetDt[subsetDt$AVR_CABG == "1", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$AVR_CABG),]	
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				} else if ( "MitralValveSurgery" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$MitralValveSurgery == 1, ] 
					
					if (input$checkSubType == "repair") {
						subsetDt <- subsetDt[subsetDt$MVprocedure == "repair", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					} else if (input$checkSubType == "replacement") {
						subsetDt <- subsetDt[subsetDt$MVprocedure == "replacement", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				} else if ( "Surgery_on_thoracic_aorta" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$Surgery_on_thoracic_aorta == 1, ] 
					
					if (input$checkSubType == "chronic.aneurysm") {
						subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "chronic.aneurysm", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					} else if (input$checkSubType == "acute.aortic.dissection") {
						subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "acute.aortic.dissection", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}								
					
				} else if ( "isolatedCABG" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$CABG == 1, ] 
					
					if (input$checkSubType == "bilatralITA") {
						subsetDt <- subsetDt[subsetDt$bilatralITA == "BilateralITA", ]  #is singleITA no required for selection?
						subsetDt <- subsetDt[!is.na(subsetDt$bilatralITA),]
					} else if (input$checkSubType == "RadialArtery") {
						subsetDt <- subsetDt[subsetDt$RadialArtery == "RadialArtery", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$RadialArtery),]
					} else if (input$checkSubType == "offpumpCABG") {
						subsetDt <- subsetDt[subsetDt$offpumpCABG == "1", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$offpumpCABG),]
					
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				}
	
	
			if (!nrow(subsetDt) == 0) {
			
				output$message <- NULL
			
				output$summary <- NULL
					  
				output$summary2 <- NULL
			
				library(leaflet)
				# leaflet() %>%
				  # addTiles()
				
				subsetDt$count <- 1
				
				casesByLSOA <- aggregate(count~LSOA.code, subsetDt, sum, na.rm = TRUE)

				# remove lsoa codes not in nacsa 
				lsoaCCGLookup_1 <- lsoaCCGLookup[lsoaCCGLookup$LSOA11CD %in% as.character(casesByLSOA$LSOA.code),]

				# remove duplicate lsoa codes 
				lsoaCCGLookup_2 <- lsoaCCGLookup_1[!duplicated(lsoaCCGLookup_1$LSOA11CD),]
			
				# merge casesByLSOA lsoa code with lsoaCCGLookup_2 lookup 
				ccgGrp <- merge(casesByLSOA, lsoaCCGLookup_2, by.x="LSOA.code",by.y="LSOA11CD")
				
				# aggregate to obtain count by unique CCGs
				casesByCCG <- aggregate(count~CCG18CD+CCG18NM, ccgGrp, sum, na.rm = TRUE)
				
				
				
				# load CCG population estimate -----------------------------------------------------
				ppltnList <- NULL
				
				if ("2009" %in% input$checkYear) {
					censusAge2009 <- fread("data/CCG_PopulationEst_2009.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)
					
					ppltnList <- list(censusAge2009)
				}
				if ("2010" %in% input$checkYear) {
					censusAge2010 <- fread("data/CCG_PopulationEst_2010.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)
					
					ppltnList <- c(ppltnList, list(censusAge2010))
				}
				if ("2011" %in% input$checkYear) {
					censusAge2011 <- fread("data/CCG_PopulationEst_2011.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)
				
					ppltnList <- c(ppltnList, list(censusAge2011))
				}
				if ("2012" %in% input$checkYear) {
					censusAge2012 <- fread("data/CCG_PopulationEst_2012.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)
	
					ppltnList <- c(ppltnList, list(censusAge2012))
				}
				if ("2013" %in% input$checkYear) {
					censusAge2013 <- fread("data/CCG_PopulationEst_2013.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)

					ppltnList <- c(ppltnList, list(censusAge2013))
				}
				if ("2014" %in% input$checkYear) {
					censusAge2014 <- fread("data/CCG_PopulationEst_2014.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)

					ppltnList <- c(ppltnList, list(censusAge2014))
				}
				if ("2015" %in% input$checkYear) {
					censusAge2015 <- fread("data/CCG_PopulationEst_2015.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)

					ppltnList <- c(ppltnList, list(censusAge2015))
				}
				if ("2016" %in% input$checkYear) {
					censusAge2016 <- fread("data/CCG_PopulationEst_2016.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)
			
					ppltnList <- c(ppltnList, list(censusAge2016))
				}
				if ("2017" %in% input$checkYear) {
					censusAge2017 <- fread("data/CCG_PopulationEst_2017.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)
	
					ppltnList <- c(ppltnList, list(censusAge2017))
				}
				if ("2018" %in% input$checkYear) {
					censusAge2018 <- fread("data/CCG_PopulationEst_2018.csv", select=c('Area_Code', 'all_ages'), data.table=FALSE)
			
					ppltnList <- c(ppltnList, list(censusAge2018))
				}
				# ppltnList <- ppltnList[-which(lapply(ppltnList,is.null) == T)]


				# ppltnEst10yrs <- Reduce((function() {counter = 1
													   # function(x, y) {
													   # counter <<- counter + 1
													   # d = merge(x, y, by = 'Area_Code')
													   # setnames(d, c(head(names(d), -1), paste0('count.x', counter)))
													  # }})(), list(censusAge2009, censusAge2010, censusAge2011, censusAge2012, censusAge2013, censusAge2014, censusAge2015, censusAge2016, censusAge2017, censusAge2018))
													  
				ppltnEst10yrs <- Reduce((function() {counter = 1
													   function(x, y) {
													   counter <<- counter + 1
													   d = merge(x, y, by = 'Area_Code')
													   setnames(d, c(head(names(d), -1), paste0('count.x', counter)))
													  }})(), ppltnList)

				# colnames(ppltnEst10yrs) <- c("CCG_Code", "2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018")

				ppltnYrs <- yearList[yearList %in% input$checkYear]  

				colnames(ppltnEst10yrs) <- c("CCG_Code", ppltnYrs)

				ppltnEst10yrs[] <- lapply(ppltnEst10yrs, gsub, pattern=',', replacement='')

				# convert yr columns to integer, ccg code to factor 
				ppltnEst10yrs[] <- lapply(ppltnEst10yrs, type.convert)

				# load CCG population estimate -----------------------------------------------------
				
				
				# averaging population estimates based on input$year ---------------------------------
				
				ppltnEstCCG <- as.data.frame(ppltnEst10yrs$CCG_Code)
				
				colnames(ppltnEstCCG) <- c("CCG_Code")
				
				
				#get yrs columns based on selection
				ppltnEstSlctd <- ppltnEst10yrs[ , colnames(ppltnEst10yrs) %in% input$checkYear, drop = FALSE]
				
				ppltnEstSlctCount <- add_column(ppltnEstCCG, ppltnEstSlctd, .after = "CCG_Code")
				

				
				if (isolate(lAuthFlag$grpLAuth)) {  #Default to use Local Authority regions

				#merge with ccg to nhs local office lookup 	
				#casesByCCG			
				
				# remove ccg codes not in casesByCCG 
				ccgToNHSLclOffcLookup_1 <- ccgToNHSLclOffcLookup[ccgToNHSLclOffcLookup$CCG18CD %in% as.character(casesByCCG$CCG18CD),]

				# remove duplicate ccg codes 
				ccgToNHSLclOffcLookup_2 <- ccgToNHSLclOffcLookup_1[!duplicated(ccgToNHSLclOffcLookup_1$CCG18CD),]
				
				# merge casesByCCG ccg code with ccgToNHSLclOffcLookup_2 lookup 
				ccgGrp_Lkp <- merge(casesByCCG, ccgToNHSLclOffcLookup_2, by.x="CCG18CD",by.y="CCG18CD")
				
				
				# --------------- merge with population data ---------------------------------------
				
				# remove CCG codes not in population estimate dataset  
				ccgGrp_Lkp_1 <- ccgGrp_Lkp[ccgGrp_Lkp$CCG18CD %in% as.character(ppltnEstSlctCount$CCG_Code),]

				# remove duplicate CCG codes 
				ccgGrp_Lkp_2 <- ccgGrp_Lkp_1[!duplicated(ccgGrp_Lkp_1$CCG18CD),]
				
				#join ccg ccgGrp_Lkp_2 data to population data 
				ccgGrp_Lkp_2_ppltnCount <- merge(ccgGrp_Lkp_2, ppltnEstSlctCount, by.x="CCG18CD",by.y="CCG_Code")	
				
				# --------------- merge with population data ---------------------------------------
								
				ccgGrp_Lkp_2_ppltnCount$CCG18CD	<- NULL
				ccgGrp_Lkp_2_ppltnCount$CCG18NM <- NULL
				#ccgGrp_Lkp_2_ppltnCount$CCG_Code.1 <- NULL
				
				
				# aggregate to obtain count and population totals per year by unique nhs local offices
				casesByLclOffc_ppltn <- aggregate(.~NHSRLO18CD+NHSRLO18NM, ccgGrp_Lkp_2_ppltnCount, sum, na.rm = TRUE)
								
				
				#obtain mean of population for selected yrs
				casesByLclOffc_ppltn$avgSlctPpltn <- rowMeans(casesByLclOffc_ppltn[, colnames(casesByLclOffc_ppltn) %in% input$checkYear, drop = FALSE], na.rm=TRUE)
				
				# up to here 
				nhsLclOffcBndryLn$bng_e <- NULL
				nhsLclOffcBndryLn$bng_n <- NULL
				nhsLclOffcBndryLn$st_areasha <- NULL
				nhsLclOffcBndryLn$st_lengths <- NULL

					
				nhsLclOffcBndryLnSF <- sf::st_as_sf(nhsLclOffcBndryLn)
				
				nhsLclOffcBndryLnSF_poly <- st_cast(nhsLclOffcBndryLnSF,"POLYGON")
				
				# remove local office codes not in casesByLclOffc_ppltn 
				nhsLclOffcBndryLnSF_poly_1 <- nhsLclOffcBndryLnSF_poly[nhsLclOffcBndryLnSF_poly$nhsrlo18cd %in% as.character(casesByLclOffc_ppltn$NHSRLO18CD), "nhsrlo18cd"]

				# remove duplicate local office codes 
				nhsLclOffcBndryLnSF_poly_2 <- nhsLclOffcBndryLnSF_poly_1[!duplicated(nhsLclOffcBndryLnSF_poly_1$nhsrlo18cd),]
				
				localOfficeGrpSp <- merge(casesByLclOffc_ppltn, nhsLclOffcBndryLnSF_poly_2, by.x="NHSRLO18CD",by.y="nhsrlo18cd")
				
				localOfficeGrpSpSF <- sf::st_as_sf(localOfficeGrpSp)
				
			
				#rm(cardioData)	
				#rm(censusAge)	
				rm(casesByLSOA)	
				#rm(nhsLclOffcBndryLn)		
				rm(subsetDt)	
				rm(casesByCCG)	
				rm(ccgToNHSLclOffcLookup_1)
				rm(ccgToNHSLclOffcLookup_2)
				rm(ccgGrp_Lkp)
				rm(ccgGrp_Lkp_1)
				rm(ccgGrp_Lkp_2)
				rm(ccgGrp_Lkp_2_ppltnCount)
				rm(casesByLclOffc_ppltn)
				rm(nhsLclOffcBndryLnSF)
				rm(nhsLclOffcBndryLnSF_poly)
				rm(nhsLclOffcBndryLnSF_poly_1)
				rm(nhsLclOffcBndryLnSF_poly_2)
				rm(localOfficeGrpSp)
				
				gc() 
				
				localOfficeGrpSpSF_longLat <- st_transform(localOfficeGrpSpSF, "+init=epsg:4326")
											
					
				#population density in %
				localOfficeGrpSpSF_longLat$popDensity <- localOfficeGrpSpSF_longLat$count / localOfficeGrpSpSF_longLat$avgSlctPpltn * 100
				
				#population density per million 
				localOfficeGrpSpSF_longLat$popDensity <- localOfficeGrpSpSF_longLat$popDensity * 10000
				
				#round cases per million to whole number 
				localOfficeGrpSpSF_longLat$popDensity <- round(localOfficeGrpSpSF_longLat$popDensity)
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
						
				mypal <- colorBin(palette = colorSet2, domain = localOfficeGrpSpSF_longLat$popDensity, bins = 5, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = TRUE, right = FALSE)
								
			
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = localOfficeGrpSpSF_longLat,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(localOfficeGrpSpSF_longLat$popDensity),
									popup = paste("Region: ", localOfficeGrpSpSF_longLat$NHSRLO18NM, "<br>",
												  "Value: ", localOfficeGrpSpSF_longLat$popDensity, "<br>"),	  
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"), pal = mypal, values = localOfficeGrpSpSF_longLat$popDensity,
								  labFormat = labelFormat(suffix = "µ"),
								  title = "Cases per million",
								  opacity = 1)
							

				} else {	  
				
				# -----------------  only for CCG but not for local office -----------------------------------

				#obtain mean of population for selected yrs
				avgSlctPpltn <- rowMeans(ppltnEstSlctd[, colnames(ppltnEstSlctd) %in% input$checkYear, drop = FALSE], na.rm=TRUE)
				
				ppltnEstAvgSlctn <- add_column(ppltnEstCCG, averagePpltnSlctd = avgSlctPpltn, .after = "CCG_Code")
				
				# averaging population estimates based on input$year ---------------------------------
				
				
				# remove CCG codes not in population estimate dataset  
				casesByCCG <- casesByCCG[casesByCCG$CCG18CD %in% as.character(ppltnEstAvgSlctn$CCG_Code),]

				# remove duplicate CCG codes 
				casesByCCG <- casesByCCG[!duplicated(casesByCCG$CCG18CD),]
				
				
				#join ccg cases data to population data 
				ccgGrp <- merge(casesByCCG, ppltnEstAvgSlctn, by.x="CCG18CD",by.y="CCG_Code")	

				
				engCCGBndryLn$ccg18nm <- NULL
				engCCGBndryLn$bng_e <- NULL
				engCCGBndryLn$bng_n <- NULL
				engCCGBndryLn$st_areasha <- NULL
				engCCGBndryLn$st_lengths <- NULL
				
				engCCGBndryLnSF <- sf::st_as_sf(engCCGBndryLn)
				
				engCCGBndryLnSF_poly <- st_cast(engCCGBndryLnSF,"POLYGON")
				
				# remove ccg codes not in nacsa 
				engCCGBndryLnSF_poly <- engCCGBndryLnSF_poly[engCCGBndryLnSF_poly$ccg18cd %in% as.character(ccgGrp$CCG18CD), "ccg18cd"]

				# remove duplicate ccg codes 
				engCCGBndryLnSF_polyUniq <- engCCGBndryLnSF_poly[!duplicated(engCCGBndryLnSF_poly$ccg18cd),]

				ccgGrpGrpSp <- merge(ccgGrp, engCCGBndryLnSF_polyUniq, by.x="CCG18CD",by.y="ccg18cd")
				
				# up to here ------------------------------------------------------------
				
				
				ccgGrpGrpSpSF <- sf::st_as_sf(ccgGrpGrpSp)
				
				rm(casesByLSOA)	
				#rm(engCCGBndryLn)
				rm(subsetDt)
				rm(ccgGrp)
				rm(engCCGBndryLnSF)
				rm(engCCGBndryLnSF_poly)
				rm(engCCGBndryLnSF_polyUniq)
				rm(ccgGrpGrpSp)
								
				gc() 
				
				ccgGrpGrpSpSF_longLat <- st_transform(ccgGrpGrpSpSF, "+init=epsg:4326")
				
				#uk <- getData('GADM', country='GBR', level = 2)  								
				
				#population density in %
				ccgGrpGrpSpSF_longLat$popDensity <- ccgGrpGrpSpSF_longLat$count / ccgGrpGrpSpSF_longLat$averagePpltnSlctd * 100
				
				#population density per million 
				ccgGrpGrpSpSF_longLat$popDensity <- ccgGrpGrpSpSF_longLat$popDensity * 10000
				
				#round cases per million to whole number 
				ccgGrpGrpSpSF_longLat$popDensity <- round(ccgGrpGrpSpSF_longLat$popDensity)
				
				colorSet2 <- c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026', '#000080')
						
				mypal <- colorBin(palette = colorSet2, domain = ccgGrpGrpSpSF_longLat$popDensity, bins = 5, pretty = FALSE, na.color = "#808080", alpha = FALSE, reverse = TRUE, right = FALSE)
				
				leaflet() %>% 
						addProviderTiles("OpenStreetMap.Mapnik") %>%
						setView(lat = 52.1936, lng = -1, zoom = 6) %>%
						addPolygons(data = uk, weight = 1, smoothFactor = 0.2, fillOpacity = 0.1) %>%
						addPolygons(data = ccgGrpGrpSpSF_longLat,
									stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
									fillColor = ~mypal(ccgGrpGrpSpSF_longLat$popDensity),
									popup = paste("Region: ", ccgGrpGrpSpSF_longLat$CCG18NM, "<br>",
												  "Value: ", ccgGrpGrpSpSF_longLat$popDensity, "<br>"),
									highlight = highlightOptions(weight = 10,
											   color = "orange",
											   fillOpacity = 5,
											   bringToFront = TRUE)) %>%
						leaflet::addLegend(position = "bottomright", labels=c("0 - 250", "250 - 500", "500 - 750", "750 - 1000", 
                       "1000 - 1250", "1250 - 1500+"), pal = mypal, values = ccgGrpGrpSpSF_longLat$popDensity,
								  labFormat = labelFormat(suffix = "µ"),
								  title = "Cases per million",
								  opacity = 1)
								
				
				
				# Local office data -----------------------------------------------------
				
				}
						

			} else {
				#no data 
				output$message <- renderText({
							"No results to display for selection"
				})
				
				output$summary <- NULL
					  
				output$summary2 <- NULL
			}
		
	})

			# output$message <<- renderPrint({
							# "summarised "
				# })
	
	
			# output$summary <<- renderPrint({
					  # paste0("False +ve level: ", "", ", False -ve level: ", "" )
				# })
				
			# output$res2 <<- renderPrint({
			  # "summarised "

			# })
				

	  
  
  } else if (input$tabs1 == "Cases by Hospital") {
	
	output$map <<- NULL
	output$plot_access_web <<- NULL
	
	output$plot_access_web <- renderPlot({
	
		par(mar=c(12.5,4.5,1.5,1))
	
			subsetDt <- cardioData
				
				#filter by hospital 
				subsetDt <- subsetDt[subsetDt$hospital %in% input$check2, ]  #input$check2
						
				#filter by year 
				subsetDt <- subsetDt[subsetDt$year %in% input$checkYear, ]  #input$checkYear
				
				if (!is.null(input$checkPrty)) {
					#filter by priority 
					subsetDt <- subsetDt[subsetDt$X2.35.Operative.Urgency %in% input$checkPrty, ]  #input$checkPrty
				}
				
				#filter by type  
				if ( "AorticValveSurgery" %in% input$checkSgryType ) {
					
					subsetDt <- subsetDt[subsetDt$AorticValveSurgery == 1, ] 
					
					if (input$checkSubType == "AVR_No_CABG") {
						subsetDt <- subsetDt[subsetDt$AVR_No_CABG == "1", ]
						subsetDt <- subsetDt[!is.na(subsetDt$AVR_No_CABG),]						
					} else if (input$checkSubType == "AVR_CABG") {
						subsetDt <- subsetDt[subsetDt$AVR_CABG == "1", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$AVR_CABG),]	
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				} else if ( "MitralValveSurgery" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$MitralValveSurgery == 1, ] 
					
					if (input$checkSubType == "repair") {
						subsetDt <- subsetDt[subsetDt$MVprocedure == "repair", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					} else if (input$checkSubType == "replacement") {
						subsetDt <- subsetDt[subsetDt$MVprocedure == "replacement", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				} else if ( "Surgery_on_thoracic_aorta" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$Surgery_on_thoracic_aorta == 1, ] 
					
					if (input$checkSubType == "chronic.aneurysm") {
						subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "chronic.aneurysm", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					} else if (input$checkSubType == "acute.aortic.dissection") {
						subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "acute.aortic.dissection", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}								
					
				} else if ( "isolatedCABG" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$CABG == 1, ] 
					
					if (input$checkSubType == "bilatralITA") {
						subsetDt <- subsetDt[subsetDt$bilatralITA == "BilateralITA", ]  #is singleITA no required for selection?
						subsetDt <- subsetDt[!is.na(subsetDt$bilatralITA),]
					} else if (input$checkSubType == "RadialArtery") {
						subsetDt <- subsetDt[subsetDt$RadialArtery == "RadialArtery", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$RadialArtery),]
					} else if (input$checkSubType == "offpumpCABG") {
						subsetDt <- subsetDt[subsetDt$offpumpCABG == "1", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$offpumpCABG),]
					
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				}
			
			if (!nrow(subsetDt) == 0) {
			
				output$message <- NULL
			
				output$summary <- NULL
					  
				output$summary2 <- NULL
			
			subsetDt$hospital <- gsub('Infirmary', 'Inf.', subsetDt$hospital)
			subsetDt$hospital <- gsub('infirmary', 'Inf.', subsetDt$hospital)
			subsetDt$hospital <- gsub('Hospital', 'H.', subsetDt$hospital)
			subsetDt$hospital <- gsub('hospital', 'H.', subsetDt$hospital)
			
			subsetDt$hospital <- strtrim(subsetDt$hospital, 20)
			
			
			caseFreq <- as.vector(table(subsetDt$hospital))
			
			if (!flag$grpHsp) {
				
				xCoord <- barplot(table(subsetDt$hospital),
					main="Number of cases by Hospital",
					xlab="",
					ylab="Number of cases",
					border="red",
					col="blue",
					density=12,
					las=2,
					ylim=c(0,1.2*max(unlist(as.vector(table(subsetDt$hospital)))))
					)
				
				## Add text at top of bars
				text(x = xCoord, y = caseFreq, label = caseFreq, pos = 3, cex = 0.8, col = "black")

				mtext(text = "Hospital",
				  side = 1,#side 1 = bottom
				  line = 10.5)	
				  
			} else {
			
				subsetDt$count <- 1
				totalCases <- sum(subsetDt$count)
				
				xCoord <- barplot(totalCases,
					main="Number of cases by Hospital",
					xlab="",
					ylab="Number of cases",
					border="red",
					col="blue",
					density=12,
					las=2,
					ylim=c(0,1.2*max(unlist(as.vector(totalCases))))
					)
				
				## Add text at top of bars
				text(x = xCoord, y = totalCases, label = totalCases, pos = 3, cex = 0.8, col = "black")
				
				mtext(text = "All Selected Hospitals",
				  side = 1,#side 1 = bottom
				  line = 1)	
			
			}
			
			rm(subsetDt)
			
			gc() 
			
			output$summary <- renderText({
					  paste0("False +ve level: ", "", ", False -ve level: ", "" )
				})
			
			 
			} else {
				#no data 
				output$message <- renderText({
							"No results to display for selection"
				})
				
				output$summary <- NULL
					  
				output$summary2 <- NULL
			}
			
	})

  } else if (input$tabs1 == "Trend: total number of cases") { 
				
	output$map <<- NULL
	output$plot_access_web <<- NULL
	
	output$trendIntPlot <- renderPlotly({
			
		par(mar=c(10,4.5,7,7))
			
				subsetDt <- cardioData
				
				#filter by hospital 
				subsetDt <- subsetDt[subsetDt$hospital %in% input$check2, ]  #input$check2
						
				#filter by year 
				subsetDt <- subsetDt[subsetDt$year %in% input$checkYear, ]  #input$checkYear
				
				if (!is.null(input$checkPrty)) {
					#filter by priority 
					subsetDt <- subsetDt[subsetDt$X2.35.Operative.Urgency %in% input$checkPrty, ]  #input$checkPrty
				}
				
				#filter by type  
				if ( "AorticValveSurgery" %in% input$checkSgryType ) {
					
					subsetDt <- subsetDt[subsetDt$AorticValveSurgery == 1, ] 
					
					if (input$checkSubType == "AVR_No_CABG") {
						subsetDt <- subsetDt[subsetDt$AVR_No_CABG == "1", ]
						subsetDt <- subsetDt[!is.na(subsetDt$AVR_No_CABG),]						
					} else if (input$checkSubType == "AVR_CABG") {
						subsetDt <- subsetDt[subsetDt$AVR_CABG == "1", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$AVR_CABG),]	
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				} else if ( "MitralValveSurgery" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$MitralValveSurgery == 1, ] 
					
					if (input$checkSubType == "repair") {
						subsetDt <- subsetDt[subsetDt$MVprocedure == "repair", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					} else if (input$checkSubType == "replacement") {
						subsetDt <- subsetDt[subsetDt$MVprocedure == "replacement", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				} else if ( "Surgery_on_thoracic_aorta" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$Surgery_on_thoracic_aorta == 1, ] 
					
					if (input$checkSubType == "chronic.aneurysm") {
						subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "chronic.aneurysm", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					} else if (input$checkSubType == "acute.aortic.dissection") {
						subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "acute.aortic.dissection", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}								
					
				} else if ( "isolatedCABG" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$CABG == 1, ] 
					
					if (input$checkSubType == "bilatralITA") {
						subsetDt <- subsetDt[subsetDt$bilatralITA == "BilateralITA", ]  #is singleITA no required for selection?
						subsetDt <- subsetDt[!is.na(subsetDt$bilatralITA),]
					} else if (input$checkSubType == "RadialArtery") {
						subsetDt <- subsetDt[subsetDt$RadialArtery == "RadialArtery", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$RadialArtery),]
					} else if (input$checkSubType == "offpumpCABG") {
						subsetDt <- subsetDt[subsetDt$offpumpCABG == "1", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$offpumpCABG),]
					
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				}
			
			if (!nrow(subsetDt) == 0) {
			
			
			output$message <- NULL
			
			output$summary <- NULL
				  
			output$summary2 <- NULL
			
			# subsetDt$X4.05.Patient.Status.at.discharge <- gsub('. Alive', '', subsetDt$X4.05.Patient.Status.at.discharge)
			# subsetDt$X4.05.Patient.Status.at.discharge <- gsub('. Dead', '', subsetDt$X4.05.Patient.Status.at.discharge)
			
			# subsetDt$X4.05.Patient.Status.at.discharge <- as.numeric(as.character(subsetDt$X4.05.Patient.Status.at.discharge)) # convert to float
			
			# subsetDt <- subsetDt[!is.na(subsetDt$X4.05.Patient.Status.at.discharge),]
			
			# #remove all % characters so mean can be performed
			# subsetDt$Logistic.Euroscore <- gsub('%', '', subsetDt$Logistic.Euroscore)
			# subsetDt$Logistic.Euroscore <- as.numeric(as.character(subsetDt$Logistic.Euroscore)) # convert to float
			
			# subsetDt$Logistic.Euroscore.Calib <- subsetDt$Logistic.Euroscore / as.numeric(input$selectCal)   #calibration to expected values (in percentage i.e. not divided by 100)
			

			subsetDt$procedureDate <- strptime(as.character(subsetDt$X3.02.Procedure.Date), "%d/%m/%Y")

			subsetDt$count <- 1
			
			#convert to POSIXct format required by ggplot 
			subsetDt$procedureDate <- as.POSIXct(as.character(subsetDt$procedureDate), format="%Y-%m-%d")
			
			
			
			#subsetDt3Col <- subsetDt[, c('count', 'procedureDate')]
			
			subsetDt3Col <- subsetDt
			
			subsetDt3Col$procedureDate <- format(as.Date(subsetDt3Col$procedureDate), "%Y")
				
			#aggregate number of cases and mtly by hospital 
			#casesByHospNMonth <- aggregate(.~procedureDate+hospital, subsetDt3Col, sum, na.rm = TRUE)
			
			if (flagType$grp_Type) {
				
				if ( "AorticValveSurgery" %in% input$checkSgryType ) {
										
					#aggregate number of cases by type and date  
					casesByTypeNMonth_1 <- aggregate(count~procedureDate+AVR_No_CABG, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_2 <- aggregate(count~procedureDate+AVR_CABG, subsetDt3Col, sum, na.rm = TRUE)
					
					#join AVR_No_CABG cases data to AVR_CABG cases data 
					jointTypeNCases <- merge(casesByTypeNMonth_1, casesByTypeNMonth_2, by.x="procedureDate",by.y="procedureDate")	
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x")]  <- c("AVR No CABG")
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.y")]  <- c("AVR CABG")
					
					jointTypeNCases$AVR_No_CABG <- NULL
					jointTypeNCases$AVR_CABG <- NULL
					
					
					jointTypeNCases_Mltd <- reshape2::melt(jointTypeNCases, id.var = c('procedureDate'), variable.name = 'GeneralType')
					
					colnames(jointTypeNCases_Mltd)[which(names(jointTypeNCases_Mltd) == "value")]  <- c("count")
					
					# # if not grouped 
					
					# #aggregate number of cases by type and date  
					# casesByTypeNMonth <- aggregate(count~procedureDate+AVprocedure, subsetDt3Col, sum, na.rm = TRUE)
					
					# colnames(casesByTypeNMonth)[which(names(casesByTypeNMonth) == "AVprocedure")]  <- c("GeneralType")
					
					rm(casesByTypeNMonth_1)
					rm(casesByTypeNMonth_2)
					rm(jointTypeNCases)
			
					gc() 
					
				} else if ( "MitralValveSurgery" %in% input$checkSgryType ) {
					
					# if not grouped 
					
					#aggregate number of cases by type and date  
					casesByTypeNMonth <- aggregate(count~procedureDate+MVprocedure, subsetDt3Col, sum, na.rm = TRUE)
					
					colnames(casesByTypeNMonth)[which(names(casesByTypeNMonth) == "MVprocedure")]  <- c("GeneralType")
					
					jointTypeNCases_Mltd <- casesByTypeNMonth
					
					rm(casesByTypeNMonth)
			
					gc() 
					
				} else if ( "Surgery_on_thoracic_aorta" %in% input$checkSgryType ) {
					
					# if not grouped 
					
					#aggregate number of cases by type and date  
					casesByTypeNMonth <- aggregate(count~procedureDate+aortic_valscular_indication3, subsetDt3Col, sum, na.rm = TRUE)
					
					colnames(casesByTypeNMonth)[which(names(casesByTypeNMonth) == "aortic_valscular_indication3")]  <- c("GeneralType")
					
					jointTypeNCases_Mltd <-	casesByTypeNMonth				
					
					rm(casesByTypeNMonth)
			
					gc() 
					
				} else if ( "isolatedCABG" %in% input$checkSgryType ) {
					
					# if not grouped 
					
					#aggregate number of cases by type and date  
					casesByTypeNMonth_1 <- aggregate(count~procedureDate+bilatralITA, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_1 <- casesByTypeNMonth_1[casesByTypeNMonth_1$bilatralITA == "BilateralITA", ]
					 
					casesByTypeNMonth_2 <- aggregate(count~procedureDate+RadialArtery, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_3 <- aggregate(count~procedureDate+offpumpCABG, subsetDt3Col, sum, na.rm = TRUE)
					
					jointTypeNCases <- Reduce(
						 function(x, y, ...) merge(x, y, by ="procedureDate"),
						 list(casesByTypeNMonth_1, casesByTypeNMonth_2, casesByTypeNMonth_3)
					)
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x")]  <- c("Bilatral ITA")
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.y")]  <- c("Radial Artery")
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count")]  <- c("Off Pump CABG ")
					
					jointTypeNCases$bilatralITA <- NULL
					jointTypeNCases$RadialArtery <- NULL
					jointTypeNCases$offpumpCABG <- NULL
										
					library(reshape2)
					jointTypeNCases_Mltd <- reshape2::melt(jointTypeNCases, id.var = c('procedureDate'), variable.name = 'GeneralType')
					
					colnames(jointTypeNCases_Mltd)[which(names(jointTypeNCases_Mltd) == "value")]  <- c("count")
					
					rm(casesByTypeNMonth_1)
					rm(casesByTypeNMonth_2)
					rm(casesByTypeNMonth_3)
					rm(jointTypeNCases)
			
					gc() 
					
				} else if ( "Overall_Activity" %in% input$checkSgryType ) {	

					#aggregate number of cases by type and date  
					casesByTypeNMonth_1 <- aggregate(count~procedureDate+AorticValveSurgery, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_1 <- casesByTypeNMonth_1[casesByTypeNMonth_1$AorticValveSurgery == "1", ]
					
					casesByTypeNMonth_2 <- aggregate(count~procedureDate+MitralValveSurgery, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_2 <- casesByTypeNMonth_2[casesByTypeNMonth_2$MitralValveSurgery == "1", ]

					casesByTypeNMonth_3 <- aggregate(count~procedureDate+Surgery_on_thoracic_aorta, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_3 <- casesByTypeNMonth_3[casesByTypeNMonth_3$Surgery_on_thoracic_aorta == "1", ]
					
					casesByTypeNMonth_4 <- aggregate(count~procedureDate+isolatedCABG, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_4 <- casesByTypeNMonth_4[casesByTypeNMonth_4$isolatedCABG == "1", ]
					
					jointTypeNCases <- Reduce((function() {counter = 1
														   function(x, y) {
														   counter <<- counter + 1
														   d = merge(x, y, by = 'procedureDate')
														   setnames(d, c(head(names(d), -1), paste0('count.x', counter)))
														  }})(), list(casesByTypeNMonth_1, casesByTypeNMonth_2, casesByTypeNMonth_3, casesByTypeNMonth_4))
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x")]  <- c("Aortic Valve Surgery")
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x2")]  <- c("Mitral Valve Surgery")
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x3")]  <- c("Surgery on Thoracic Aorta")
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x4")]  <- c("Isolated CABG")
					
					jointTypeNCases$AorticValveSurgery <- NULL
					jointTypeNCases$MitralValveSurgery <- NULL
					jointTypeNCases$Surgery_on_thoracic_aorta <- NULL
					jointTypeNCases$isolatedCABG <- NULL
					
					# up to here --------------------------------------------------
									
					
					library(reshape2)
					jointTypeNCases_Mltd <- reshape2::melt(jointTypeNCases, id.var = c('procedureDate'), variable.name = 'GeneralType')
					
					colnames(jointTypeNCases_Mltd)[which(names(jointTypeNCases_Mltd) == "value")]  <- c("count")
					
					rm(casesByTypeNMonth_1)
					rm(casesByTypeNMonth_2)
					rm(casesByTypeNMonth_3)
					rm(casesByTypeNMonth_4)
					rm(jointTypeNCases)
			
					gc() 
				}
						
			
			#sort jointTypeNCases_Mltd by date 
			jointTypeNCases_MltdSrtd <- jointTypeNCases_Mltd[order(jointTypeNCases_Mltd$procedureDate),]
			
			if (length(unique(jointTypeNCases_MltdSrtd$procedureDate)) != 1 ) {
			
					p <- ggplot(jointTypeNCases_MltdSrtd, aes(x=procedureDate, y=count, group = as.factor(GeneralType), colour = as.factor(GeneralType))) + theme_bw() + geom_line(na.rm=TRUE)  + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Number of cases") + ggtitle("Trend: number of cases over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
			
			} else {				
				
					p <- ggplot(jointTypeNCases_MltdSrtd, aes(x=procedureDate, y=count, group = as.factor(GeneralType), colour = as.factor(GeneralType))) + theme_bw() + geom_point(shape = 21, size = 1.5, na.rm=TRUE) + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Number of cases") + ggtitle("Trend: number of cases over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
			
			}
			
			#ggplotly(p) 
			
			fig <- ggplotly(p, autosize = F, width = 800, height = 600)
			
			fig <- plotly_build(fig)
			
			#fig$x$data[[1]]$name <- 'Group A'
			
			# m <- list(
						# l = 50,
						# r = 50,
						# b = 100,
						# t = 100,
						# pad = 4
					# )
			
			#fig$x$layout$margin <- list(b=10,r=7, t=10.5, l=4.5)
			
			for (i in seq_along(fig$x$data) ) {
			
				
				
				fig$x$data[[i]]$text <- sub("as.factor.*", "", fig$x$data[[i]]$text)
				
			}
			
			
			fig <- fig %>%
			  add_annotations( text="Subtype Id", xref="paper", yref="paper",
							  x=1.02, xanchor="left",
							  y=0.65, yanchor="bottom",    # Same y as legend below
							  legendtitle=TRUE, showarrow=FALSE )  %>%
			layout( legend=list(y=0.65, yanchor="top" ), xaxis=list(tickangle=270, title = "\n Date (year)", tickfont = list(size = '15')))
			
			
			# fig$height  = "100%"
			# fig$width = "100%"
					

			fig %>% config(displayModeBar = F)
			
			} else {
			#group subtype results 
			
			#casesByConsNMonth <- aggregate(count~month_year, transform(subsetDt, month_year = format(as.Date(dischargeDate), "%Y-%m")), sum, na.rm = TRUE)
			
			casesByTypeNMonth <- aggregate(count~procedureDate, subsetDt3Col, sum, na.rm = TRUE)
			
			#sort casesByHospNMonth by date 
			casesByTypeNMonthSrtd <- casesByTypeNMonth[order(casesByTypeNMonth$procedureDate),]
			
			casesByTypeNMonthSrtd$one <- 1
						
						
			if (length(unique(casesByTypeNMonthSrtd$procedureDate)) != 1 ) {
			
					p <- ggplot(casesByTypeNMonthSrtd, aes(x=procedureDate, y=count, group = 1, colour = as.factor(one))) + theme_bw() + geom_line(na.rm=TRUE, size=0.8)  + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Number of cases") + ggtitle("Trend: number of cases over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(labels = NULL, values=c("skyblue")) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
			
			} else {				
				
					p <- ggplot(casesByTypeNMonthSrtd, aes(x=procedureDate, y=count, group = 1, colour = as.factor(one))) + theme_bw() + geom_point(shape = 21, size = 1.5, na.rm=TRUE) + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Number of cases") + ggtitle("Trend: number of cases over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(labels = NULL, values=c("skyblue")) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
			
			}
			
			fig <- ggplotly(p, autosize = F, width = 800, height = 600)
			
			fig <- plotly_build(fig)
			
			fig$x$data[[1]]$name <- 'All Subtypes Selected'
			
			# m <- list(
						# l = 50,
						# r = 50,
						# b = 100,
						# t = 100,
						# pad = 4
					# )
			
			#fig$x$layout$margin <- list(b=10,r=7, t=10.5, l=4.5)
			
			for (i in seq_along(fig$x$data) ) {
			
				fig$x$data[[i]]$text <- sub("as.factor.*", "", fig$x$data[[i]]$text)
				
			}
			
			
			fig <- fig %>%
			  add_annotations( text="Key", xref="paper", yref="paper",
							  x=1.02, xanchor="left",
							  y=0.65, yanchor="bottom",    # Same y as legend below
							  legendtitle=TRUE, showarrow=FALSE )  %>%
			layout( legend=list(y=0.65, yanchor="top" ), xaxis=list(tickangle=270, title = "\n Date (year)", tickfont = list(size = '15')))
			
			
			# fig$height  = "100%"
			# fig$width = "100%"
					

			fig %>% config(displayModeBar = F)
			
			
			# rm(casesByTypeNMonth)
			
			# gc() 
			
			}
			
			
			} else {
				#no data 
				output$message <- renderText({
						"No results to display for selection"
				})
							
				output$summary <- NULL
					  
				output$summary2 <- NULL
			}
	
	})

	
  
  
  } else if (input$tabs1 == "Trend: outcome") { 
	
	output$map <<- NULL
	output$plot_access_web <<- NULL
	
	
	output$trendIntPlot <- renderPlotly({
			
		par(mar=c(10,4.5,7,7))
			
				subsetDt <- cardioData
				
				#filter by hospital 
				subsetDt <- subsetDt[subsetDt$hospital %in% input$check2, ]  #input$check2
						
				#filter by year 
				subsetDt <- subsetDt[subsetDt$year %in% input$checkYear, ]  #input$checkYear
				
				if (!is.null(input$checkPrty)) {
					#filter by priority 
					subsetDt <- subsetDt[subsetDt$X2.35.Operative.Urgency %in% input$checkPrty, ]  #input$checkPrty
				}
				
				#filter by type  
				if ( "death" %in% input$checkOutcomeType ) {
					
					subsetDt <- subsetDt[subsetDt$death == 1, ] 
					
					# if (input$checkSubType == "AVR_No_CABG") {
						# subsetDt <- subsetDt[subsetDt$AVR_No_CABG == "1", ]
						# subsetDt <- subsetDt[!is.na(subsetDt$AVR_No_CABG),]						
					# } else if (input$checkSubType == "AVR_CABG") {
						# subsetDt <- subsetDt[subsetDt$AVR_CABG == "1", ] 
						# subsetDt <- subsetDt[!is.na(subsetDt$AVR_CABG),]	
					# } else { #nothing selected, so display all aortic valve
						# #no actions required 			
					# }
					
				} else if ( "SWI" %in% input$checkOutcomeType ) {
					subsetDt <- subsetDt[subsetDt$SWI == 1, ] 
					
					# if (input$checkSubType == "repair") {
						# subsetDt <- subsetDt[subsetDt$MVprocedure == "repair", ] 
						# subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					# } else if (input$checkSubType == "replacement") {
						# subsetDt <- subsetDt[subsetDt$MVprocedure == "replacement", ] 
						# subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					# } else { #nothing selected, so display all aortic valve
						# #no actions required 			
					# }
					
				} else if ( "post_opCVA" %in% input$checkOutcomeType ) {
					subsetDt <- subsetDt[subsetDt$post_opCVA == 1, ] 
					
					# if (input$checkSubType == "chronic.aneurysm") {
						# subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "chronic.aneurysm", ] 
						# subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					# } else if (input$checkSubType == "acute.aortic.dissection") {
						# subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "acute.aortic.dissection", ] 
						# subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					# } else { #nothing selected, so display all aortic valve
						# #no actions required 			
					# }								
					
				} else if ( "postop_dialysis" %in% input$checkOutcomeType ) {
					subsetDt <- subsetDt[subsetDt$postop_dialysis == 1, ] 
					
					# if (input$checkSubType == "bilatralITA") {
						# subsetDt <- subsetDt[subsetDt$bilatralITA == "BilateralITA", ]  #is singleITA no required for selection?
						# subsetDt <- subsetDt[!is.na(subsetDt$bilatralITA),]
					# } else if (input$checkSubType == "RadialArtery") {
						# subsetDt <- subsetDt[subsetDt$RadialArtery == "RadialArtery", ] 
						# subsetDt <- subsetDt[!is.na(subsetDt$RadialArtery),]
					# } else if (input$checkSubType == "offpumpCABG") {
						# subsetDt <- subsetDt[subsetDt$offpumpCABG == "1", ] 
						# subsetDt <- subsetDt[!is.na(subsetDt$offpumpCABG),]
					
					# } else { #nothing selected, so display all aortic valve
						# #no actions required 			
					# }
					
				}
			
			if (!nrow(subsetDt) == 0) {
			
			
			output$message <- NULL
			
			output$summary <- NULL
				  
			output$summary2 <- NULL
			
			# subsetDt$X4.05.Patient.Status.at.discharge <- gsub('. Alive', '', subsetDt$X4.05.Patient.Status.at.discharge)
			# subsetDt$X4.05.Patient.Status.at.discharge <- gsub('. Dead', '', subsetDt$X4.05.Patient.Status.at.discharge)
			
			# subsetDt$X4.05.Patient.Status.at.discharge <- as.numeric(as.character(subsetDt$X4.05.Patient.Status.at.discharge)) # convert to float
			
			# subsetDt <- subsetDt[!is.na(subsetDt$X4.05.Patient.Status.at.discharge),]
			
			# #remove all % characters so mean can be performed
			# subsetDt$Logistic.Euroscore <- gsub('%', '', subsetDt$Logistic.Euroscore)
			# subsetDt$Logistic.Euroscore <- as.numeric(as.character(subsetDt$Logistic.Euroscore)) # convert to float
			
			# subsetDt$Logistic.Euroscore.Calib <- subsetDt$Logistic.Euroscore / as.numeric(input$selectCal)   #calibration to expected values (in percentage i.e. not divided by 100)
			

			subsetDt$procedureDate <- strptime(as.character(subsetDt$X3.02.Procedure.Date), "%d/%m/%Y")

			subsetDt$count <- 1
			
			#convert to POSIXct format required by ggplot 
			subsetDt$procedureDate <- as.POSIXct(as.character(subsetDt$procedureDate), format="%Y-%m-%d")
			
			
			
			#subsetDt3Col <- subsetDt[, c('count', 'procedureDate')]
			
			subsetDt3Col <- subsetDt
			
			subsetDt3Col$procedureDate <- format(as.Date(subsetDt3Col$procedureDate), "%Y")
				
			#aggregate number of cases and mtly by hospital 
			#casesByHospNMonth <- aggregate(.~procedureDate+hospital, subsetDt3Col, sum, na.rm = TRUE)
			
			
			if (flagOutcome$grp_Outcome) {
				

					#aggregate number of cases by type and date  
					casesByTypeNMonth_1 <- aggregate(count~procedureDate+death, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_1 <- casesByTypeNMonth_1[casesByTypeNMonth_1$death == "1", ]
					
					casesByTypeNMonth_2 <- aggregate(count~procedureDate+SWI, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_2 <- casesByTypeNMonth_2[casesByTypeNMonth_2$SWI == "1", ]

					casesByTypeNMonth_3 <- aggregate(count~procedureDate+post_opCVA, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_3 <- casesByTypeNMonth_3[casesByTypeNMonth_3$post_opCVA == "1", ]
					
					casesByTypeNMonth_4 <- aggregate(count~procedureDate+postop_dialysis, subsetDt3Col, sum, na.rm = TRUE)
					
					casesByTypeNMonth_4 <- casesByTypeNMonth_4[casesByTypeNMonth_4$postop_dialysis == "1", ]
					
					jointTypeNCases <- Reduce((function() {counter = 1
														   function(x, y) {
														   counter <<- counter + 1
														   d = merge(x, y, by = 'procedureDate')
														   setnames(d, c(head(names(d), -1), paste0('count.x', counter)))
														  }})(), list(casesByTypeNMonth_1, casesByTypeNMonth_2, casesByTypeNMonth_3, casesByTypeNMonth_4))
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x")]  <- c("Mortality")
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x2")]  <- c("S.W.I")
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x3")]  <- c("Postop CVA")
					
					colnames(jointTypeNCases)[which(names(jointTypeNCases) == "count.x4")]  <- c("Postop Dialysis")
					
					jointTypeNCases$death <- NULL
					jointTypeNCases$SWI <- NULL
					jointTypeNCases$post_opCVA <- NULL
					jointTypeNCases$postop_dialysis <- NULL
					
					# up to here --------------------------------------------------
									
					
					library(reshape2)
					jointTypeNCases_Mltd <- reshape2::melt(jointTypeNCases, id.var = c('procedureDate'), variable.name = 'GeneralType')
					
					colnames(jointTypeNCases_Mltd)[which(names(jointTypeNCases_Mltd) == "value")]  <- c("count")
					
					rm(casesByTypeNMonth_1)
					rm(casesByTypeNMonth_2)
					rm(casesByTypeNMonth_3)
					rm(casesByTypeNMonth_4)
					rm(jointTypeNCases)
			
					gc() 
				#}
			
			
			
			#sort jointTypeNCases_Mltd by date 
			jointTypeNCases_MltdSrtd <- jointTypeNCases_Mltd[order(jointTypeNCases_Mltd$procedureDate),]
			
			if (length(unique(jointTypeNCases_MltdSrtd$procedureDate)) != 1 ) {
			
					p <- ggplot(jointTypeNCases_MltdSrtd, aes(x=procedureDate, y=count, group = as.factor(GeneralType), colour = as.factor(GeneralType))) + theme_bw() + geom_line(na.rm=TRUE)  + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Number of cases") + ggtitle("Trend: number of cases over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
			
			} else {				
				
					p <- ggplot(jointTypeNCases_MltdSrtd, aes(x=procedureDate, y=count, group = as.factor(GeneralType), colour = as.factor(GeneralType))) + theme_bw() + geom_point(shape = 21, size = 1.5, na.rm=TRUE) + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Number of cases") + ggtitle("Trend: number of cases over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
			
			}
			
			#ggplotly(p) 
			
			fig <- ggplotly(p, autosize = F, width = 800, height = 600)
			
			fig <- plotly_build(fig)
			
			#fig$x$data[[1]]$name <- 'Group A'
			
			# m <- list(
						# l = 50,
						# r = 50,
						# b = 100,
						# t = 100,
						# pad = 4
					# )
			
			#fig$x$layout$margin <- list(b=10,r=7, t=10.5, l=4.5)
			
			for (i in seq_along(fig$x$data) ) {
			
				
				
				fig$x$data[[i]]$text <- sub("as.factor.*", "", fig$x$data[[i]]$text)
				
			}
			
			
			fig <- fig %>%
			  add_annotations( text="Subtype Id", xref="paper", yref="paper",
							  x=1.02, xanchor="left",
							  y=0.65, yanchor="bottom",    # Same y as legend below
							  legendtitle=TRUE, showarrow=FALSE )  %>%
			layout( legend=list(y=0.65, yanchor="top" ), xaxis=list(tickangle=270, title = "\n Date (year)", tickfont = list(size = '15')))
			
			
			# fig$height  = "100%"
			# fig$width = "100%"
					

			fig %>% config(displayModeBar = F)
			
			} else {
			#display total number of cases 
			
			casesByTypeNMonth <- aggregate(count~procedureDate, subsetDt3Col, sum, na.rm = TRUE)
			
			#sort casesByHospNMonth by date 
			casesByTypeNMonthSrtd <- casesByTypeNMonth[order(casesByTypeNMonth$procedureDate),]
			
			casesByTypeNMonthSrtd$one <- 1
						
						
			if (length(unique(casesByTypeNMonthSrtd$procedureDate)) != 1 ) {
			
					p <- ggplot(casesByTypeNMonthSrtd, aes(x=procedureDate, y=count, group = 1, colour = as.factor(one))) + theme_bw() + geom_line(na.rm=TRUE, size=0.8)  + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Number of cases") + ggtitle("Trend: number of cases over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(labels = NULL, values=c("skyblue")) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
			
			} else {				
				
					p <- ggplot(casesByTypeNMonthSrtd, aes(x=procedureDate, y=count, group = 1, colour = as.factor(one))) + theme_bw() + geom_point(shape = 21, size = 1.5, na.rm=TRUE) + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Number of cases") + ggtitle("Trend: number of cases over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(labels = NULL, values=c("skyblue")) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
			
			}
			
			fig <- ggplotly(p, autosize = F, width = 800, height = 600)
			
			fig <- plotly_build(fig)
			
			fig$x$data[[1]]$name <- 'All Subtypes Selected'
			
			# m <- list(
						# l = 50,
						# r = 50,
						# b = 100,
						# t = 100,
						# pad = 4
					# )
			
			#fig$x$layout$margin <- list(b=10,r=7, t=10.5, l=4.5)
			
			for (i in seq_along(fig$x$data) ) {
			
				fig$x$data[[i]]$text <- sub("as.factor.*", "", fig$x$data[[i]]$text)
				
			}
			
			
			fig <- fig %>%
			  add_annotations( text="Key", xref="paper", yref="paper",
							  x=1.02, xanchor="left",
							  y=0.65, yanchor="bottom",    # Same y as legend below
							  legendtitle=TRUE, showarrow=FALSE )  %>%
			layout( legend=list(y=0.65, yanchor="top" ), xaxis=list(tickangle=270, title = "\n Date (year)", tickfont = list(size = '15')))
			
			
			# fig$height  = "100%"
			# fig$width = "100%"
					

			fig %>% config(displayModeBar = F)
			
			
			# rm(casesByTypeNMonth)
			
			# gc() 
			
			}
			
			
			} else {
				#no data 
				output$message <- renderText({
						"No results to display for selection"
				})
							
				output$summary <- NULL
					  
				output$summary2 <- NULL
			}
	
	})
    
  } else if (input$tabs1 == "Trend: expected and observed mortality") {
   
	output$plot_access_web <- renderPlot({
	  	  
		output$map <- NULL
		
		par(mar=c(10,4.5,7,7))
			
				subsetDt <- cardioData
				
				#filter by hospital 
				subsetDt <- subsetDt[subsetDt$hospital %in% input$check2, ]  #input$check2
						
				#filter by year 
				subsetDt <- subsetDt[subsetDt$year %in% input$checkYear, ]  #input$checkYear
				
				if (!is.null(input$checkPrty)) {
					#filter by priority 
					subsetDt <- subsetDt[subsetDt$X2.35.Operative.Urgency %in% input$checkPrty, ]  #input$checkPrty
				}
				
				#filter by aortic valve  
				if ( "AorticValveSurgery" %in% input$checkSgryType ) {
					
					subsetDt <- subsetDt[subsetDt$AorticValveSurgery == 1, ] 
					
					if (input$checkSubType == "repair") {
						subsetDt <- subsetDt[subsetDt$AVprocedure == "repair", ]
						subsetDt <- subsetDt[!is.na(subsetDt$AVprocedure),]						
					} else if (input$checkSubType == "replacement") {
						subsetDt <- subsetDt[subsetDt$AVprocedure == "replacement", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$AVprocedure),]	
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				} else if ( "MitralValveSurgery" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$MitralValveSurgery == 1, ] 
					
					if (input$checkSubType == "repair") {
						subsetDt <- subsetDt[subsetDt$MVprocedure == "repair", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					} else if (input$checkSubType == "replacement") {
						subsetDt <- subsetDt[subsetDt$MVprocedure == "replacement", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$MVprocedure),]
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				} else if ( "Surgery_on_thoracic_aorta" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$Surgery_on_thoracic_aorta == 1, ] 
					
					if (input$checkSubType == "chronic.aneurysm") {
						subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "chronic.aneurysm", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					} else if (input$checkSubType == "acute.aortic.dissection") {
						subsetDt <- subsetDt[subsetDt$aortic_valscular_indication3 == "acute.aortic.dissection", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$aortic_valscular_indication3),]
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}								
					
				} else if ( "CABG" %in% input$checkSgryType ) {
					subsetDt <- subsetDt[subsetDt$CABG == 1, ] 
					
					if (input$checkSubType == "bilatralITA") {
						subsetDt <- subsetDt[subsetDt$bilatralITA == "BilateralITA", ]  #is singleITA no required for selection?
						subsetDt <- subsetDt[!is.na(subsetDt$bilatralITA),]
					} else if (input$checkSubType == "RadialArtery") {
						subsetDt <- subsetDt[subsetDt$RadialArtery == "RadialArtery", ] 
						subsetDt <- subsetDt[!is.na(subsetDt$RadialArtery),]
					} else { #nothing selected, so display all aortic valve
						#no actions required 			
					}
					
				}
				
				
				if (!nrow(subsetDt) == 0) {
			
				output$message <- NULL
				
				output$summary <- NULL
					  
				output$summary2 <- NULL
				
				
				subsetDt$X4.05.Status.at.Discharge <- gsub('. Alive', '', subsetDt$X4.05.Status.at.Discharge)
				subsetDt$X4.05.Status.at.Discharge <- gsub('. Dead', '', subsetDt$X4.05.Status.at.Discharge)
				
				 
				
				subsetDt$X4.05.Status.at.Discharge <- as.numeric(as.character(subsetDt$X4.05.Status.at.Discharge)) # convert to float
				
				subsetDt <- subsetDt[!is.na(subsetDt$X4.05.Status.at.Discharge),]
				
				#remove all % characters so mean can be performed
				#subsetDt$Logistic.Euroscore <- gsub('%', '', subsetDt$LogEusoSCORE)
				subsetDt$LogEusoSCORE <- as.numeric(as.character(subsetDt$LogEuroSCORE)) # convert to float
				
				subsetDt$LogEusoSCORE <- subsetDt$LogEusoSCORE * 100  #convert to %
				
				subsetDt$Logistic.Euroscore.Calib <- subsetDt$LogEusoSCORE / as.numeric(input$selectCal)   #calibration to expected values (in percentage i.e. not divided by 100)
				

				subsetDt$procedureDate <- strptime(as.character(subsetDt$X3.02.Procedure.Date), "%d/%m/%Y")
							
				subsetDt$count <- 1
				
				#convert to POSIXct format required by ggplot 
				subsetDt$procedureDate <- as.POSIXct(as.character(subsetDt$procedureDate), format="%Y-%m-%d")
				
				if (flag$grpHsp) {
				
				subsetDt3Col <- subsetDt[, c('count', 'procedureDate', 'X4.05.Status.at.Discharge', 'hospital', 'Logistic.Euroscore.Calib')]
				
				subsetDt3Col$procedureDate <- format(as.Date(subsetDt3Col$procedureDate), "%Y")
				
				#aggregate number of cases and mtly by hospital 
				casesByHospNMonth <- aggregate(.~procedureDate+hospital, subsetDt3Col, sum, na.rm = TRUE)
				
				casesByHospNMonth$mtlyPercntg <- casesByHospNMonth$X4.05.Status.at.Discharge / casesByHospNMonth$count * 100
		
									
				#use summed calibrated euroscore during each time period (month), divided by number of cases during that period -> mean calibrated Eurscore
				casesByHospNMonth$mean.Euroscore.Calib <- casesByHospNMonth$Logistic.Euroscore.Calib / casesByHospNMonth$count
		
				#sort casesByHospNMonth by date 
				casesByHospNMonthSrtd <- casesByHospNMonth[order(casesByHospNMonth$procedureDate),]
				
				casesByHospNMonthSrtd$one <- 1
				casesByHospNMonthSrtd$two <- 1

				# ggplot(casesByHospNMonthSrtd, aes(procedureDate, group = as.factor(hospital))) + theme_bw() + geom_line(size = 0.5, na.rm=TRUE, aes(y = mtlyPercntg, linetype = "Observed Mortality (%)", colour = as.factor(hospital))) + geom_line(na.rm=TRUE, aes(y = mean.Euroscore.Calib, linetype = "Expected Mortality (%)", colour = as.factor(hospital))) + scale_linetype_manual(values = c('solid', 'dashed'))+ labs(color='Hospital Id') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year month)") + ylab("Percentage (%)") + ggtitle("Trend: expected and observed mortality over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
				
				if (length(unique(casesByHospNMonthSrtd$procedureDate)) != 1 ) {
				
					ggplot(casesByHospNMonthSrtd, aes(procedureDate, group = as.factor(hospital))) + theme_bw() + geom_line(size = 0.5, na.rm=TRUE, aes(y = mtlyPercntg, linetype = "Observed Mortality (%)", colour = as.factor(hospital))) + geom_line(na.rm=TRUE, aes(y = mean.Euroscore.Calib, linetype = "Expected Mortality (%)", colour = as.factor(hospital))) + scale_linetype_manual(values = c('solid', 'dashed')) + labs(color='Hospital Id') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Percentage (%)") + ggtitle("Trend: expected and observed mortality over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2) + guides(color = guide_legend(override.aes = list(size = 1.5))) 
				
				} else {				
				
					p <- ggplot(casesByHospNMonthSrtd, aes(procedureDate, group = as.factor(hospital))) + theme_bw() + geom_point(shape = 21, size = 1.5, na.rm=TRUE, aes(y = mtlyPercntg, colour = as.factor(hospital))) + geom_point(shape = 24, size = 1.5, na.rm=TRUE, aes(y = mean.Euroscore.Calib, colour = as.factor(hospital))) + labs(color='Hospital Id') + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Percentage (%)") + ggtitle("Trend: expected and observed mortality over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(values = colorSet2, guide = guide_legend(override.aes = list(shape = c(rep(1, length(casesByHospNMonthSrtd$hospital))), size = 1.5) )) 


					p2 <- p + geom_point(aes(y = mtlyPercntg, shape = NA, size = "Observed Mortality (%)")) + geom_point(aes(y = mean.Euroscore.Calib, shape = NA, size = "Expected Mortality (%)")) 
									
					p2 + guides(size=guide_legend("Key definition", override.aes=list(shape=c(24,21), size = 1.5)))
				
				}
						
						
				} else {
				
				subsetDt3Col <- subsetDt[, c('count', 'procedureDate', 'X4.05.Status.at.Discharge', 'Logistic.Euroscore.Calib')]
				
				subsetDt3Col$procedureDate <- format(as.Date(subsetDt3Col$procedureDate), "%Y")
				
				#aggregate number of cases and mtly by all hospitals
				casesByHospNMonth <- aggregate(.~procedureDate, subsetDt3Col, sum, na.rm = TRUE)
				
				casesByHospNMonth$mtlyPercntg <- casesByHospNMonth$X4.05.Status.at.Discharge / casesByHospNMonth$count * 100
				
				#use summed calibrated euroscore during each time period (month), divided by number of cases during that period -> mean calibrated Eurscore
				casesByHospNMonth$mean.Euroscore.Calib <- casesByHospNMonth$Logistic.Euroscore.Calib / casesByHospNMonth$count
				 
				#sort casesByConsNMonth by date 
				casesByHospNMonthSrtd <- casesByHospNMonth[order(casesByHospNMonth$procedureDate),]
				
				casesByHospNMonthSrtd$one <- 1
				
				if (length(unique(casesByHospNMonthSrtd$procedureDate)) != 1 ) {
				
					ggplot(casesByHospNMonthSrtd, aes(procedureDate)) + theme_bw() + geom_line(na.rm=TRUE, aes(y = mtlyPercntg,  linetype = "Observed Mortality (%)", group = 1, colour = as.factor(one)), size = 0.6) + geom_line(na.rm=TRUE, aes(y = mean.Euroscore.Calib, linetype = "Expected Mortality (%)", group = 1, colour = as.factor(one))) + scale_linetype_manual(values = c('solid', 'dashed')) + labs(color=NULL) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Percentage (%)") + ggtitle("Trend: expected and observed mortality over time period") + theme(plot.title = element_text(hjust = 0.5)) + scale_colour_manual(labels = c("All Selected Hospitals"), values = "darkorange") + guides(color = guide_legend(override.aes = list(size = 1.5)), linetype = guide_legend(override.aes = list(size = 0.5)) ) 
				
				} else { 
					
					p <- ggplot(casesByHospNMonthSrtd, aes(procedureDate)) + theme_bw() + geom_point(shape = 21, size = 2.5, fill= "#8856a7", na.rm=TRUE, aes(y = mtlyPercntg, group = 1, colour = as.factor(mtlyPercntg))) + geom_point(shape = 24, size = 2.5, fill= "#2ca25f", na.rm=TRUE, aes(y = mean.Euroscore.Calib, group = 1, colour = as.factor(mean.Euroscore.Calib))) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, margin=margin(5,0,7,0),face="bold")) + xlab("Date (year)") + ylab("Percentage (%)") + ggtitle("Trend: expected and observed mortality over time period") + theme(plot.title = element_text(hjust = 0.5)) +  scale_colour_manual(guide=FALSE, name="All Selected Hospitals", labels = c('Observed Mortality (%)', 'Expected Mortality (%)'), values = c('#8856a7', '#2ca25f')) 
					
					p2 <- p + geom_point(aes(y = mtlyPercntg, shape = NA, size = "Observed Mortality (%)")) + geom_point(aes(y = mean.Euroscore.Calib, shape = NA, size = "Expected Mortality (%)")) 
									
					p2 + guides(size=guide_legend("All Selected Hospitals", override.aes=list(shape=c(24,21), size = 1.5)))
				
				
				}
				
				}
				
				} else {
					#no data 
					output$message <- renderText({
							"No results to display for selection"
					})
					
					output$summary <- NULL
						  
					output$summary2 <- NULL
					
				} 

			
	  


  })
  
  }
  
  }) #observe input processId
	  
}



# Run app ----
shinyApp(ui, server)