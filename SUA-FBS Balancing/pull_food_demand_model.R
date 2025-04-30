rm(list = ls())
sapply(list.files(pattern="[.]R$", path="SUA-FBS Balancing/R", full.names=TRUE), source)
library(devtools)
## load the library
# install_github(repo = "SWS-Methodology/faoswsModules")
library(faoswsModules)
# install_github(repo = "SWS-Methodology/faoswsFlag")
## load the library
library(faosws)
library(faoswsUtil)
library(faoswsBalancing)
library(faoswsStandardization)
library(faoswsFlag)

message("libraries loaded")
library(devtools)
library(data.table)
library(igraph)
library(stringr)
library(dplyr)
# library(dtplyr)
library(MASS) 
library(lattice)
library(reshape2)
library(sendmailR)
library(readxl)

#To faoswsUtil 0.4.2,
# install_git("https://ROSA@sdlc.fao.org/bitbucket/scm/sws/faoswsutil.git")
R_SWS_SHARE_PATH = Sys.getenv("R_SWS_SHARE_PATH")
if (CheckDebug()) {
    library(faoswsModules)
    message("Not on server, so setting up environment...")
    
    # Read settings file sws.yml in working directory. See 
    # sws.yml.example for more information
    PARAMS <- ReadSettings("SUA-FBS Balancing/sws.yml")
    message("Connecting to server: ", PARAMS[["current"]])
    
    R_SWS_SHARE_PATH = PARAMS[["share"]]
    apiDirectory = "./R"
    
    ## Get SWS Parameters
    SetClientFiles(dir = PARAMS[["certdir"]])
    GetTestEnvironment(
        baseUrl = PARAMS[["server"]],
        token = PARAMS[["token"]]
    )
    
    
    batchnumber = 204 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   
    
} else {
    batchnumber = 204 # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! SET IT   
    message("Running on server, no need to call GetTestEnvironment...")
    
}
SWS_USER = regmatches(swsContext.username, 
                      regexpr("(?<=/).+$", swsContext.username, perl = TRUE))
if(CheckDebug()){
    SUB_FOLDER = paste0(PARAMS[["subShare"]],batchnumber) 
}
message("Getting parameters/datasets...")
areaKeys <- as.character(swsContext.datasets[[1]]@dimensions$geographicAreaM49@keys)

#merge with names 
elementName=read_excel("Data/Reference File.xlsx", "Elements")
setDT(elementName)

# commodityName=read_excel("Data/Reference File.xlsx", "SUA_Commodities")
# setDT(commodityName)
commodityName <- data.table(read_excel("Data/cpc2.1.xlsx"))
commodityName[,c("CONVERSION FACTORS\r\n(FCL-CPC)", "notes" ) :=NULL]
setnames(commodityName, c("SWS CODE","SWS DESCRIPTOR"), c("CPCCode","Commodity"))
commodityName[CPCCode == "21439.040000000001",CPCCode := "21439.04"]
commodityName <- commodityName[!duplicated(commodityName[,"CPCCode"])]
countryName =read_excel("Data/Reference File.xlsx", "Country")
setDT(countryName)

#########################################################################################################

foodData <- get(load(file="Data/countrySUA.RData"))
foodData <- foodData[ElementCode == 5141]
foodData[, .N, ElementCode]
comCodes <- GetCodeList("food", "food_factors","foodCommodityM")$code
fdmCodes <- GetCodeList("food", "food_factors","foodFdm")$code
funCodes <- GetCodeList("food", "food_factors","foodFunction")$code
varCodes <- "y_e" ## Only need elasticities from the food domain table
dimM49 <- Dimension(name = "geographicAreaM49", keys = as.character(areaKeys))
dimCom <- Dimension(name = "foodCommodityM", keys = comCodes)
dimFdm <- Dimension(name = "foodFdm", keys = fdmCodes)
dimFun <- Dimension(name = "foodFunction", keys = funCodes)
dimVar <- Dimension(name = "foodVariable", keys = varCodes)
keyFdm <- DatasetKey(domain = "food", dataset = "food_factors",
                     dimensions = list(dimM49, dimCom, dimFdm, dimFun, dimVar))
fdmData <- GetData(keyFdm, flags = FALSE, normalized = FALSE)
stopifnot(nrow(fdmData) > 0)
setnames(fdmData, old = c("Value_foodVariable_y_e", "foodFdm", "foodCommodityM"),
         new = c("elasticity", "foodDemand", "foodCommodity"))
# Read map table from old code to new code
oldToNewCommodity <- ReadDatatable("food_old_code_map")
stopifnot(nrow(oldToNewCommodity) > 0)
# Food Commodity
funcCodes <- commodity2FunctionalForm(
    as.numeric(cpc2fcl(foodData$CPCCode, returnFirst = TRUE)))
foodData <- cbind(foodData, do.call("cbind", funcCodes))

# setkeyv(timeSeriesData, c("geographicAreaM49", "timePointYears"))
## Country income group
countryIncomeGroup <- ReadDatatable("country_income_group")
stopifnot(nrow(countryIncomeGroup) > 0)
setnames(countryIncomeGroup,
         old = c("geographic_area_m49", "country_name", "country_code", "group_code", "income_group"),
         new = c("geographicAreaM49", "CountryName", "CountryCode", "GroupCode", "incomeGroup"))
countryIncomeGroup <- countryIncomeGroup[geographicAreaM49 %in% areaKeys]

# Elasticity
fdmData <-
    merge(
        fdmData,
        oldToNewCommodity,
        all.x = TRUE,
        allow.cartesian = TRUE,
        by.x = "foodCommodity",
        by.y = "old_code"
    )
fdmData[is.na(new_code), new_code := foodCommodity]
fdmData <- fdmData[foodCommodity != "2500"]
fdmData[, c("foodCommodity") := NULL]
setnames(fdmData, old = "new_code", new = "foodCommodity")
fdmData <-
    fdmData[,
            .(elasticity = max(elasticity)),
            by = list(geographicAreaM49, foodDemand, foodFunction)
    ]
setnames(fdmData,c("geographicAreaM49"),c("CountryM49"))

cat("Merge in food demand model data...\n")
foodData <- merge(foodData, fdmData,
              by = c("foodDemand", "CountryM49"),
              all.x = TRUE)


## We need to fill the gaps of the elasticity for the combination country/commodity

key <- "CountryM49"
setnames(countryIncomeGroup,c("geographicAreaM49"),c("CountryM49"))
foodData <- merge(foodData, countryIncomeGroup[, c("CountryM49", "incomeGroup"), with = FALSE],
              by = key, all.x = TRUE)

## Take the elasticity average for each combination commodity/income group
foodData[, foodFunctionAux := as.numeric(foodFunction)]
elastAverage <-
    foodData[,
         list(
             elasticityAverage = mean(elasticity, na.rm = TRUE),
             foodFunctionAux = round(mean(foodFunctionAux, na.rm = TRUE))
         ),
         by = list(CPCCode, incomeGroup)
    ][
        !is.na(elasticityAverage)
    ][,
      foodFunctionAux := as.character(foodFunctionAux)
    ]

foodData[, "foodFunctionAux" := NULL]
## Merge elastAverage with data
keys <- c("CPCCode", "incomeGroup")
foodData <- merge(foodData, elastAverage, by = keys, all.x = TRUE)

## If elasticity is NA, we take the figure from elasticityAverage

foodData[, updatedElast := ifelse(is.na(elasticity), elasticityAverage, elasticity)]
foodData[, updatedFoodFunction := ifelse(is.na(foodFunction), foodFunctionAux, foodFunction)]

## Analysing elasticity

tabSD <-
  foodData[
    Year == 2012,
    list(
      minUpdatedElast = min(updatedElast, na.rm = TRUE),
      averageUpdatedElast = mean(updatedElast, na.rm = TRUE),
      sdUpdatedElast = sd(updatedElast, na.rm = TRUE)
    ),
    by = list(incomeGroup, CPCCode)
  ]

tabSD[, lowerTreshold := averageUpdatedElast - 2 * sdUpdatedElast]
tabSD[, upperTreshold := averageUpdatedElast + 2 * sdUpdatedElast]
tabSD[is.na(lowerTreshold), lowerTreshold := averageUpdatedElast]
tabSD[is.na(upperTreshold), upperTreshold := averageUpdatedElast]
foodData <- merge(foodData, tabSD[, c("incomeGroup", "CPCCode", "lowerTreshold",
                              "upperTreshold", "averageUpdatedElast"), with = FALSE],
              by = c("incomeGroup", "CPCCode"))
# If the condition is TRUE, it's an outlier
foodData[,
     newElasticity :=
       ifelse(
         updatedElast > upperTreshold | updatedElast < lowerTreshold,
         averageUpdatedElast,
         updatedElast
       )
]
foodData <- foodData[,c("CPCCode","foodCommodity", "foodDemand", "updatedFoodFunction","newElasticity"),with = F]
foodData <- unique(foodData)
foodData <- foodData[!is.na(updatedFoodFunction)]
foodData <- merge(foodData,commodityName, by= "CPCCode",all.x = TRUE)
setnames(foodData,c("foodCommodity","foodDemand","updatedFoodFunction","newElasticity"),c("FBSCode","Food Demand"
                      ,"Food Function","Elasticity"))
fbsName=read_excel("Data/Reference File.xlsx", "FBS_Commodities")
setDT(fbsName)
setnames(fbsName, "Commodity","fbsCommodity")
foodData=merge(foodData,fbsName, by="FBSCode",all.x = TRUE)
setnames(foodData, "fbsCommodity","FBSCommodity")
foodData[, Description := ifelse(`Food Function` == 0, "linear", 
                                ifelse(`Food Function`==1,"log-log",
                                       ifelse(`Food Function` == 2, "semi-log",
                                              ifelse(`Food Function`==3, "log-inverse", NA))))]
setcolorder(foodData, c("CPCCode","Commodity","FBSCode","FBSCommodity","Food Demand","Food Function","Description","Elasticity"))
foodData=foodData[order(CPCCode)]
foodData=foodData[!is.na(Elasticity)]
foodData <- foodData[!duplicated(foodData[,c("CPCCode")])]
write.csv(foodData,"Data/fdmData.csv",row.names = FALSE)

###Food classification Table
food_classification=ReadDatatable("food_classification_country_specific")
food_classification=subset(food_classification,geographic_area_m49 %in% areaKeys )
food_classification[,geographic_area_m49 := NULL]
setnames(food_classification,c("measured_item_cpc","food_classification"),c("CPCCode","Type"))
food_classification=merge(food_classification,commodityName, by="CPCCode", all.x = TRUE)
setcolorder(food_classification,c("CPCCode","Commodity","Type"))
#food_classification[, Type := "Food Residual"]
write.csv(food_classification, "Data/foodCommodityList.csv", row.names = FALSE)







