library(faosws)
library(faoswsUtil)
library(faoswsBalancing)
library(faoswsStandardization)
library(dplyr)
library(data.table)
library(tidyr)
library(openxlsx)
library(readxl)


#if the country was not a reporter for any year use the HS6 mapping (below).


basedir <-getwd()

start_time <- Sys.time()

if (CheckDebug()) {
  mydir <- "SUA-FBS Balancing/"
  
  SETTINGS <- faoswsModules::ReadSettings(file.path(mydir, "sws.yml"))
  
  SetClientFiles(SETTINGS[["certdir"]])
  
  GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = SETTINGS[["token"]])
  R_SWS_SHARE_PATH <- "//hqlprsws1.hq.un.fao.org/sws_r_share"
}

country_m49 <- as.character("426") # Lesotho

tradeMap  <- ReadDatatable("ess_trademap_2022", where = "area = '426' ")  # Lesotho

tradeMap <- tradeMap[,c("area","flow","hs","cpc"),with = F]

write.xlsx(tradeMap,"Data/tradeMap_2019.xlsx",row.names = F)
# Sys.setenv("R_ZIPCMD" = "C:/Rtools/bin/zip.exe") #if rtools error occurs







# For a never non-reported country (example Gabon, Mozambique). Use the standard mapping stated below :  



standard_data <- read_excel("Data/Reference File.xlsx", sheet = "Trade_Commodities")

setDT(standard_data)

s <- strsplit(standard_data$HS6, split = ",")
standard_data <- data.frame(CPCCode = rep(standard_data$CPCCode, sapply(s, length)),HS6 = unlist(s))

setDT(standard_data)

standard_data <- standard_data[!is.na(HS6)]

standard_data[, area := as.character("508")]


flow_1 <- copy(standard_data)
flow_1 <- flow_1[, flow := 1]

flow_2 <- copy(standard_data)
flow_2 <- flow_2[, flow := 2]


standard_data <- rbind(flow_1,flow_2)


setnames(standard_data, c("CPCCode","HS6"),c("cpc","hs"))


write.xlsx(standard_data,"Data/tradeMap_2019.xlsx",row.names = F)


