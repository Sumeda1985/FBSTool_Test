

library(readxl)

M49_code <- as.character("400")

country_data <- data.table(read_excel("SUA-FBS Balancing/Trade/Data/trad2014.xlsx"))

country_data <- country_data[,c("SUCODE","EXPORT_QUNN","IMPORT_QNN","Year")]



country_data <- country_data[, lapply(.SD,sum), by=list(Year,SUCODE)]


countrycode <- fread("SUA-FBS Balancing/Trade/Data/country_codes.csv")


country_data[, reporter := countrycode[`M49 Code` == M49_code]$`Country Code`]

setnames(country_data,"SUCODE","hs")


country_data= melt.data.table(country_data, id.vars = c("Year", "hs","reporter"), measure.vars = c("IMPORT_QNN","EXPORT_QUNN"), value = TRUE,
                               value.name= "QTY")


setnames(country_data,"variable","flow")


country_data[flow == "IMPORT_QNN", flow1 := 1L]
country_data[flow == "EXPORT_QUNN", flow1 := 2L]

country_data[, flow := flow1]

country_data[,  flow1 := NULL]


country_data[,hs6 := str_sub(hs,1L,6)]

country_data[,HSLength := nchar(hs)]


write.csv(country_data,"SUA-FBS Balancing/Trade/Data/country_trade.csv",row.names = F)





