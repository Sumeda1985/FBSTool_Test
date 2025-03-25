#Source functions

css <- HTML(
  "table.dataTable tr.selected td.yellow {
  background-color: yellow !important
  }
  td.yellow {
  background-color: yellow !important
  }"
)

colorizeCell <- function(i, j,id){
  sprintf("colorizeCell(%d, %d, %s)", i, j, id)
}

#create connection for SQL database
con <- dbConnect(SQLite(), "Data\\Permanent.db")

sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source)
tourist_activate =TRUE

colorizeCell <- function(i, j,id){
  sprintf("colorizeCell(%d, %d, %s)", i, j, id)
}

## Require packages or install if missing
packageRequirevInstall()

# all cpc codes 
all_cpc <- data.table(read_excel("Data/cpc2.1.xlsx"))
all_cpc[,c("CONVERSION FACTORS\r\n(FCL-CPC)", "notes" ) :=NULL]
setnames(all_cpc, c("SWS CODE","SWS DESCRIPTOR"), c("CPCCode","Commodity"))
all_cpc[CPCCode == "21439.040000000001",CPCCode := "21439.04"]
add_cpc <- data.table(CPCCode = c("21439.9","01199") , Commodity = c("Juice of fruits n.e.", "Other cereals") )
all_cpc <- rbind(all_cpc,add_cpc)
all_cpc <- all_cpc[!duplicated(all_cpc[,"CPCCode"])]
all_cpc <- all_cpc[CPCCode == "39141", CPCCode := "39140.02"]
all_cpc_codes <- unique(all_cpc$CPCCode)

# reference 
all_elements <- data.table(read_excel("Data/Reference File.xlsx",sheet= "Elements"))
all_elements_to_merge <- copy(all_elements)
all_elements <- subset(all_elements, ElementCode %in% c("5510","5610","5910","5071","5520","5016","5525","5141","5318","5319","5320","5314","5327","5313","5321",
                    "5165","5113","5166","5312","5025","5023", "665","664","674","684","1001","1003","1005","261","271","281"))
all_element_codes <- unique(all_elements$ElementCode)

# remove existing codes from all codes 
allCodes <- data.table(expand.grid(CPCCode = all_cpc_codes, ElementCode = all_element_codes))
######################################################################################################################################################################
#production domain 
classification <- data.table(read_excel("Data/production_list_cpc.xlsx"))
classification <- classification[CPCCode == "39141", CPCCode := "39140.02"]
setnames(classification, "Commodity", "Commodity_name")
classification <- merge(classification,all_cpc , by = "CPCCode" , all.x = TRUE)
classification[is.na(Commodity), Commodity := Commodity_name]
classification <- classification[!is.na(Commodity)]
classification[, Commodity_name := NULL]

set_hot_colwidths <- function(data) {
  ncols <- ncol(data)
  widths <- rep(100, ncols + 1)
  widths[grep("^CPCCode", colnames(data))] <- 0.1
  widths[grep("Commodity", colnames(data))] <- 0.1
  widths[grep("^Flag", colnames(data))] <- 0.1
  return(widths)
}

### countrym49 selection
countries <- data.table(read_excel("Data/Reference File.xlsx", sheet = "Country"))
country_selc <- paste0(countries$CountryM49 , "|" , countries$Country)
