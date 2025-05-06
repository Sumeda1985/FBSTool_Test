# Required libraries
## In the first argument of apppath, provide the location of the shiny tool
#apppath<-file.path("~/fao2025","FBSTool_Test")
apppath<-getwd()

userauth <- readRDS(file.path(apppath, "userauth.rds"))[status==1]
pubkey <- readRDS(file.path(apppath,"publickey.rds"))

# Initialize reactive values
rv <- reactiveValues()
#In case to overwrite the database. database is the SUA Balanced
#database <- get(load("countrySUA.RData"))
#database[,StatusFlag :=  1 ]
#database[,LastModified := as.numeric(Sys.time())]
#dbWriteTable(con, name="dbcountry", value=database, append = TRUE)

# Database connection
con <- DBI::dbConnect(SQLite(), "Data/Permanent.db")
contbl <- dplyr::tbl(con, "dbcountry")

# Source all R functions
sapply(list.files(pattern = "[.]R$", path = "R/", full.names = TRUE), source)

# Global flags
tourist_activate <- TRUE

# Load and process CPC codes
all_cpc <- data.table (dbReadTable(concore, name="cpc2.1"))
all_cpc[, c("CONVERSION.FACTORS...FCL.CPC.", "notes") := NULL]
setnames(all_cpc, c("SWS.CODE", "SWS.DESCRIPTOR"), c("CPCCode", "Commodity"))

# Fix specific CPC codes
all_cpc[CPCCode == "21439.040000000001", CPCCode := "21439.04"]
all_cpc[CPCCode == "39141", CPCCode := "39140.02"]

# Add missing CPC codes
add_cpc <- data.table(
  CPCCode = c("21439.9", "01199"),
  Commodity = c("Juice of fruits n.e.", "Other cereals")
)
all_cpc <- rbind(all_cpc, add_cpc)
all_cpc <- all_cpc[!duplicated(all_cpc[, "CPCCode"])]
all_cpc_codes <- unique(all_cpc$CPCCode)

# Load and process element codes
all_elements <- data.table(dbReadTable(concore, name="Elements_All"))
all_elements_to_merge <- copy(all_elements)

# Filter relevant element codes
relevant_elements <- c(
  "5510", "5610", "5910", "5071", "5520", "5016", "5525", "5141",
  "5318", "5319", "5320", "5314", "5327", "5313", "5321", "5165",
  "5113", "5166", "5312", "5025", "5023", "665", "664", "674",
  "684", "1001", "1003", "1005", "261", "271", "281","R5520","R5525","R5016"
)
all_elements <- all_elements[ElementCode %in% relevant_elements]
all_element_codes <- unique(all_elements$ElementCode)

# Create all possible code combinations
allCodes <- data.table(expand.grid(
  CPCCode = all_cpc_codes,
  ElementCode = all_element_codes
))
#function to read rds for data.table

# Load and process production classification
classification <- data.table(dbReadTable(concore, name="production_list_cpc"))
classification <- classification[CPCCode == "39141", CPCCode := "39140.02"]
setnames(classification, "Commodity", "Commodity_name")
classification <- merge(classification, all_cpc, by = "CPCCode", all.x = TRUE)
classification[is.na(Commodity), Commodity := Commodity_name]
classification <- classification[!is.na(Commodity)][, Commodity_name := NULL]

# Function to set column widths for tables
set_hot_colwidths <- function(data) {
  ncols <- ncol(data)
  widths <- rep(100, ncols + 1)
  widths[grep("^CPCCode", colnames(data))] <- 0.1
  widths[grep("Commodity", colnames(data))] <- 0.1
  widths[grep("^Flag", colnames(data))] <- 0.1
  return(widths)
}

# Load country data
countries <- data.table(dbReadTable(concore, name="countries"))
country_selc <- unique(countries[, Country])

#fbs tree

fbsTree <- data.table(dbReadTable(concore, name="fbs_tree"))
#nutrient Elements
nutrientEle <- data.table(dbReadTable(concore, name="nutrientEle"))
nutrientEle[, new := paste(Element, Unit)]
nutrientEle[, Element := NULL]
setnames(nutrientEle, "new","Element")
# elements to bind
new_nutrient_element <- c(4008,4031, 4009,4006,4007,4001,4010,4011,4023,4012,4013,4022,4014,4021,4017,
                          4018,4029,4030,4015
)
nutri2bind <- nutrientEle[,c("measuredElement","Measured.Element"),with= F]
setnames(nutri2bind, c("measuredElement","Measured.Element"),c("ElementCode","Element"))
all_elements <- rbind(all_elements,nutri2bind, nutrientEle[,c("ElementCode","Element"),with = F])

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

## Require packages or install if missing
packageRequirevInstall()
