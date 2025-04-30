
Calculate_Food_Processing <- function(input,output,session){

  dbg_print <- function(x) {
  print(paste0("NEWBAL (", COUNTRY, "): ", x))
}
files = dir("SUA-FBS Balancing/R",full.names = TRUE)

for(i in files){
  source(i, local = TRUE)
}
t=as.numeric(as.numeric(2010) : as.numeric(input$endyear))
basedir <-getwd()
start_time <- Sys.time()
sapply(dir("SUA-FBS Balancing/R", full.names = TRUE), source)
COUNTRY <- as.character(unique(input$countrym49))
dbg_print("parameters")
YEARS <- as.character(t)
p <- defaultStandardizationParameters()
p$itemVar <- "measuredItemSuaFbs"
p$mergeKey[p$mergeKey == "measuredItemCPC"] <- "measuredItemSuaFbs"
p$elementVar <- "measuredElementSuaFbs"
p$childVar <- "measuredItemChildCPC"
p$parentVar <- "measuredItemParentCPC"
p$createIntermetiateFile <- "TRUE"
p$protected <- "Protected"
p$official <- "Official"
# RemainingToProcessedParent() and RemainingProdChildToAssign() will
# be used in the derivation of shareDownUp

RemainingToProcessedParent<-function(data){
  data[, 
       parent_already_processed:=ifelse(is.na(parent_qty_processed),parent_qty_processed,
                                        sum(processed_to_child/extractionRate,na.rm = TRUE)),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
  ]
  
  data[,remaining_processed_parent:=round(parent_qty_processed-parent_already_processed)]
  
  data[remaining_processed_parent<0,remaining_processed_parent:=0]
  data[,
       only_child_left:=ifelse(sum(is.na(processed_to_child))==1 & 
                                 is.na(processed_to_child) &
                                 !is.na(production_of_child) &
                                 !is.na(parent_qty_processed) & 
                                 production_of_child>0,TRUE,FALSE),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
  ]
  
  data[only_child_left==TRUE,processed_to_child:=remaining_processed_parent*extractionRate]
  
  data[,
       parent_already_processed:=ifelse(is.na(parent_qty_processed), parent_qty_processed,
                                        sum(processed_to_child/extractionRate,na.rm = TRUE)),
       by=c("geographicAreaM49","measuredItemParentCPC","timePointYears")
  ]
  
  data[,remaining_processed_parent:=round(parent_qty_processed-parent_already_processed)]
  data[remaining_processed_parent<0,remaining_processed_parent:=0]
  
  return(data)
}


RemainingProdChildToAssign<-function(data){
  data[,
       available_processed_child:=sum(processed_to_child,na.rm = TRUE),
       by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")
  ]
  
  data[,remaining_to_process_child:=round(production_of_child-available_processed_child)]
  data[remaining_to_process_child<0,remaining_to_process_child:=0]
  
  data[,
       only_parent_left:=ifelse(sum(is.na(processed_to_child))==1 & 
                                  is.na(processed_to_child) &
                                  !is.na(parent_qty_processed) & 
                                  parent_qty_processed>=0,TRUE,FALSE)
  ]
  
  data[only_parent_left==TRUE,processed_to_child:=0]
  
  data[,available_processed_child:=sum(processed_to_child,na.rm = TRUE),
       by=c("geographicAreaM49","measuredItemChildCPC","timePointYears")
  ]
  
  data[,remaining_to_process_child:=round(production_of_child-available_processed_child)]
  data[remaining_to_process_child<0,remaining_to_process_child:=0]
  return(data)
  
}
# The fmax function is used when fixing the processingShare of coproducts.
# If TRUE it means that "+" or "or" cases are involved.
fmax <- function(child, main, share, plusor = FALSE) {
  main <- unique(main)
  
  if (plusor) {
    found <- sum(sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE))
    
    if (found == 0) {
      return(max(share, na.rm = TRUE))
    } else if (found == 1) {
      return(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]])
    } else { # should be 2
      return(max(share[(1:length(child))[sapply(child, function(x) grepl(x, main), USE.NAMES = FALSE)]], na.rm = TRUE))
    }
  } else {
    if (sum(grepl(main, child)) > 0) {
      share[child == main]
    } else {
      max(share, na.rm = TRUE)
    }
  }
}

rollavg <- function(x, order = 3) {
  # order should be > 2
  stopifnot(order >= 3)
  non_missing <- sum(!is.na(x))
  # For cases that have just two non-missing observations
  order <- ifelse(order > 2 & non_missing == 2, 2, order)
  
  if (non_missing == 1) {
    x[is.na(x)] <- na.omit(x)[1]
  } else if (non_missing >= order) {
    n <- 1
    while(any(is.na(x)) & n <= 10) { # 10 is max tries
      movav <- suppressWarnings(RcppRoll::roll_mean(x, order, fill = 'extend', align = 'right'))
      movav <- data.table::shift(movav)
      x[is.na(x)] <- movav[is.na(x)]
      n <- n + 1
    }
    
    x <- zoo::na.fill(x, 'extend')
  }
  
  return(x)
}

#####################################  TREE #################################
dbg_print("download tree")
tree <- readRDS("SUA-FBS Balancing/Data/tree.rds")[timePointYears %in% c(2010:input$endyear)]
#when pulling data to tool, fill extraction rate function is ran. so no need to rerun again. 
if (COUNTRY == "835"){
  tree[, geographicAreaM49 := as.character(835)]
}
tree[Value == 0, Value := NA]
uniqueLevels <- unique(tree[, list(geographicAreaM49, timePointYears)])
levels <- list()
treeLevels <- list()
for (i in seq_len(nrow(uniqueLevels))) {
  filter <- uniqueLevels[i, ]
  treeCurrent <- tree[filter, on = c("geographicAreaM49", "timePointYears")]
  levels <- findProcessingLevel(treeCurrent, "measuredItemParentCPC", "measuredItemChildCPC")
  setnames(levels, "temp", "measuredItemParentCPC")
  treeLevels[[i]] <- merge(treeCurrent, levels, by = c("measuredItemParentCPC"), all.x = TRUE)
}
tree <- rbindlist(treeLevels)
# XXX there are no different process levels, but check it
tree[,
     processingLevel := max(processingLevel, na.rm = TRUE),
     by = c("geographicAreaM49", "timePointYears",
            "measuredElementSuaFbs", "measuredItemChildCPC")
]
# XXX Check if this one is still good or it can be obtained within the dataset
processed_item_datatable <- fread_rds("SUA-FBS Balancing/Data/processed_item_datatable.rds")
processedCPC <- processed_item_datatable[, measured_item_cpc]
# XXX what is this for?
itemMap <- readRDS("SUA-FBS Balancing/Data/itemMap.rds")
itemMap <- itemMap[, list(measuredItemSuaFbs = code, type)]
##################################### / TREE ################################
coproduct_table <- readRDS("SUA-FBS Balancing/Data/zeroweight_coproducts.rds")
elemKeys <- c("5510", "5610", "5071", "5113", "5910", "5016",
              "5165", "5520", "5525", "5164", "5166", "5141")
Utilization_Table <- readRDS("SUA-FBS Balancing/Data/utilization_table_2018.rds")
stockable_items <- Utilization_Table[stock == 'X', cpc_code]
zeroWeight <- readRDS("SUA-FBS Balancing/Data/zeroWeight.rds")$x
flagValidTable=readRDS("SUA-FBS Balancing/Data/flagValidTable.rds")
flagValidTable[, flagObservationStatus := as.factor(flagObservationStatus)]
dbg_print("download data")
data_2010 <- data.table(dbReadTable(con, "dbcountry"))
data_2010 <- data_2010[,c("StatusFlag","LastModified") := NULL]
data_2010[!is.na(Value) & is.na(Flag), Flag := ""]
#commented for Zanzibar
data_2000 <- get(load("Data/countrySUA_2000_2009.RData"))
# data <- rbind(data_2010)
#commented for zanzibar
data <- rbind(data_2010,data_2000)
data[,c("Country","Commodity","Element") := NULL]
setnames(data,c("CountryM49","ElementCode","CPCCode","Year","Value","Flag"),
         c("geographicAreaM49","measuredElementSuaFbs","measuredItemFbsSua","timePointYears","Value","flagObservationStatus")
)
#################### FODDER CROPS ##########################################
# Some of these items may be missing in reference files,
# thus we carry forward the last observation.
fodder_crops_items <-
  tibble::tribble(
    ~description, ~code,
    "Maize for forage", "01911",
    "Alfalfa for forage", "01912",
    "Sorghum for forage", "01919.01",
    "Rye grass for forage", "01919.02",
    "Other grasses for forage", "01919.91",
    "Clover for forage", "01919.03",
    "Other oilseeds for forage", "01919.94",
    "Other legumes for  forage", "01919.92",
    "Cabbage for fodder", "01919.04",
    "Mixed grass and legumes for forage", "01919.93",
    "Turnips for fodder", "01919.05",
    "Beets for fodder", "01919.06",
    "Carrots for fodder", "01919.07",
    "Swedes for fodder", "01919.08",
    "Other forage products, nes", "01919.96",
    "Other forage crops, nes", "01919.95",
    "Hay for forage, from legumes", "01919.10",
    "Hay for forage, from grasses", "01919.09",
    "Hay for forage, from other crops nes", "01919.11"
  )
fodder_crops_availab <-
  data[
    measuredItemFbsSua %in% fodder_crops_items$code &
      measuredElementSuaFbs == "5510"
  ]
if (nrow(fodder_crops_availab) > 0) {
  fodder_crops_complete <-
    CJ(
      geographicAreaM49 = unique(fodder_crops_availab$geographicAreaM49),
      measuredElementSuaFbs = "5510",
      timePointYears = unique(data$timePointYears),
      measuredItemFbsSua = unique(fodder_crops_availab$measuredItemFbsSua)
    )
  
  fodder_crops_complete <-
    fodder_crops_complete[order(geographicAreaM49, measuredItemFbsSua, timePointYears)]
  
  fodder_crops <-
    merge(
      fodder_crops_complete,
      fodder_crops_availab[, .(geographicAreaM49, measuredElementSuaFbs,
                               measuredItemFbsSua, timePointYears, Value)],
      by = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears"),
      all.x = TRUE
    )
  
  fodder_crops[,
               Value := zoo::na.locf(Value),
               by = c("geographicAreaM49", "measuredItemFbsSua")
  ]
  fodder_crops_new <-
    fodder_crops[
      !fodder_crops_availab,
      on = c("geographicAreaM49", "measuredElementSuaFbs",
             "measuredItemFbsSua", "timePointYears")
    ]
  if (nrow(fodder_crops_new) > 0) {
    
    # fodder_crops_new[, `:=`(flagObservationStatus = "E", flagMethod = "t")]
    
    fodder_crops_new[, `:=`(flagObservationStatus = "E")]
    
    data <- rbind(data, fodder_crops_new)
    
  }
}
#################### / FODDER CROPS ########################################
original_opening_stocks <- data[measuredElementSuaFbs == "5113"]
original_opening_stocks[, flagMethod := NULL]
original_opening_stocks <-
  flagValidTable[
    original_opening_stocks,
    on = c("flagObservationStatus")
  ][,
    Valid := NULL
  ]
# Remove protected in 2014 from cumulated
all_opening_stocks<- copy(original_opening_stocks)
all_opening_stocks <- all_opening_stocks[!is.na(timePointYears)]
data <- merge(data, flagValidTable, by = c("flagObservationStatus"), all.x = TRUE)
data[flagObservationStatus %in% c("", "T"), `:=`(Official = TRUE, Protected = TRUE)]
data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]
# We remove "5113" (opening stocks) as will be stored in separate table.
data <- data[measuredElementSuaFbs != "5113"]
dbg_print("elementToCodeNames")
# XXX FIXME: the elementCodesToNames below is
# not working proberply, see issue #38
codes <- tibble::tribble(
  ~measuredElementSuaFbs,  ~name,
  "5910", "exports",
  "5520", "feed",
  "5141", "food",
  "5023", "foodmanufacturing",
  "5610", "imports",
  "5165", "industrial",
  "5016", "loss",
  "5510", "production",
  "5525", "seed",
  "5164", "tourist",
  "5071", "stock_change"
)
data <- merge(data, codes, by = "measuredElementSuaFbs", all.x = TRUE)
data[, measuredElementSuaFbs := name]
data[, name := NULL]
# XXX: there are some NAs here, but probably there shouldn't
data <- data[!is.na(measuredElementSuaFbs)]
setnames(data, "measuredItemFbsSua", "measuredItemSuaFbs")
dbg_print("convert sugar")
# XXX
##############################################################
######### SUGAR RAW CODES TO BE CONVERTED IN 2351F ###########
##############################################################
data <- convertSugarCodes(data)
########## / Remove feed if new element and negative imbalance is huge
treeRestricted <- tree[, .(measuredItemParentCPC, measuredItemChildCPC, processingLevel)]
treeRestricted <- unique(treeRestricted[order(measuredItemChildCPC)])
primaryInvolved <- getPrimary(processedCPC, treeRestricted, p)
dbg_print("primary involved descendents")
# XXX: check here, 0111 is in results
primaryInvolvedDescendents <-
  getChildren(
    commodityTree = treeRestricted,
    parentColname = "measuredItemParentCPC",
    childColname = "measuredItemChildCPC",
    topNodes = primaryInvolved
  )
# stocks need to be generated for those items for
# which "opening stocks" are available
items_to_generate_stocks <-
  unique(all_opening_stocks$measuredItemFbsSua)
deriv <-
  CJ(
    measuredItemSuaFbs    = unique(tree$measuredItemChildCPC),
    measuredElementSuaFbs = 'production',
    geographicAreaM49     = unique(data$geographicAreaM49),
    timePointYears        = unique(data$timePointYears)
  )
# rbind with anti_join
data <-
  rbind(
    data,
    deriv[!data, on = c('measuredItemSuaFbs', 'measuredElementSuaFbs',
                        'geographicAreaM49', 'timePointYears')],
    fill = TRUE
  )
data[is.na(Official), Official := FALSE]
data[is.na(Protected), Protected := FALSE]
data[, stockable := measuredItemSuaFbs %chin% stockable_items]
# 5 non-missing/non-null data points exist
historical_avail_stocks <-
  data[
    measuredElementSuaFbs == "stock_change" &
      timePointYears <= 2013 &
      !is.na(Value) &
      stockable == TRUE,
    .(n = sum(!dplyr::near(Value, 0))),
    by = c("geographicAreaM49", "measuredItemSuaFbs")
  ][
    n >= 5
  ][,
    n := NULL
  ]
# Keep processingShare and shareDownUp
computed_shares <- list()
computed_shares_send <- list()
# Keep negative availability
negative_availability <- list()
#keep shareuPdOWN
updated_shareUpDOwn<-list()
fixed_proc_shares <- list()
original_stock_variation <-
  data[
    measuredElementSuaFbs == "stock_change" & timePointYears >= 2014 & !is.na(Value),
    .(
      geographicAreaM49, measuredItemSuaFbs, timePointYears, Value, flagObservationStatus
    )
  ]
########################  
dbg_print("Calculate Food Processing ")
treeProc <-
  tree[
    !is.na(Value) &
      # processingLevel == lev &
      measuredElementSuaFbs == 'extractionRate',
    list(
      measuredItemParentCPC,
      geographicAreaM49,
      measuredItemChildCPC,
      timePointYears,
      extractionRate = Value
    )
  ]
#     
#     #tree containing children of all parent of current level
tree_parent_Level<-
  tree[
    !is.na(Value) &
      measuredElementSuaFbs == 'extractionRate' &
      measuredItemParentCPC %in% treeProc[, get(p$parentVar)],
    list(
      measuredItemParentCPC,
      geographicAreaM49,
      measuredItemChildCPC,
      timePointYears,
      extractionRate = Value
      # processingLevel
    )
  ]

dataMergeTree <- data[measuredElementSuaFbs %chin% c('production', 'imports', 'exports', 'stock_change')]
data_proc <- copy(dataMergeTree)
setnames(dataMergeTree, "measuredItemSuaFbs", "measuredItemParentCPC")
dataMergeTree[, geographicAreaM49 := as.character(geographicAreaM49)]
tree_parent_Level[, geographicAreaM49 := as.character(geographicAreaM49)]
tree_parent_Level[, timePointYears := as.character(timePointYears)]
dataMergeTree <-
  merge(
    dataMergeTree,
    tree_parent_Level,
    by = c(p$parentVar, p$geoVar, p$yearVar),
    allow.cartesian = TRUE
  )
dataMergeTree[,
              availability :=
                sum(
                  Value[get(p$elementVar) %in% c(p$productionCode, p$importCode)],
                  # XXX p$stockCode is "stockChange", not "stock_change"
                  - Value[get(p$elementVar) %in% c(p$exportCode, "stock_change")],
                  na.rm = TRUE
                ),
              by = c(p$geoVar, p$yearVar, p$parentVar, p$childVar)
]
# XXX: Replace negative availability, so we get zero production, instead of negative. This, , however, should be fixed in advance, somehow.
dataMergeTree[availability < 0, availability := 0]
dataMergeTree[,
              availabilitieChildEquivalent := availability * extractionRate #,
              #by = c(params$geoVar, params$yearVar, params$parentVar, params$childVar, "measuredElementSuaFbs")
]
dataMergeTree[,
              sumAvail := sum(unique(na.omit(availabilitieChildEquivalent))),
              by = c(p$childVar, p$yearVar, p$geoVar)  #, "measuredElementSuaFbs"
]
dataMergeTree[, shareDownUp := availabilitieChildEquivalent / sumAvail]
dataMergeTree[shareDownUp > 1, shareDownUp := 1]
dataMergeTree[shareDownUp < 0, shareDownUp := 0]
dataMergeTree[is.nan(shareDownUp), shareDownUp := 0]
setkey(dataMergeTree, NULL)
dataMergeTree <-
  unique(
    dataMergeTree[,
                  list(geographicAreaM49, timePointYears, measuredItemParentCPC,
                       measuredItemChildCPC, extractionRate, availability, shareDownUp)
    ]
  )
tree_with_shareDWN <- copy(dataMergeTree)
#data_proc includes data for the current level
setnames(data_proc, p$itemVar, p$childVar)
tree_with_shareDWN = merge(tree_with_shareDWN, data_proc, by = c(p$geoVar,p$yearVar, p$childVar), all.x = TRUE)
tree_with_shareDWN[, weight := ifelse(get(p$childVar) %in% zeroWeight,0,1)]
# dataMergeTree[timePointYears >= 2014 & Protected == FALSE, Value := NA][, Protected := NULL]
tree_with_shareDWN[, foodProcElement:= ((Value/extractionRate)*shareDownUp)*weight]
tree_with_shareDWN <- tree_with_shareDWN[measuredElementSuaFbs == "production"]
food_proc_table=tree_with_shareDWN[,list(measuredItemSuaFbs=get(p$parentVar),geographicAreaM49 = get(p$geoVar) ,timePointYears = get(p$yearVar)
                                         ,foodProcElement)]
food_proc_table <-  food_proc_table[, 
                                    list(Value = sum(foodProcElement, na.rm = TRUE)), 
                                    by = list(geographicAreaM49,measuredItemSuaFbs, timePointYears)]
food_proc_table <- food_proc_table[Value > 0]
food_proc_table <- food_proc_table[!is.na(Value)]
if (dim(food_proc_table)[1] != 0){
  proc_to_bind <- food_proc_table[, list(geographicAreaM49,measuredItemSuaFbs,timePointYears,Value,
                                         measuredElementSuaFbs = p$foodProcCode, flagObservationStatus ="E", Valid = TRUE
                                         , Protected = TRUE, Official = FALSE)]
  proc_to_bind[, stockable := measuredItemSuaFbs %chin% stockable_items]
  proc_to_bind <- unique(proc_to_bind)
  data <- rbind(data, proc_to_bind)
  
}
dbg_print("end Food Processing")
food_processing <- data[measuredElementSuaFbs == "foodManufacturing"]
food_processing <- food_processing[, c("measuredItemSuaFbs","timePointYears", "Value", "flagObservationStatus")]
food_processing[,ElementCode := "5023"]
setnames(food_processing,c("measuredItemSuaFbs","timePointYears","flagObservationStatus"),
         c("CPCCode","Year","Flag"))
food_processing <- merge(food_processing,all_cpc, by= c("CPCCode"),all.x = T)
food_processing <- merge(food_processing,all_elements, by= c("ElementCode"),all.x = T)
setcolorder(food_processing,c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
food_processing <-food_processing[Year %in% t]
return(food_processing)
}