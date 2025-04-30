imputeSeed =function(input,output,session){
show_modal_spinner(
    
    spin = "cube-grid",
    color = "firebrick",
    text = "Please wait...",
    session = shiny::getDefaultReactiveDomain()
)
t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
data_Session <- data.table(value$data_sua_unbalanced )
data_Session <- long_format(data_Session)
seedData=subset(data_Session, ElementCode == "5525")
seedData[is.na(Value), Flag := NA ]
setnames(seedData, c("Value", "Flag"), c("[5525] Seed [t]", "[5525] Flag"))
seedData[, c("ElementCode","Element"):= NULL]
seedData$`[5525] Flag` =ifelse(!is.na(seedData$`[5525] Seed [t]`) & is.na(seedData$`[5525] Flag`), "", seedData$`[5525] Flag`)
if (unique(value_database$data$CountryM49) %in% "835"){
      itemSeed <- fread_rds("Data/seedRate.rds")
      itemSeed=unique(itemSeed[,c("CPCCode", "Commodity")])
      itemSeed_session=subset(data_Session, ElementCode == "5525")
      itemSeed_session <- unique(itemSeed_session[,c("CPCCode","Commodity")])
      itemSeed <- rbind(itemSeed,itemSeed_session)
      itemSeed <-itemSeed[!duplicated(itemSeed[,c("CPCCode")])]
   }else {
      itemSeed_session=subset(data_Session, ElementCode == "5525")
      itemSeed_session <- unique(itemSeed_session[,c("CPCCode","Commodity")])
      itemSeed <- fread_rds("Data/seedRate.rds")
      itemSeed=unique(itemSeed[,c("CPCCode", "Commodity")])
      itemSeed <- rbind(itemSeed,itemSeed_session)
      itemSeed <-itemSeed[!duplicated(itemSeed[,c("CPCCode")])]
}
    
productionData <- data_Session[ElementCode == "5510" & CPCCode %in% itemSeed$CPCCode]
productionData$Flag =ifelse(!is.na(productionData$Value) & is.na(productionData$Flag), "", productionData$Flag)
areaHarvData  <- value_database$data[ElementCode == "5312"]
areaHarvData[,c("CountryM49","Country"):=NULL]
setnames(areaHarvData, c("Value", "Flag"), c("[5312] Area Harvested [ha]", "[5312] Flag"))
other_variables <- NULL
for (j in t){
area_harv_data <- areaHarvData[Year %in% c(2010:(j-1)) , Protected_5312 :=TRUE]
area_harv_data[Year %in% c(j) & `[5312] Flag` == "" | `[5312] Flag` == "T"| `[5312] Flag` == "E", Protected_5312 :=TRUE]
area_harv_data[Year %in% c(j) & `[5312] Flag` %in% c("M","I") , Protected_5312 :=FALSE]
area_harv_data[,c("ElementCode", "Element") := NULL]
#Area Sown must be taken from the Seed sesssion 
seedSession <- long_format(data.table(value$data_seed))
#filter for area sown
areaSownData <- seedSession[ElementCode == "5025"]
areaSownData <- areaSownData[Year %in% c(2010:j)]
setnames(areaSownData, c("Value", "Flag"), c("[5025] Area Sown [ha]", "[5025] Flag"))
areaSownData$`[5025] Flag` =ifelse(!is.na(areaSownData$`[5025] Area Sown [ha]`) & is.na(areaSownData$`[5025] Flag`), "", areaSownData$`[5025] Flag`)
areaSownData$`[5025] Flag` =ifelse(is.na(areaSownData$`[5025] Area Sown [ha]`) & areaSownData$`[5025] Flag` == "", NA, areaSownData$`[5025] Flag`)
areaSownData[Year %in% c(2010:(j-1)) & !is.na(`[5025] Area Sown [ha]`), Protected_5025 :=TRUE]
areaSownData[Year %in% c(j) & `[5025] Flag` == "" | `[5025] Flag` == "T"| `[5025] Flag` == "E", Protected_5025 :=TRUE]
areaSownData[Year %in% c(j) & `[5025] Flag` %in% c("M","I") , Protected_5025 :=FALSE]
    
#let us make empty area sown for unprotected
areaSownData[Protected_5025 == FALSE, `[5025] Area Sown [ha]` := NA]
areaSownData$`[5025] Flag` =ifelse(is.na(areaSownData$`[5025] Area Sown [ha]`) & !is.na(areaSownData$`[5025] Flag`), NA, areaSownData$`[5025] Flag`)
areaSownData[,c("ElementCode","Element") := NULL]   
imputeData <- copy(seedData)
seedRates <- fread_rds("Data/seedRate.rds")
seedRates[,c("ElementCode","Element") := NULL]
seedRates <- subset(seedRates, select= c("CPCCode","Commodity",j,paste("Flag", j)))
setnames(seedRates, c(j, paste("Flag", j)), c("Seed Rate [Kg/ha]", "[Seed Rate] Flag"))
seedRates$`[Seed Rate] Flag` =ifelse(!is.na(seedRates$`Seed Rate [Kg/ha]`) & is.na(seedRates$`[Seed Rate] Flag`), "", seedRates$`[Seed Rate] Flag`)
imputeData <-  data.table (merge(imputeData[,-c("Commodity")],areaHarvData[,-c("Commodity")],by =c("CPCCode", "Year"),
                     all = TRUE))
imputeData <-  data.table(merge(imputeData,areaSownData[,-c("Commodity")],by =c("CPCCode", "Year"),
                     all = TRUE))
imputeData <- data.table(merge(imputeData,seedRates[CPCCode %in% unique(imputeData$CPCCode)][,-c("Commodity")],
                     by = c("CPCCode"),
                     all = TRUE))
imputeData <- data.table(merge(imputeData,all_cpc, by= c("CPCCode"), all.x = TRUE))
cols <- c("[5525] Seed [t]", "[5312] Area Harvested [ha]", "[5025] Area Sown [ha]", "Seed Rate [Kg/ha]")
imputeData[cols] <- lapply(imputeData[cols], as.numeric)    
imputeData[Year %in% c(2010:(as.numeric(j)-1)), Protected_5525 :=TRUE]
#expert knowledge estiamtion is protected. 
imputeData[Year %in% c(j) & `[5525] Flag` == "" | `[5525] Flag` == "T"  | `[5525] Flag` == "E" , Protected_5525 :=TRUE]
imputeData[Year %in% c(j) & `[5525] Flag` %in% c("M","I") , Protected_5525 :=FALSE]
 imputeData[Year %in% c(j) & is.na(`[5525] Flag`),Protected_5525 :=FALSE]
## Impute missing data
    # following the guidelines 
    # If Area Sown is missing, set Area Sown = Area Harvested
    imputeData$`[5025] Area Sown [ha]` = ifelse(is.na(imputeData$`[5025] Area Sown [ha]`),imputeData$`[5312] Area Harvested [ha]`,imputeData$`[5025] Area Sown [ha]`)
    # imputeData$`Flag[5025]`=ifelse(!is.na(imputeData$`[5025] Area Sown [ha]`) & imputeData$Protected_5025 ==FALSE,"I",imputeData$`Flag[5025]`)
    
    imputeData$`[5025] Flag` = ifelse(!is.na(imputeData$`[5025] Area Sown [ha]`) & is.na(imputeData$`[5025] Flag`),"I",imputeData$`[5025] Flag`)
    # imputeData$Protected_5025= ifelse(!is.na(imputeData$`[5025] Area Sown [ha]`) & is.na(imputeData$Protected_5025),FALSE,imputeData$Protected_5025)
   imputeData <- imputeData[Year %in% c(2010:j)]
# Given Seed and Area Sown we impute Seed Rate
 for(k in 2010:j){
      if(k != j){
        imputeData[Year == k,]$`Seed Rate [Kg/ha]` = ifelse(is.na(imputeData[Year == k,]$`Seed Rate [Kg/ha]`), 1000*(imputeData[Year == k,]$`[5525] Seed [t]`)/(imputeData[Year == k+1,]$`[5025] Area Sown [ha]`),imputeData[Year == k,]$`Seed Rate [Kg/ha]`)
      } else {
        imputeData[Year == k,]$`Seed Rate [Kg/ha]` = ifelse(is.na(imputeData[Year == k,]$`Seed Rate [Kg/ha]`), 1000*imputeData[Year == k,]$`[5525] Seed [t]`/imputeData[Year == k,]$`[5025] Area Sown [ha]`,imputeData[Year == k,]$`Seed Rate [Kg/ha]`)
      }
    }
    # imputeData$`Seed Rate [Kg/Ha]` = ifelse(is.na(imputeData$`Seed Rate [Kg/Ha]`), 1000*imputeData$`[5525] Seed [t]`/imputeData$`[5025] Area Sown [ha]`,imputeData$`Seed Rate [Kg/Ha]`)
   imputeData[, `Seed Rate [Kg/ha]` := round(`Seed Rate [Kg/ha]`, 0)]
   imputeData[Protected_5525 == FALSE, `:=`(
     `[5525] Seed [t]` = NA,
     `[5525] Flag` = NA
   )]
  imputeData$`[Seed Rate] Flag`=ifelse(!is.na(imputeData$`Seed Rate [Kg/ha]`)&is.na(imputeData$`[Seed Rate] Flag`), "I",imputeData$`[Seed Rate] Flag`)
    
    # Given Area Sown and Seed Rates we impute Seed
    for(k in 2010:j){
      
      if(k != j ){
        imputeData[Year == k ,]$`[5525] Seed [t]` = ifelse(is.na(imputeData[Year == k ,]$`[5525] Seed [t]`), imputeData[Year == k + 1,]$`[5025] Area Sown [ha]` * imputeData[Year == k,]$`Seed Rate [Kg/ha]` /1000, imputeData[Year == k,]$`[5525] Seed [t]`)
      } else {
        imputeData[Year == k ,]$`[5525] Seed [t]` = ifelse(is.na(imputeData[Year == k,]$`[5525] Seed [t]`), imputeData[Year == k,]$`[5025] Area Sown [ha]` * imputeData[Year == k,]$`Seed Rate [Kg/ha]` /1000, imputeData[Year == k,]$`[5525] Seed [t]`)
      }
      
    }
    # imputeData$ = ifelse(is.na(imputeData$`[5525] Seed [t]`), imputeData$`[5025] Area Sown [ha]` * imputeData$`Seed Rate [Kg/Ha]` /1000, imputeData$`[5525] Seed [t]`)
    imputeData[, `[5525] Seed [t]` := round(`[5525] Seed [t]`, 0)]
    imputeData$`[5525] Flag` = ifelse(!is.na(imputeData$`[5525] Seed [t]`) & is.na(imputeData$`[5525] Flag`) & imputeData$Protected_5525 == FALSE,"I",imputeData$`[5525] Flag`)
    imputeData <- imputeData[Year %in% c(j)]
    imputeData[, c("ElementCode","Element"):= NULL]
    data <- merge(seedData[, -c("Commodity")], imputeData[, -c("Commodity")], by = c("CPCCode","Year"), all= TRUE)
    data[, `[5525] Seed [t].x`:= ifelse(Protected_5525 == F & Year == j,`[5525] Seed [t].y`,`[5525] Seed [t].x`)]
    data[, `[5525] Flag.x`:= ifelse(Protected_5525 == F & Year == j,`[5525] Flag.y`,`[5525] Flag.x`)]
    data[,c("[5525] Seed [t].y","[5525] Flag.y"):= NULL]
    setnames(data,c("[5525] Seed [t].x","[5525] Flag.x"),c("[5525] Seed [t]","[5525] Flag"))
    data <- merge(data,all_cpc, by= "CPCCode", all.x = TRUE)
    seedData <- copy(data[,c("CPCCode","Commodity","Year","[5525] Seed [t]","[5525] Flag")])
    other_variables <- rbind(other_variables, copy(imputeData[,c("CPCCode","Commodity","Year","[5312] Area Harvested [ha]","[5312] Flag",
                                         "[5025] Area Sown [ha]","[5025] Flag","Seed Rate [Kg/ha]","[Seed Rate] Flag")]))
}
setnames(seedData,c("[5525] Seed [t]","[5525] Flag"),c("Value","Flag"))  
seedData[,ElementCode := "5525"]
seedData <- merge(seedData,all_elements,by=c("ElementCode") ,all.x = TRUE)
#################################################### other variables ##############
otherValueData <- data.table(melt(other_variables[,-c("[5312] Flag","[5025] Flag","[Seed Rate] Flag")], 
                     id.vars = c("CPCCode","Commodity","Year"),
                     variable.name = "Element", value.name = "Value"))
otherValueData$Element = ifelse(otherValueData$Element == "[5312] Area Harvested [ha]","Area Harvested [ha]",
                                      ifelse(otherValueData$Element == "[5025] Area Sown [ha]","Area Sown [ha]",
                                             "Seed Rate [Kg/ha]"
                                      ))
otherValueFlagData <-  data.table (melt(other_variables[,-c("[5312] Area Harvested [ha]","[5025] Area Sown [ha]"
                                 ,"Seed Rate [Kg/ha]")],id.vars = c("CPCCode","Commodity","Year"),
                    variable.name = "Flag Description", value.name = "Flag"))
otherValueFlagData$`Flag Description` = NULL
other_variable_final = cbind(otherValueData,otherValueFlagData[,"Flag"])  
other_variable_final[Element == "Area Harvested [ha]", ElementCode := "5312"]
other_variable_final[Element == "Area Sown [ha]", ElementCode := "5025"]
other_variable_final[Element == "Seed Rate [Kg/ha]", ElementCode := "R5525"]
finalSeedData <- rbind(seedData,other_variable_final) 
finalSeedData <- setcolorder(finalSeedData, c("CPCCode","Commodity","ElementCode","Element","Year","Value","Flag"))
finalSeedData <- finalSeedData[order(CPCCode,Year)]
finalSeedData <- subset(finalSeedData, CPCCode %in% unique(itemSeed$CPCCode))
finalSeedData <- finalSeedData[!duplicated(finalSeedData[,c("CPCCode","ElementCode","Year")])]
finalSeedData[, Value := ifelse(ElementCode =="5525" & Value< 0 , NA, Value)]
finalSeedData <-  finalSeedData[ElementCode == "5525" & is.na(Value), Flag := NA]
finalSeedData[, Value := ifelse(Value == Inf, NA, Value)]
finalSeedData[, Flag := ifelse(is.na(Value), NA, Flag)]
Sys.sleep(3)
  remove_modal_spinner()
observeEvent(input$seed_imputation,{
  sendSweetAlert(
    session = session,
    title = "Imputed !!",
    text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
    type = "success"
  )
})
return(finalSeedData) 

}





















