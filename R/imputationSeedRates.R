imputationSeedRates=function(input,output,session){
show_modal_spinner(
   spin = "cube-grid",
    color = "firebrick",
    text = "Please wait...",
    session = shiny::getDefaultReactiveDomain()
)
t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
data <- data.table(value$data_seed_ratio)
data <- long_format(data)
data[!is.na(Value) & is.na(Flag), Flag := ""]
data[is.na(Value) & Flag == "", Flag := NA_character_ ]
for (j in seq(t)){
  data_seed_Rates <- list() 
  data_seed_Rates[[j]] <- subset(data , Year %in% c(2010 :t[j]))
  data_seed_Rates[[j]][ , new_value := ifelse(Year == t[j] & !Flag %in%  c("", "T","E"), NA, Value)]
  data_seed_Rates[[j]][ , Value := ifelse(Year == t[j] & !Flag %in%  c("", "T","E"), NA, Value)]
  data_seed_Rates[[j]][ , new_flag := ifelse(Year == t[j] & !Flag %in%  c("", "T","E"), NA, Flag)]
  data_seed_Rates[[j]][ , Flag := ifelse(Year == t[j] & !Flag %in%  c("", "T","E"), NA, Flag)]
  data_seed_Rates[[j]] <-
    data_seed_Rates[[j]] %>%
    group_by(CPCCode) %>%
    arrange(CPCCode) %>%
    tidyr::fill(new_value, new_flag, .direction = "down") %>%
    # tidyr::fill(Value, flagObservationStatus, flagMethod, .direction = "down") %>%
    setDT()
data_seed_Rates[[j]][is.na(Value), Value := new_value]
data_seed_Rates[[j]][is.na(Flag), Flag := new_flag]
data_seed_Rates[[j]][, c("new_value","new_flag") := NULL]
new_data <- rbindlist(data_seed_Rates)
data <- data[!Year %in% c(2010:t[j])]
data <- rbind(data,new_data)
data <-  data[order(CPCCode, Year)]
}
data <- wide_format(data)
Sys.sleep(3)
remove_modal_spinner()
observeEvent(input$seed_imputation_ratio,{
   sendSweetAlert(
     session = session,
     title = "Imputed !!",
     text = "Missing values have been imputed successfully. Please refer to the manual for the methodology applied.",
     type = "success"
   )
 })
 return(data)
}
 
 
   
