Run_All_Imputation <- function(input,output,session){
show_modal_spinner(
  spin = "cube-grid",
  color = "firebrick",
  text = "Please wait...",
  session = shiny::getDefaultReactiveDomain()
)
Sys.sleep(3)
remove_modal_spinner()
#stock
print("Start Stock Imputation")
data_stock <- wide_format(imputeStocksChanges(input,output,session))
data_stock <- data_stock[order(CPCCode, factor(ElementCode, levels = c("5113", "5071")))]
data_stock=visualize_data(data_stock,input,session)
data_stock[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
value$data_stock <- data_stock

data_to_save_stock <- copy(value$data_stock)
data_to_save_stock[, hidden := NULL]
data_to_save_stock <- data_to_save_stock[ElementCode %in% c("5071", "5113")]
# It is needed a validation for opening stock and stock variation. If stock var <0 , abs(stock) < opening stock.
stock_validation <- long_format(copy(data_to_save_stock))
yearval <- c(as.numeric(input$fromyear):as.numeric(input$endyear))
stock_validation[,validation := ifelse(Year %in% yearval & Value[ElementCode == "5071"]<0 &
                                         Value[ElementCode == "5113"]< abs(Value[ElementCode == "5071"]),1,0),
                 by = c("CPCCode","Year")]
stock_validation <- stock_validation[validation == 1]  
if (nrow(stock_validation)>0){
  CommoditiesTocheck <- unique(stock_validation$Commodity)
  sendSweetAlert(
    session = session,
    title = "Issue detected in Stock Variation!!",
    text = paste(c("Please check commodities:", CommoditiesTocheck),collapse= " "),
    type = "Error"
  )
}else if (nrow(stock_validation) == 0){
  save_to_database(
    data = data_to_save_stock,
    longData = value$stockDataLong,
    year_range = c(input$fromyear:input$endyear),
    session = session,
    input = input,
    output = output,
    data_session = value$data_stock
  )
  
}
print("end stock imputation")
#loss
print("Start Loss Imputation")
data_loss <- wide_format(imputeLoss(input,output,session))
data_loss=visualize_data(data_loss,input,session)
data_loss[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
value$data_loss <- data_loss
  
data_to_save_loss <- copy(value$data_loss)
data_to_save_loss[, hidden := NULL]
data_to_save_loss <- data_to_save_loss[ElementCode %in% c("5016")]
save_to_database(
  data = data_to_save_loss,
  longData = value$lossDataLong,
  year_range = c(input$fromyear:input$endyear),
  session = session,
  input = input,
  output = output,
  data_session = value$data_loss
)
print("End Loss Imputation")
#feed
print("Start Feed Imputation")
data_feed <- wide_format(imputeFeed(input,output,session))
if ( is.null(data_feed) ){
    sendSweetAlert(
      session = session,
      title = c("Imputation Error"),
      text = c("No time series data to impute"),
      type = "warning"
    )
 } else {
   data_feed <- visualize_data(data_feed,input,session)
   data_feed[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
   value$data_feed <- data_feed
}
data_to_save_feed <- copy(value$data_feed)
data_to_save_feed[, hidden := NULL]
data_to_save_feed <- data_to_save_feed[ElementCode %in% c("5520")]
save_to_database(
  data = data_to_save_feed,
  longData = value$feedDataLong,
  year_range = c(input$fromyear:input$endyear),
  session = session,
  input = input,
  output = output,
  data_session = value$data_feed
)
print("End Feed Imputation")
#seed
print("Start Seed Imputation")
data_seed <- wide_format(imputeSeed(input,output,session))
if ( is.null(data_seed) ){
   sendSweetAlert(
      session = session,
      title = c("Imputation Error"),
      text = c("No time series data to impute"),
      type = "warning"
    )
 } else {
data_seed <- visualize_data(data_seed,input,session)
data_seed[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
value$data_seed <- data_seed
}
data_to_save_seed <- copy(value$data_seed)
data_to_save_seed[, hidden := NULL]
data_to_save_seed <- data_to_save_seed[ElementCode %in% c("5525")]
save_to_database(
  data = data_to_save_seed,
  longData = value$seedDataLong,
  year_range = c(input$fromyear:input$endyear),
  session = session,
  input = input,
  output = output,
  data_session = value$data_seed
)
print("End Seed Imputation")
#food
print("Start Food Imputation")
data_food <- wide_format(imputeFood(input,output,session))
if ( is.null(data_food) ){
    sendSweetAlert(
      session = session,
      title = c("Imputation Error"),
      text = c("No time series data to impute"),
      type = "warning"
    )
} else {
  data_food <- visualize_data(data_food,input,session)
  data_food[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
  value$data_food <- data_food
}
data_to_save_food <- copy(value$data_food)
data_to_save_food[, hidden := NULL]
data_to_save_food <- data_to_save_food[ElementCode %in% c("5141")]
save_to_database(
  data = data_to_save_food,
  longData = value$foodDataLong,
  year_range = c(input$fromyear:input$endyear),
  session = session,
  input = input,
  output = output,
  data_session = value$data_food
)
}
