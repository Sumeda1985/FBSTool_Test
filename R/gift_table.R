GIFT<- function(input,output,session){
  observeEvent(input$total_DES, {
   t <- as.numeric(input$endyear)
   # Load and filter SUA-balanced data
    sua_balanced <- fread_rds("SUA-FBS Balancing/Data/sua_balanced.rds")[
      timePointYears %in% 2014:t & 
        !measuredElementSuaFbs %in% c("261", "271", "281", "4031", "4030", "4008", "4023", "4014", "4001") & 
        measuredElementSuaFbs %in% c("664", "674", "684", new_nutrient_element)
    ]
   # Drop unneeded columns
    sua_balanced[, c("geographicAreaM49", "flagObservationStatus") := NULL]
   # Load and prepare food grouping data
    grouping_data <- fread_rds("Data/Food Grouping_SUA to GIFT_for FBS tool.rds")
    grouping_data[, `SUA description (CPC)` := NULL]
    setnames(grouping_data, "SUA code (CPC)", "CPCCode")
    grouping_data <- grouping_data[!duplicated(grouping_data[, .(`Food group code (GIFT)`, CPCCode)])]
    # Merge with grouping info
    setnames(sua_balanced, "measuredItemFbsSua", "CPCCode")
    sua_balanced <- merge(sua_balanced, grouping_data, by = "CPCCode", all.x = TRUE)[
      !is.na(`Food groups (GIFT)`), 
      !"CPCCode"
    ]
    # Aggregate values
    sua_balanced <- sua_balanced[, .(Aggregate_Val = sum(Value)), 
                                 by = .(`Food group code (GIFT)`, `Food groups (GIFT)`, timePointYears, measuredElementSuaFbs)]
    # Reshape from long to wide
    sua_balanced <- data.table(dcast(
      sua_balanced, 
      `Food group code (GIFT)` + `Food groups (GIFT)` + measuredElementSuaFbs ~ timePointYears, 
      value.var = "Aggregate_Val"
    ))
    # Add element metadata
    setnames(sua_balanced, "measuredElementSuaFbs", "ElementCode")
    sua_balanced[, ElementCode := as.character(ElementCode)]
    sua_balanced <- merge(sua_balanced, all_elements, by = "ElementCode", all.x = TRUE)
   # Reorder columns
    yearcols <- grep("^[0-9]{4}$", names(sua_balanced), value = TRUE)
    setcolorder(sua_balanced, c("Food group code (GIFT)", "Food groups (GIFT)", "ElementCode", "Element", yearcols))
    # Save to reactive value
    value$data_gift <- sua_balanced
 })
  
output$download_gift<- downloadHandler(
    filename = function() {
     "GIFT.xlsx"
    },
   content = function(file) {
    data_download <- data.table(value$data_gift)
     write.xlsx(data_download ,file,row.names = FALSE)
    }
)
  
output$gift_tab <-
    renderDataTable({
      if (is.null(value$data_gift)){
        validate(
          need(nrow(value$data_gift)>0, "Please run the balancing plugin")
        )
      }
     datatable (value$data_gift, rownames= FALSE,class = 'cell-border stripe',
                 extensions = c("FixedColumns","FixedHeader"),
                 options = list(
                   pageLength = 25,
                   fixedHeader= FALSE,
                   scrollX = TRUE,
                   scrollY = "400px"
                   # autoWidth = T,
                   # fixedColumns = list(leftColumns = 6)
                   # columnDefs = list(
                   #   list(visible = FALSE, targets = (ncol(df_gift$data_gift))))
                 ))   %>%
       formatCurrency(columns = as.character(c(2014:input$endyear)),currency = "", digits = 2,interval = 3, mark = ",")
})
}