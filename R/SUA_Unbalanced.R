SUA_Unbalanced <- function(input,output,session){
#load SUA Unbalanced  
observeEvent(input$startContinue,{
  if (input$endyear == ""){
    value$data_sua_unbalanced <- NULL
    validate(
      need(!is.null(value$data_sua_unbalanced), "No data")
    )
}
else {
     dataSUA <- copy(value_database$data)[Year %in% 2010:input$endyear]
     dataSUA <- dataSUA[,c("StatusFlag", "LastModified") := NULL]
 ##################food processing for Zanzibar commented#########################################################################
    food_processing_data <- Calculate_Food_Processing(input,output,session)
     dataSUA <-rbind(dataSUA[,c("CountryM49","Country") := NULL],food_processing_data)
#################################################################################################################    
     sua_elements <- c("5510","5610","5910","5071", "5141", "5525","5520","5165","5166", "5016","5113", "5023")
     dataSUA <- dataSUA[ElementCode %in% sua_elements]
     dataSUA[, Value := round(Value,0)]
     data_residual <- copy(dataSUA)[,c("Commodity","Element") := NULL]
  #####compute imbalance
  data_residual[,
       `:=`(
         supply =
           sum(Value[ElementCode %chin% c("5510","5610")],
               - Value[ElementCode %chin% c("5910","5071")],
               na.rm = TRUE),
         # All elements that are NOT supply elements
         utilizations =
           sum(Value[!(ElementCode %chin% c("5510","5610","5910","5071","5113"))],
               na.rm = TRUE)
       ),
       by = c("CPCCode","Year")
       ][,
         imbalance := supply - utilizations
         ][ , c("supply","utilizations") := NULL
         ][, c("ElementCode","Value", "Flag") := NULL][, ElementCode := c("5166")]
  setnames(data_residual, "imbalance","Value")
  data_residual[, Flag := c("I")]
  data_residual <- unique(data_residual)
  data_residual <- merge(data_residual,all_cpc, by= "CPCCode",all.x = TRUE)
  data_residual <- merge(data_residual,all_elements, by= "ElementCode",all.x = TRUE)
  dataSUA <- rbind(dataSUA, data_residual)
###########################################################################################################################################################
  dataSUA <- wide_format(dataSUA)
 #sort it by fbs tree 
  dataSUA <- merge(dataSUA, fbsTree, by= c("CPCCode"), all.x = TRUE)
  # Define the desired order for ElementCode
  element_levels <- c("5113", "5510", "5610", "5910", "5071", "5141", "5023", "5525", "5520", "5016", "5165", "5166")
  # Order data by CPCCode, ElementCode (with specified levels), then by id4
  dataSUA <-dataSUA[order(CPCCode, factor(ElementCode, levels = element_levels))]
  dataSUA <-dataSUA[order(id4)][, id4 := NULL]
}
validate(
    need(!is.null(dataSUA), "No data")
)
dataSUA[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 
value$data_sua_unbalanced <- dataSUA
})
#download SUA Unbalanced
output$download_sua_unbalanced  <- createDownloadHandler(reactive(value$data_sua_unbalanced),
                                                         "SUA_Unbalanced_data.xlsx")


#Run all imputation in SUA
observeEvent(input$all_imp,{
 Run_All_Imputation(input,output,session)
}) 
#Running the balancingplugin 
observeEvent(input$runPlugin,{ style <- isolate(input$style)
show_modal_spinner(
  spin = "cube-grid",
  color = "firebrick",
  text = "Please wait...",
  session = shiny::getDefaultReactiveDomain()
)
SUAFBS_t(input,output,session)
Sys.sleep(3)
remove_modal_spinner()
sendSweetAlert(
  session = session,
  title = c("The Balancing Plugin"),
  text = c("Completed"),
  type = "success"
)
})

observeEvent(input$startContinue, {
 data <- fread_rds("SUA-FBS Balancing/Data/tree.rds")
 t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
 data <- data[timePointYears %in% t ]
 tree=copy(data)
 tree[, c("geographicAreaM49","flagMethod") := NULL]
 tree <- tree[measuredElementSuaFbs %in% c("extractionRate") & timePointYears %in% as.character(t)]
 tree[, c("measuredElementSuaFbs") := NULL ]
 setnames(tree, c("measuredItemParentCPC", "measuredItemChildCPC", "Value", "flagObservationStatus","timePointYears"), 
          c("CPCCode Parent","CPCCode Child", "Extraction Rate","Flag","Year"))
 tree <- merge(tree,all_cpc,by.x = "CPCCode Parent" , by.y = "CPCCode", all.x = TRUE)
 setnames(tree,"Commodity","Parent Commodity")
 tree <- merge(tree,all_cpc,by.x = "CPCCode Child" , by.y = "CPCCode", all.x = TRUE)
 setnames(tree,"Commodity","Child Commodity")
 setcolorder(tree,c("CPCCode Parent","Parent Commodity","CPCCode Child","Child Commodity","Year","Extraction Rate","Flag"))
 tree[is.na(`Extraction Rate`),`Extraction Rate`:=0]
 tree <- tree[order(`CPCCode Parent`)]
 value$data_tree <- tree
  
})

output$download_tree<- downloadHandler(
  filename = function() {
    "Commodity_tree.xlsx"
  },
  content = function(file) {
  data_download_tree <- data.table(value$data_tree)
  write.xlsx(data_download_tree ,file,row.names = FALSE)
  }
)

observeEvent(input$commodity_tree_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('commodity_tree'),
    cell_edit_info = input$commodity_tree_cell_edit,
    data = value$data_tree,
    session = session,
    table_name = "commodity_tree"
  )
})


observeEvent(input$treeSave,{
  t=as.numeric(as.numeric(input$fromyear) : as.numeric(input$endyear))
  treeOriginal <- fread_rds("SUA-FBS Balancing/Data/tree.rds")
  tree <- data.table(value$data_tree)
 tree[,c("Parent Commodity","Child Commodity") := NULL]
  setnames(tree, c("CPCCode Parent","CPCCode Child","Year"),c("measuredItemParentCPC","measuredItemChildCPC",
                                                              "timePointYears"))
  tree_to_save <- merge(treeOriginal,tree, by=c("measuredItemParentCPC","measuredItemChildCPC","timePointYears"), 
                        all.x = T )
  tree_to_save[measuredElementSuaFbs == "extractionRate" & timePointYears %in% t,
               `:=` (Value = `Extraction Rate`,
                     flagObservationStatus= Flag )]
  tree_to_save <- tree_to_save[,c("Extraction Rate","Flag") := NULL]
  saveRDS(tree_to_save,"SUA-FBS Balancing/Data/tree.rds")
})

#output
output$sua_unbalanced <- 
 renderDataTable(
  datatable (value$data_sua_unbalanced, rownames= FALSE,class = 'cell-border stripe',
               # editable = list(target = "cell", disable = list(columns = c(0,1,2,3))), 
               extensions = c("FixedColumns","FixedHeader","Buttons"),
               options = list(
                 pageLength = 25,
                 dom= 'Blfrtip', buttons = I('colvis'),
                 # paging = TRUE, searching = TRUE, info = FALSE,
                 # sort = TRUE,
                 scrollX = TRUE,
                 scrollY = "500px" ,
                 autoWidth = T,
                 fixedColumns = list(leftColumns = 4),
                 columnDefs = list(
                   # list(width = '200px', targets = c(3)),
                   list(visible = FALSE, targets = (ncol(value$data_sua_unbalanced)-1))) 
               ))  %>%
      formatStyle(0:ncol(value$data_sua_unbalanced), valueColumns = "hidden",
                  `border-bottom` = styleEqual(1, "solid 3px")) %>%
      formatStyle('ElementCode', target = "row", color = styleEqual(unique(value$data_sua_unbalanced$ElementCode),
                                                                    ifelse(unique(value$data_sua_unbalanced$ElementCode)=='5166','blue','black'))) %>%
      formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
)

output$commodity_tree <- 
  renderDataTable(
    datatable (value$data_tree, rownames= FALSE,class = 'cell-border stripe', 
               editable = list(target = "cell", disable = list(columns = c(1:4))),
               
               extensions = c("FixedHeader"),
               options = list(
                 pageLength = 25,
                 fixedHeader= T,
                 scrollX = TRUE,
                 scrollY = "500px" 
               ))%>%
      DT::formatStyle(columns = names(data), color="red")

    
  )



}
