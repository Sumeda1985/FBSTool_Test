reference_tables = function(input,output,session) {
flags_reactive <-reactive({
 flags= fread_rds("Data/Flags.rds")
flags[is.na(Flag), Flag := ""]
 flags[, Description := ifelse(Flag %in% c("","T","E"), "Protected", "Non-Protected")]
 return(flags)
})
  
output$flags=renderDataTable(
 datatable(
     flags_reactive(),
      rownames = FALSE,
      extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
        pageLength = 20,fixedHeader = FALSE,
        orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
        lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
        dom = 'Bfrtip',
        buttons =
        #colvis have the option to visualize columns
          list('pageLength', list(
            extend = 'excel',
           pageSize = 'A4',
            orientation = 'landscape',
            filename = 'Flags',
            title = "Flags"
         ))
     )),
server = FALSE
)
  
suaCommodities_reactive <- reactive({
suacommodityName <- copy(all_cpc)
suacommodityName=suacommodityName[order(CPCCode)]
return(suacommodityName)
})
  
output$suaCommodities = renderDataTable(
    datatable(
      suaCommodities_reactive(),
      rownames = FALSE,
      extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
        pageLength = 20,fixedHeader = FALSE,
        orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
        lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
        dom = 'Bfrtip',
        buttons =
          list('pageLength', list(
            extend = 'excel',
            pageSize = 'A4',
            orientation = 'landscape',
            filename = 'SUA Commodities',
            title = "SUA Commodities"
          ))
      )),
    server = FALSE
)  
fbsCommo_reactive <- reactive({
    fbscommodityName = fread_rds("Data/FBS_Commodities.rds")
    fbscommodityName=fbscommodityName[order(FBSCode)]
    return(fbscommodityName)
})
  
output$fbsCommodities = renderDataTable(
    datatable(
      fbsCommo_reactive(),
      rownames = FALSE,
      extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
        pageLength = 20,fixedHeader = FALSE,
        orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
        lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
        dom = 'Bfrtip',
        buttons =
        #colvis have the option to visualize columns
          list('pageLength', list(
            extend = 'excel',
             pageSize = 'A4',
            orientation = 'landscape',
            filename = 'FBS Commodities',
            title = "FBS Commodities"
          ))
      )),
   server = FALSE
)  
  
tradeCommodities_reacitve <- reactive({
    tradecommodityName = fread_rds("Data/Trade_Commodities.rds")
    commodityName = fread_rds("Data/SUA_Commodities.rds") 
    commodityName = data.table(commodityName)
    tradecommodityName=merge(tradecommodityName,commodityName, by=c("CPCCode","Commodity"),all.x = T)
    tradecommodityName=tradecommodityName[order(CPCCode)]
    # setnames(tradecommodityName,"HS6 standard code","HS6")
    tradecommodityName <- tradecommodityName[!is.na(HS6)]
    setcolorder(tradecommodityName,c("HS6","CPCCode","Commodity"))
    return(tradecommodityName)
})
  
output$tradeCommodities = renderDataTable( 
    datatable(
      tradeCommodities_reacitve(),
      rownames = FALSE,
      extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
        pageLength = 20,fixedHeader = FALSE,
        orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
        lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
        dom = 'Bfrtip',
        buttons =
          #colvis have the option to visualize columns
          list('pageLength', list(
            extend = 'excel',
            pageSize = 'A4',
            orientation = 'landscape',
            filename = 'Trade Commodities',
            title = "Trade Commodities"
          ))
)),
    server = FALSE
)   
  
element_reactive <- reactive({
  elementName = fread_rds("Data/Elements_All.rds")
  elementName <- elementName[ElementCode %in% all_element_codes]
  elementName=elementName[order(ElementCode)]
  return(elementName)
})  
  
output$elements=renderDataTable(
   datatable(
     element_reactive(),
     rownames = FALSE,
     extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
       pageLength = 20,fixedHeader = FALSE,
       orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
       lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
       dom = 'Bfrtip',
       buttons =
         #colvis have the option to visualize columns
         list('pageLength', list(
           extend = 'excel',
           
           pageSize = 'A4',
           orientation = 'landscape',
           filename = 'Element Names',
           title = "Element Names"
           
         ))
       
     )),
   server = FALSE
) 
 
fbsTree_reactive=reactive({
  data=fread_rds("SUA-FBS Balancing/Data/fbsTree.rds")
  fbsName= fread_rds("Data/FBS_Commodities.rds")
  id_vars <- c("id1", "id2", "id3", "id4")
  data[, (id_vars) := lapply(.SD, as.character), .SDcols = id_vars]
  data=merge(data,fbsName, by.x = "id1",by.y = "FBSCode",all.x = T)
  setnames(data,"Commodity","Grand Total")
  data=merge(data,fbsName, by.x = "id2",by.y = "FBSCode",all.x = T)
  setnames(data,"Commodity","FBS Catergory")
  data=merge(data,fbsName, by.x = "id3",by.y = "FBSCode",all.x = T)
  setnames(data,"Commodity","FBS Macro Aggregate")
  data=merge(data,fbsName, by.x = "id4",by.y = "FBSCode",all.x = T)
  setnames(data,"Commodity","FBS Group")
  setnames(data,c("id1","id2","id3","id4","item_sua_fbs"),c("Grand Total Code","FBS Catergory Code","FBS Macro Aggrgate Code","FBS Group Code","CPCCode"))
  data=merge(data,all_cpc,by="CPCCode",all.x = TRUE)
  setcolorder(data,c("Grand Total Code","Grand Total",
                     "FBS Catergory Code","FBS Catergory","FBS Macro Aggrgate Code","FBS Macro Aggregate",
                     "FBS Group Code","FBS Group","CPCCode","Commodity"))
  return(data)
})
 
output$fbsTree=renderDataTable(
    datatable(
     fbsTree_reactive(),
     rownames = FALSE,
     extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
       pageLength = 20,fixedHeader = FALSE,
       orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
       lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
       dom = 'Bfrtip',
       buttons =
          #colvis have the option to visualize columns
         list('pageLength', list(
           extend = 'excel',
           pageSize = 'A4',
           orientation = 'landscape',
           filename = 'Tree',
           title = "Commodity Tree"
         ))
     )),
   server = FALSE
) 
   
nutrient_elements <- reactive({
  nutrientElements = fread_rds("Data/Nutrient Elements.rds")
})
 
output$nutrient_elements= renderDataTable(
  datatable(
    nutrient_elements(),
    rownames = FALSE,
    extensions = c('Responsive', 'Buttons','RowReorder','KeyTable', 'FixedHeader'), options = list(
      pageLength = 20,fixedHeader = FALSE,
      orientation ='landscape', keys= TRUE, fixedHeader = TRUE,
      lengthMenu = list(c(6, 12, 18, -1), c('6', '12', '18', 'All')),
      dom = 'Bfrtip',
      buttons =
       #colvis have the option to visualize columns
        list('pageLength', list(
          extend = 'excel',
          pageSize = 'A4',
          orientation = 'landscape',
          filename = 'Tree',
          title = "Commodity Tree"
        ))
   )),
  server = FALSE
)
}