seedRates <- function (input,output,session){
  observeEvent(input$undoSeedratio, {
    # get last version
    new_version <- Pop_table_version("seed_ratio")  
    # nothing to reset -- optionally a warning could be displayed in this case
    if(is.null(new_version)) {
      return()
    }
    value$data_seed_ratio <- new_version
})
  
observeEvent(input$startContinue,{
    seed_ratios<- fread_rds("Data/seedRate.rds")
    seed_ratios <- visualize_data(seed_ratios,input, session)
    seed_ratios[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)]
    value$data_seed_ratio <- seed_ratios
    Add_table_version("seed_ratio", copy( value$data_seed_ratio))  
})
  
observeEvent(input$add_Seed_ratio, {
    showModal(viewSeedRatio())
})
  
viewSeedRatio <- function(failed = FALSE) {
    modalDialog(
      easyClose = TRUE, size= "l",
      dataTableOutput("viewSeedRatio"),
      footer = tagList(
        actionButton("SeedRatioInsert", "Insert")
      )
    )
  }
output$viewSeedRatio= renderDataTable({
    commodity=copy(all_cpc)[order(CPCCode),]
    commodity[["Select"]]<-paste0('<input type="checkbox" name="row_selected" value="Row',1:nrow(commodity),'"><br>')
    commodity <- commodity[!CPCCode %in% unique(value$data_seed_ratio$CPCCode)]
    value$seedratioCommodities <- commodity
    datatable(value$seedratioCommodities,escape=F)
})
  
observeEvent(input$SeedRatioInsert, {
    removeModal()
})
  
observeEvent(input$seed_ratio_cell_edit, {
  handle_cell_edit(
    proxy = dataTableProxy('seed'),
    cell_edit_info = input$seed_ratio_cell_edit,
    data = value$data_seed_ratio,
    session = session,
    table_name = "seed_ratio"
  )
})

observeEvent(input$SeedRatioInsert, {
  handleInsertEvent(
    input_rows = input$viewSeedRatio_rows_selected,
    element_codes = c("R5525"),
    value_store = value,
    data_type = "seed_ratio"
  )   
})
  
observeEvent(input$delete_btn_seed_ratio, {
    t = copy(value$data_seed_ratio)
    if (!is.null(input$seed_ratio_rows_selected)) {
      t <- t[-as.numeric(input$seed_ratio_rows_selected),]
    }
    value$data_seed_ratio<- t
    Add_table_version("seed_ratio", copy( value$data_seed_ratio))
})
  
#download excle file 
output$downloadSeedRatio <- createDownloadHandler(reactive(value$data_seed_ratio),"seedRates_data.xlsx")

upload_denormalized_data(input= input, 
                         session = session, 
                         all_elements=all_elements, 
                         all_cpc = all_cpc, 
                         file_input = "fileSeedRatiodenormalized", 
                         data_key = "data_seed_ratio", 
                         version_label =  "seed_ratio",
                         value_env = value)

observeEvent(input$uploadSeedRatioModal, {
    showModal(uploadSeedRatio())
  })
  
uploadSeedRatio <- function(failed = FALSE) {
    modalDialog(size = "l",
         titlePanel("Upload File"),
         sidebarLayout(
           sidebarPanel(
               fileInput("fileSeedRatio", "Choose Excel File",
                              multiple = TRUE,
                              accept = NULL),
           selectizeInput("cpcSeedratio", "CPC Code",
                            selected = NULL, choices = 
                               c("",colnames( value$data_seed_ratioCountry)),multiple=F),
           selectizeInput("elementSeedratio", "Element Code",
                            selected = NULL, choices = 
                                 c("",colnames( value$data_seed_ratioCountry)) ,multiple=F),
           selectizeInput("yearSeedratio", "Year :",
                           selected = NULL, choices =
                              c("",colnames( value$data_seed_ratioCountry)),multiple=F),
           selectizeInput("valueSeedratio", "Value :",
                           selected = NULL, choices = 
                               c("",colnames( value$data_seed_ratioCountry)),multiple=F),
           selectizeInput("flagSeedratio", "Flag :",
                           selected = NULL, choices = 
                               c("",colnames( value$data_seed_ratioCountry)),multiple=F),
            actionButton("uploadSeedratio","Upload Seed ratio")),
                  mainPanel(
                    div(style = 'overflow-x: scroll', dataTableOutput('SeedratioCountry')))
                ) )}
  
output$SeedratioCountry <- renderDataTable({
    req(input$fileSeedRatio)
    inFile <- input$fileSeedRatio
    file.rename(inFile$datapath,
                paste(inFile$datapath, ".xlsx", sep=""))
    DATA=read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
    value$data_seed_ratioCountry <- DATA
    datatable(value$data_seed_ratioCountry, list(lengthMenu = c(5, 30, 50), pageLength = 5))
 })
  
 observe({
    updateSelectInput(session, "cpcSeedratio", choices = c("", colnames( value$data_seed_ratioCountry)))
    updateSelectInput(session, "elementSeedratio", choices = c("",colnames( value$data_seed_ratioCountry)))
    updateSelectInput(session, "yearSeedratio", choices = c("",colnames( value$data_seed_ratioCountry)))
    updateSelectInput(session, "valueSeedratio", choices = c("",colnames( value$data_seed_ratioCountry)))
    updateSelectInput(session, "flagSeedratio", choices = c("",colnames( value$data_seed_ratioCountry)))
  })
  
 observeEvent(input$uploadSeedratio, {
    removeModal()
 })
  
 observeEvent(input$uploadSeedratio, {
   normalized_upload(
     data_source = "data_seed_ratioCountry",
     input_cols = c("cpcSeedratio", "elementSeedratio", "yearSeedratio", "valueSeedratio", "flagSeedratio"),
     std_names = c("CPCCode", "ElementCode", "Year", "Value", "Flag"),
     filter_element_code = c("R5525"),
     from_year = input$fromyear,
     to_year = input$endyear,
     existing_data = "data_seed_ratio",
     all_elements = all_elements,
     all_cpc = all_cpc,
     value_env = value,
     value_name = "data_seed_ratio",
     version_label = "seed_ratio",
     session = session
   )
 })
  
 observeEvent(input$saveSeedratio,{
    data_to_save <- value$data_seed_ratio
    saveRDS(data_to_save,"Data/seedRate.rds")
    
  })
  
observeEvent(input$seed_imputation_ratio,{
    data <- imputationSeedRates(input,output,session)
    value$data_seed_ratio <- data
     if ( !is.null(data) ){
      sendSweetAlert(
        session = session,
        title = c("Imputation Error"),
        text = c("No time series data to impute"),
        type = "success"
      )
  }
  Add_table_version("seed_ratio", copy( value$data_seed_ratio))  
 })
  
 output$seed_ratio <-
    renderDataTable({
     datatable (value$data_seed_ratio, rownames= FALSE,class = 'cell-border stripe',
                 editable = list(target = "cell", disable = list(columns = c(0:as.numeric(which( colnames(value$data_seed_ratio) 
                                                                                                 == input$fromyear)-2)))),
                 extensions = c("FixedColumns","FixedHeader","Buttons"),
                 options = list(
                  scrollX = TRUE,
                   dom= 'Blfrtip', buttons = I('colvis'),
                   scrollY = "500px" ,
                   autoWidth = T,
                   fixedColumns = list(leftColumns = 4),
                   columnDefs = list(list(width = '', targets =  c(6)),
                                list(visible = FALSE, targets = (ncol(value$data_seed_ratio)-1))
                   )
                 ))%>%
        formatCurrency(columns = as.character(c(2010:input$endyear)),currency = "", digits = 0,interval = 3, mark = ",")
    } )
 }