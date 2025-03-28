js <- HTML(
  "function colorizeCell(i, j,id){
  var selector = 'tr:nth-child(' + i + ') td:nth-child(' + j + ')';
  $(id).find(selector).addClass('yellow');
  }"
)

ui <-
 dashboardPage(
    dashboardHeader( titleWidth = 600, title =  span(img(src="LogoFAOSmall.png", width = 50),paste("Food Balance Sheet (FBS) Compiler,"),
                                                     tags$a(href="javascript:history.go(0)", 
                                                            popify(tags$i(class="fa fa-refresh fa-1x"),
                                                                   title = "Reload", 
                                                                   content = "Click here to restart the Shiny session",
                                                                   placement = "right")))
   ),
    dashboardSidebar(
      
      sidebarMenu( id= "fao", 
                   #menuItem("Country", tabName = "Country",icon =icon("hourglass-start")) ,          
                   menuItem("Start", tabName = "Start",icon =icon("hourglass-start")),
                   menuItem("Production", tabName = "production",icon = icon("fa-solid fa-tractor"))
                   )),                                        
      dashboardBody(
      tags$head(
        tags$link(
          rel = "stylesheet", 
          type = "text/css", 
          href = "stylesheet.css")
      ),
      tags$script(src = "javascript.js"),
      useShinyjs(),
      tabItems (
        tabItem(tabName = "Start",
                fluidRow(
                  box(title = "Select Country and Year Range", width = 12,  status = "primary", 
                      solidHeader = TRUE, collapsible = TRUE,
                      selectInput(inputId="countrym49", label="Country",width = "400px", choices = c("",country_selc)),
                      textInput(inputId="fromyear", label="From",width = "400px"),
                      textInput(inputId="endyear", label="To",width = "400px")
                     , br(), 
                      
                      column(3,
                         div(style="display:inline-block",
                         actionGroupButtons(
                                   inputIds =c("startContinue"),
                                   labels=list(tags$span(icon("success"), "Start Compilation")),
                                   status = "success", direction = "vertical"
                                 )
                             )),    
                      
               br(), br(),br()
              )
                )),
            tabItem(tabName = "production",
                fluidRow(
                  box(title = a("Crop", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=60"), width = 12,  status = "primary", br(),
                      solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE,
                      div(style="display:inline-block",
                          div(style="display:inline-block",
                          actionGroupButtons(
                                inputIds =c("add_Crop"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),
                      div(style="display:inline-block",
                      actionGroupButtons(
                      inputIds =c("delete_btn_crop"),
                      labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                      status = "danger", direction = "vertical"
                          )
                      ),
                      
                      div(style="display:inline-block",
                        dropdownButton(label = "Upload/Download", #fullwidth=TRUE,
                         actionGroupButtons(
                         inputIds = c("uploadCropModal"),
                         labels = list(tags$span(icon("plus"), "Upload Crop Data (Normalized)")),
                         status = "primary", direction = "vertical"
                                         ), br(),
                        fileInput("fileCropdenormalized", "Choose Crop Excel File", multiple = TRUE, accept = c(".xlsx"))
                        , br(),
                        tags$script('$( "#fileCrop" ).on( "click", function() { this.value = null; });'),
                        downloadButton("downloadCrop", "Download Excel file", class= "btn-block" ),
                        tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                        circle = FALSE, status = "success", icon = icon("file-excel-o"),
                        tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),
                      div(style="display:inline-block",
                         actionGroupButtons(
                            inputIds =c("undoCrop"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),
                       div(style="display:inline-block",
                         actionGroupButtons(
                            inputIds =c("saveCrop"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ), br(),br(), 
                      DT::dataTableOutput("crop",height = "auto"),
                      
                     useShinyjs(),
                      tags$head(
                        tags$style(css),
                        tags$script(js))
            ),
             box(title = a("Livestock", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=65"), width = 12,  status = "primary", 
                      solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,br(),
                      div(style="display:inline-block",
                          div(style="display:inline-block",
                          actionGroupButtons(
                                inputIds =c("add_Livestock"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),
                     div(style="display:inline-block",
                        actionGroupButtons(
                            inputIds =c("delete_btn_livestock"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),
                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,
                           actionGroupButtons(
                           inputIds = c("uploadLivestockModal"),
                           labels = list(tags$span(icon("plus"), "Upload Livestock Data (Normalized)")),
                            status = "primary", direction = "vertical"), br(),
                              fileInput("fileLivestockdenormalized", "Choose Livestock Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx")),
                                       # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileLivestock" ).on( "click", function() { this.value = null; });'),
                                         downloadButton("downloadLivestock", "Download Excel file"),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),
                     div(style="display:inline-block",
                        actionGroupButtons(
                            inputIds =c("undoLivestock"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),
                      div(style="display:inline-block",
                         actionGroupButtons(
                            inputIds =c("saveLivestock"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical")), br(),br(),
                      DT::dataTableOutput("livestock",height = "auto"))
                )
         )
      )#tabItems
    )#dashboardBody
  )#dashboard Page







