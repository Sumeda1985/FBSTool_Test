js <- HTML(
  "function colorizeCell(i, j,id){
  var selector = 'tr:nth-child(' + i + ') td:nth-child(' + j + ')';
  $(id).find(selector).addClass('yellow');
  }"
)
ui <-
dashboardPage(
    dashboardHeader( titleWidth = 600,
    title =  span(img(src="LogoFAOSmall.png", width = 50),paste("Food Balance Sheet (FBS) Compiler"),
    tags$a(href="javascript:history.go(0)",
    popify(tags$i(class="fa fa-refresh fa-1x"),
    title = "Reload",
    content = "Click here to restart the Shiny session",
    placement = "right")))
    ),
    dashboardSidebar(
      sidebarMenu( id= "fao",
         menuItem("Start", tabName = "Start",icon =icon("hourglass-start")),
         menuItem("Production", tabName = "production",icon = icon("tractor")),
         menuItem("Trade", tabName = "Trade",icon = icon("globe")),
         menuItem("Stock", tabName = "Stock",icon = icon("th")),
         menuItem("Loss", tabName = "Loss",icon = icon("trash")),
         menuItem("Feed", tabName = "Feed",icon = icon("hippo")),
         menuItem("Seed", tabName = "Seed",icon = icon("seedling")),
         menuItem("Industrial Non-Food Use", tabName = "Industry",icon = icon("industry")),
         menuItem("Food Availability", tabName = "Food",icon = icon("th")),
         menuItem("SUA", icon = icon("book"),
                menuSubItem("SUA Unbalanced", tabName = "sua_unbalanced",icon = icon("book")),
                menuSubItem("SUA Balanced", tabName = "sua_balanced",icon = icon("balance-scale"))),
         menuItem("Fish, Seafood", tabName = "fish",icon = icon("fa-thin fa-fish")),
         menuItem("FBS",tabName = "fbs_tab", icon = icon("th-list"),
         menuSubItem("FBS", tabName = "fbs", icon = icon("th-list")),
         menuSubItem("FBS Report", tabName = "fbs_report",icon = icon("th-list"))),
         menuItem("Nutrients",tabName = "nutrient_tab", icon = icon("th-list"),
                 menuSubItem("Nutrients by FBS food group", tabName = "total_des",icon = icon("th-list")),
                 menuSubItem("Nutrients by GIFT food group", tabName = "gift",icon = icon("th-list"))),
         menuItem("Indicators", tabName = "indicators",icon = icon("th-list")),
         menuItem("Reference Tables", tabName = "reference",icon = icon("th-list"))
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
      tabItems (tabItem(tabName = "Start",
                        fluidRow(
                            box(title = "Enter your email address and authentication key",
                                width = 12,
                                status = "primary",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                textInput(inputId="useremail",
                                          label="User Email",
                                          width = "400px"),
                                textInput(inputId="authkey",
                                          label="Authentication Key",
                                          width = "400px"),
                                actionButton(inputId="authenticate",
                                             label="Authenticate"),
                                br()),
                            uiOutput("selectcountryyear"),
                            ## box(title = "Select Country and Year Range",
                            ##     width = 12,
                            ##     status = "primary",
                            ##     solidHeader = TRUE,
                            ##     collapsible = TRUE,
                            ##     selectInput(
                            ##         inputId = "countrym49",
                            ##         label = "Country",
                            ##         width = "400px",
                            ##         choices = c("", country_selc)
                            ##     ),
                            ##     textInput(inputId="fromyear", label="From",width = "400px"),
                            ##     textInput(inputId="endyear", label="To",width = "400px")
                            ##   , br(),
                            ##     column(3,
                            ##            div(style="display:inline-block",
                            ##                actionGroupButtons(
                            ##                    inputIds =c("startContinue"),
                            ##                    labels=list(tags$span(icon("success"), "Start Compilation")),
                            ##                    status = "success", direction = "vertical"
                            ##                )
                            ##                )),br(), br(),br()
                            ##     )
                        )),
    tabItem(tabName = "reference",
       box(title = "Flags", width = 12,  status = "primary", collapsed = TRUE,
       solidHeader = TRUE, collapsible = TRUE,
       # tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)
       dataTableOutput("flags")
           ),
       box(title = "SUA Commodities", width = 12,  status = "primary", collapsed = TRUE,
       solidHeader = TRUE, collapsible = TRUE,
       # tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)
       dataTableOutput("suaCommodities")
          ),
       box(title = "FBS Commodities", width = 12,  status = "primary", collapsed = TRUE,
       solidHeader = TRUE, collapsible = TRUE,
       # tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)
       dataTableOutput("fbsCommodities")
        ),
       box(title = "Trade Commodities", width = 12,  status = "primary", collapsed = TRUE,
       solidHeader = TRUE, collapsible = TRUE,
       # tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)
       dataTableOutput("tradeCommodities")
                ),
       box(title = "Elements", width = 12,  status = "primary", collapsed = TRUE,
       solidHeader = TRUE, collapsible = TRUE,
       # tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)
        dataTableOutput("elements")
         ),
       box(title = "FBS Group", width = 12,  status = "primary", collapsed = TRUE,
       solidHeader = TRUE, collapsible = TRUE,
       # tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)
       dataTableOutput("fbsTree")
       ),
       box(title = "Nutrient Elements", width = 12,  status = "primary", collapsed = TRUE,
       solidHeader = TRUE, collapsible = TRUE,
       # tags$style(".skin-blue .sidebar a { color: #444; }"), # higlting tabs)
       dataTableOutput("nutrient_elements")
             )),
       tabItem(tabName = "fbs",
        box(title = "FBS", width = 12, height = 100,  status = "primary", collapsed = TRUE,
        solidHeader = TRUE, collapsible = TRUE, downloadButton("download_fbs_balanced", "Download", class= "butt1"),
        div(style="display:inline-block",dropdownButton(label = "FBS Report", #fullwidth=TRUE,
        textInput(inputId="fbs_report_yr", label="Year : ",value = "",width = "250px"),
        actionGroupButtons(inputIds = c("fbs_report_button"),labels = list(tags$span(icon("plus"), "Get FBS Report")),
                            status = "primary", direction = "vertical"), br(), br(),
                   circle = FALSE, status = "success", icon = icon("file-excel-o"),
                   tooltip = tooltipOptions(title = "FBS Report", placement = "top"))),
                  div(style="display:inline-block",
                        actionGroupButtons(
                          inputIds = c("total_DES"),
                          labels = list(tags$span(icon("magic"),"Total DES & Nutrients")),
                          status = "info", direction = "vertical"
                        )
                     ), br(),br(), dataTableOutput("fbs_balanced") )
        ),
        tabItem(tabName = "fbs_report",
           box(title = "FBS Report", width = 12, height = 50,  status = "primary", collapsed = TRUE, solidHeader = TRUE,
               collapsible = TRUE, br(),br(),dataTableOutput("fbs_report") )
        ),
        tabItem(tabName = "sua_unbalanced",
          fluidRow(
            box(title = "SUA Unbalanced", width = 12,  status = "primary", collapsed = TRUE,
                solidHeader = TRUE, collapsible = TRUE,
                tags$style(".skin-blue .sidebar a { color: #444; }"),br(),
                downloadButton("download_sua_unbalanced", "Download", class= "butt1"),
                div(style="display:inline-block",
                actionGroupButtons(inputIds = c("all_imp"),
                    labels = list(tags$span(icon("magic"), "Run All Imputation")),
                                             status = "primary", direction = "vertical")),
                div(style="display:inline-block",
                 actionGroupButtons(inputIds =c("runPlugin"),
                            labels=list(tags$span(icon("magic"), "Run Balancing Plugin")),
                            status = "success", direction = "vertical"
                          )
                      ),
                div(style="display:inline-block",
                actionGroupButtons(inputIds =c("gotosuaBalanced"),
                            labels=list(tags$span(icon("arrow-alt-circle-right"), "SUA Balanced")),
                            status = "primary", direction = "vertical"
                          )
                      ), br(), br(),br(),DT::dataTableOutput("sua_unbalanced",height = "auto"))
                ),
          fluidRow(
               box(title = "Commodity Tree", background = "aqua", width = 12, collapsed = TRUE,
                   solidHeader = TRUE, collapsible = TRUE, br(),
               div(style="display:inline-block",downloadButton("download_tree", "Download", class= "butt1"),
               actionGroupButtons(inputIds = c("treeSave"),
                            labels = list(tags$span(icon("save"), "Save Modifications to the tree")),
                            status = "warning", direction = "vertical")),br(),br(),br(),
               DT::dataTableOutput("commodity_tree",height = "auto")))
        ),
       tabItem(tabName = "sua_balanced",
          fluidRow(
           box(title = "SUA Balanced", width = 12, height = 100,  status = "primary",solidHeader = TRUE,
               collapsible = TRUE,
           fluidRow(column(width = 4,downloadButton("download_sua_balanced", "Download", class= "butt1")),
           div(style="display:inline-block",
           actionGroupButtons(inputIds = c("fbs_balanced_button"),
                              labels = list(tags$span(icon("magic"),"FBS Standardization")),
                               status = "info", direction = "vertical" )),br(), br(),
          checkboxInput('checkbox','Visualize Nutrients',value = FALSE)),br(), br(),
          DT::dataTableOutput("sua_balanced",height = "auto"))),
          fluidRow(
            box(title = "Nutrient Factors", background = "aqua", width = 12, collapsed = TRUE,solidHeader = TRUE, collapsible = TRUE,
                br(),
            div(style="display:inline-block",
            actionGroupButtons(inputIds = c("nutrientSave"),
                                labels = list(tags$span(icon("save"), "Save Modifications to nutrient factors")),
                                 status = "warning", direction = "vertical")),
            column(width = 2, downloadButton("download_nutri_table", "Download", class= "butt1")),br(), br(),br(),
            DT::dataTableOutput("nutrient_factors",height = "auto")))
          ),
       tabItem(tabName = "total_des",
          box(title = "Nutrients by FBS food group", width = 12,  status = "primary",
              solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
              fluidRow(column(width = 5,
                       downloadButton("download_total_des_level", "Download nutrients by level", class= "butt1"),
                       downloadButton("download_total_des_year", "Download nutrients at SUA by year", class= "butt1")
                      )),br(),
              div(h3("Table settings", style = 'color: green; text-decoration:underline;'),br(),
               radioButtons('select_des_data_filter',
                             label = 'Filter data by: ',choices = c('Year', 'Nutrient'),inline = TRUE),
               selectInput('select_des_data_year',label = 'Year: ',choices = c(''),width = "40%"),
               selectInput('select_des_data_nutrient',label = 'Nutrient: ',choices = c(''), width = "40%"),
              style = "border-left: 2px solid green; padding-left:20px;"),
              br(), h4("*Nutrient Values are in capita/day",style = 'color: red; text-decoration:underline;'), br(),
              DT::dataTableOutput("total_des",height = "auto"))
      ),
     tabItem(tabName = "indicators",
            tabBox(width = 12,height = 830,
            tabPanel("Visualization",icon = icon("fas fa-chart-bar"),
            box(title = "Settings", width = 6,status = "success",solidHeader = TRUE, height = 360,
                radioButtons('select_ssr_level',label = 'Compute for ',choices = c('Commodity', 'FBS group'),
                             inline = TRUE),
                selectInput('select_ssr_commodity',label = 'Commodity: ',choices = c('Loading...'),
                             width = '100%'),
                selectInput('select_ssr_fbs_group',label = 'FBS group: ',choices = c('Loading...'),
                             width = '100%'),
                checkboxInput('check_ssr_show_ratio',label = 'Display ratios',value = TRUE)
               ),
             box(title = "Self-sufficiency ratio",width = 6,status = "primary", solidHeader = TRUE,
                  height = 360,plotOutput('ssr_plot', height = 300),
             div(style = "position: absolute; left:1em; bottom: 1em; width:90%", class = "settings_row",
             downloadButton("download_ssr_plot",icon = icon('download'),label = "",
                            class = "btn-default btn-xs btn-download"))),
             box(title = "Import dependency ratio",width = 6, status = "primary",solidHeader = TRUE,
                 height = 360,plotOutput('idr_plot', height = 300),
                  div(style = "position: absolute; left:1em; bottom: 1em; width:90%",
                  class = "settings_row",
                   downloadButton("download_idr_plot", icon = icon('download'),label = "",class = "btn-default btn-xs btn-download")
                      )
                   ),
              box(title = "SSR/IDR",width = 6,status = "primary",solidHeader = TRUE,height = 360,
                  plotOutput('ssr_idr_plot', height = 300),
              div(style = "position: absolute; left:1em; bottom: 1em; width:90%",
                  class = "settings_row",
                  downloadButton("download_ssr_idr_plot",icon = icon('download'),label = "",class = "btn-default btn-xs btn-download")
                    )
                  )
                ),
    tabPanel("Data self-sufficiency ratio", icon = icon("far fa-table"),
              fluidRow(column(width = 4,downloadButton("download_table_ssr", "Download", class= "butt1"))
                      ),br(), br(),br(),
               DT::dataTableOutput("table_ssr",height = "auto")
            ),
    tabPanel("Data import dependency ratio",icon = icon("far fa-table"),
        fluidRow(column(width = 4, downloadButton("download_table_idr", "Download", class= "butt1"))), br(), br(),br(),
         DT::dataTableOutput("table_idr",height = "auto")
            ))),
    tabItem(tabName = "gift",
         box(title = "Nutrients by GIFT food group", width = 12, height = 100, status = "primary",
              solidHeader = TRUE, collapsible = TRUE,
         fluidRow(column(width = 4,downloadButton("download_gift", "Download", class= "butt1"))),br(), br(),br(),
         DT::dataTableOutput("gift_tab",height = "auto"))
        ),
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


                                         fileInput("fileCropdenormalized", "Choose Crop Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
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
                                           status = "primary", direction = "vertical"
                                         ), br(),


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
                            status = "warning", direction = "vertical"
                          )
                      ), br(),br(),




                      DT::dataTableOutput("livestock",height = "auto"))
                )

        ),


        #stocks




        tabItem(tabName = "Stock",
                fluidRow(
                  box(title = a("Stock Change", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=70"), width = 12,  status = "primary", br(),
                      solidHeader = TRUE, collapsible = TRUE,h4("*Please do not manually enter opening stocks."),
                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Stock"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_Stock"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadStockModal"),
                                           labels = list(tags$span(icon("plus"), "Upload Stock Data (Normalized)")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileStockdenormalized", "Choose Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileStock" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadStock", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),


                      # div(style="display:inline-block",
                      #
                      #
                      #     actionGroupButtons(inputIds = c("open_stock_computation"),
                      #                        labels = list(tags$span(icon("magic"), "Computation of Opening Stock")
                      #                        ),
                      #                        status = "info", direction = "vertical")),

                      div(style="display:inline-block",


                          actionGroupButtons(inputIds = c("stock_imputation"),
                                             labels = list(tags$span(icon("magic"), "Imputation of Stocks")
                                             ),
                                             status = "primary", direction = "vertical")),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoStock"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveStock"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ), br(),br(),

                      DT::dataTableOutput("stock",height = "auto"))




                )

        ),

        #new

        tabItem(tabName = "fish",
                fluidRow(
                  box(title = a("Fish, Seafood", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=70"), width = 12,  status = "primary", br(),
                      solidHeader = TRUE, collapsible = TRUE,



                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoFish"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveFish"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ), br(),br(),

                      DT::dataTableOutput("fish",height = "auto"))




                )

        ),
     tabItem(tabName = "Loss",
                fluidRow(
                  box(title = "Loss Ratios", width = 12,  status = "primary",
                      solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE
                      , br(),

                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Loss_ratio"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),



                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_loss_ratio"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadlossRatioModal"),
                                           labels = list(tags$span(icon("plus"), "Upload Loss Ratio Data (Normalized)")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileLossRatiodenormalized", "Choose Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileLossRatio" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadLossRatio", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(inputIds = c("loss_imputation_ratio"),
                                             labels = list(tags$span(icon("magic"), "Imputation of Loss Ratios")
                                             ),
                                             status = "primary", direction = "vertical")),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoLossratio"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveLossratio"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),



                      br(), br(),br(),


                      DT::dataTableOutput("loss_ratio",height = "auto"))
                ),



                fluidRow(
                  box(title = a("Loss", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=91"), width = 12,  status = "primary",
                      solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE
                      , br(),

                      br(), br(),
                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Loss"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_loss"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadLossdModal"),
                                           labels = list(tags$span(icon("plus"), "Upload Loss Data (Normalized)")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileLossdenormalized", "Choose Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileLoss" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadLoss", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(inputIds = c("loss_imputation"),
                                             labels = list(tags$span(icon("magic"), "Imputation of Loss")
                                             ),
                                             status = "primary", direction = "vertical")),
                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoLoss"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveLoss"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),br(),br(),
                      DT::dataTableOutput("loss",height = "auto"))
                )),
        tabItem(tabName = "Feed",
                fluidRow(
                  box(title = "Feed Ratios", width = 12,  status = "primary",
                      solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE
                      , br(),

                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Feed_ratio"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_feed_ratio"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadFeedRatioModal"),
                                           labels = list(tags$span(icon("plus"), "Upload Feed Ratio Data (Normalized)")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileFeedRatiodenormalized", "Choose Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileFeedRatio" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadFeedRatio", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(inputIds = c("feed_imputation_ratio"),
                                             labels = list(tags$span(icon("magic"), "Imputation of Feed Ratios")
                                             ),
                                             status = "primary", direction = "vertical")),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoFeedratio"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveFeedratio"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),



                      br(), br(),br(),


                      DT::dataTableOutput("feed_ratio",height = "auto"))
                ),



                fluidRow(
                  box(title = a("Feed", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=79"), width = 12,  status = "primary",
                      solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE
                      , br(),
                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Feed"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_feed"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadFeedModal"),
                                           labels = list(tags$span(icon("plus"), "Upload Feed Data (Normalized)")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileFeeddenormalized", "Choose Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileFeed" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadFeed", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(inputIds = c("feed_imputation"),
                                             labels = list(tags$span(icon("magic"), "Imputation of Feed")
                                             ),
                                             status = "primary", direction = "vertical")),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoFeed"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveFeed"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),br(),br(),
                      DT::dataTableOutput("feed",height = "auto"))
                )),

        tabItem(tabName = "Seed",
                fluidRow(
                  box(title = "Seed Rates", width = 12,  status = "primary",
                      solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE
                      , br(),

                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Seed_ratio"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_seed_ratio"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadSeedRatioModal"),
                                           labels = list(tags$span(icon("plus"), "Upload Seed Ratio Data (Normalized)")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileSeedRatiodenormalized", "Choose Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileSeedRatio" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadSeedRatio", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(inputIds = c("seed_imputation_ratio"),
                                             labels = list(tags$span(icon("magic"), "Imputation of Seed Rates")
                                             ),
                                             status = "primary", direction = "vertical")),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoSeedratio"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveSeedratio"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),



                      br(), br(),br(),


                      DT::dataTableOutput("seed_ratio",height = "auto"))
                ),



                fluidRow(
                  box(title = a("Seed", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=83"), width = 12,  status = "primary",
                      solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE
                      , br(),


                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Seed"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_seed"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadSeedModal"),
                                           labels = list(tags$span(icon("plus"), "Upload Seed Data (Normalized)")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileSeeddenormalized", "Choose Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileSeed" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadSeed", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(inputIds = c("seed_imputation"),
                                             labels = list(tags$span(icon("magic"), "Imputation of Seed")
                                             ),
                                             status = "primary", direction = "vertical")),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoSeed"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),




                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveSeed"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),br(),br(),
                      DT::dataTableOutput("seed",height = "auto"))
                )),



        tabItem(tabName = "Industry",
                fluidRow(
                  box(title = a("Industrial Non-Food Use", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=89"), width = 12,  status = "primary",
                      solidHeader = TRUE, collapsible = TRUE
                      , br(),

                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Industry"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_industry"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadIndustryModal"),
                                           labels = list(tags$span(icon("plus"), "Upload Industrial Use Data (Normalized)")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileIndustrydenormalized", "Choose Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileIndustry" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadIndustry", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoIndustry"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveIndustry"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),



                      br(), br(),br(),


                      DT::dataTableOutput("industry",height = "auto"))
                )),

        tabItem(tabName = "Food",
                fluidRow(
                  box(title = "GDP per Capita", width = 12,  status = "primary", collapsed = TRUE,
                      solidHeader = TRUE, collapsible = TRUE
                      , br(),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoGDP"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveGDP"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),



                      br(), br(),br(),


                      rHandsontableOutput("gdp"))
                ),


                fluidRow(
                  box(title = "Population", width = 12,  status = "primary", collapsed = TRUE,
                      solidHeader = TRUE, collapsible = TRUE
                      , br(),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoPopulation"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("savePopulation"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),



                      br(), br(),br(),


                      rHandsontableOutput("popultaion",height = "auto"))
                ),


                fluidRow(
                  box(title = "Food Demand Model",
                      background = "aqua",width = 12, collapsed = TRUE,
                      solidHeader = TRUE, collapsible = TRUE
                      , br(),

                      # div(style="display:inline-block",
                      #
                      #
                      #     actionGroupButtons(
                      #       inputIds =c("saveFDM"),
                      #       labels=list(tags$span(icon("save"), "Save ")),
                      #       status = "warning", direction = "vertical"
                      #     )
                      # ),



                      br(), br(),br(),


                      rHandsontableOutput("food_fdm",height = "auto"))
                ),



                fluidRow(
                  box(title = "Food Classification",status = "primary"
                      , width = 12,   collapsed = TRUE,
                      solidHeader = TRUE, collapsible = TRUE
                      , br(),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoFoodclassific"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("savefoodclassific"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),



                      br(), br(),br(),

                      # selectInput( inputId = "filter_cpc", width = 150,
                      #                                label = "select CPC Code",
                      #                                choices = c("",unique(data_food_classification$CPCCode))
                      #   ),


                      rHandsontableOutput("food_classification",height = "auto"))
                ),




                fluidRow(
                  box(title = a("Food", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=73"), width = 12,  status = "primary", collapsed = TRUE,
                      solidHeader = TRUE, collapsible = TRUE
                      , br(),


                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Food"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_food"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadFoodModal"),
                                           labels = list(tags$span(icon("plus"), "Upload Food Data (Normalized)")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileFooddenormalized", "Choose Excel File",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileFood" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadFood", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(inputIds = c("food_imputation"),
                                             labels = list(tags$span(icon("magic"), "Imputation of Food")
                                             ),
                                             status = "primary", direction = "vertical")),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoFood"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),



                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveFood"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ),br(),br(),
                      DT::dataTableOutput("food",height = "auto"))
                )),

        tabItem(tabName = "Trade",
                fluidRow(
                  box(title = a("Imports", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=65"), width = 12,  status = "primary", br(),
                      solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE,
                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Imports"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_imports"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadImportsModal"),
                                           labels = list(tags$span(icon("plus"), "Normalized Upload")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileImportsdenormalized", "Denormalized Upload",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileImports" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadImports", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoImports"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveImports"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ), br(),br(),

                      DT::dataTableOutput("imports",height = "auto")),



                  box(title = a("Exports", style = "font-size: 100%;color: white;",href="FBSGuidelines.pdf#page=68"), width = 12,  status = "primary", br(),
                      solidHeader = TRUE, collapsible = TRUE,collapsed = TRUE,
                      div(style="display:inline-block",
                          div(style="display:inline-block",


                              actionGroupButtons(
                                inputIds =c("add_Exports"),
                                labels=list(tags$span(icon("plus"), "Add Commodity")),
                                status = "success", direction = "vertical"
                              )
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("delete_btn_exports"),
                            labels=list(tags$span(icon("trash-alt"), "Delete Commodity")),
                            status = "danger", direction = "vertical"
                          )
                      ),

                      div(style="display:inline-block",
                          dropdownButton(label = "Upload/Download", #fullwidth=TRUE,

                                         actionGroupButtons(
                                           inputIds = c("uploadExportsModal"),
                                           labels = list(tags$span(icon("plus"), "Normalized Upload")),
                                           status = "primary", direction = "vertical"
                                         ), br(),


                                         fileInput("fileExportsdenormalized", "Denormalized Upload",
                                                   multiple = TRUE,
                                                   accept = c(".xlsx"))



                                         , br(),


                                         # fileInput("fileCrop", "Choose Crop Excel File",
                                         #           multiple = TRUE,
                                         #           accept = c(".xlsx")),
                                         tags$script('$( "#fileImports" ).on( "click", function() { this.value = null; });'),

                                         downloadButton("downloadExports", "Download Excel file", class= "btn-block" ),
                                         tags$head(tags$style(".butt{background-color:#add8e6;} .butt{color: #337ab7;}")),
                                         circle = FALSE, status = "success", icon = icon("file-excel-o"),
                                         tooltip = tooltipOptions(title = "Download and Upload Excel formats", placement = "top")
                          )
                      ),

                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("undoExports"),
                            labels=list(tags$span(icon("undo"), "Undo ")),
                            status = "info", direction = "vertical"
                          )
                      ),


                      div(style="display:inline-block",


                          actionGroupButtons(
                            inputIds =c("saveExports"),
                            labels=list(tags$span(icon("save"), "Save ")),
                            status = "warning", direction = "vertical"
                          )
                      ), br(),br(),

                      DT::dataTableOutput("Exports",height = "auto"))
              )

        )
   )
    )
  )
