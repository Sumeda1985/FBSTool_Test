library(shiny)
library(shinyjs)
library(DT)

css <- HTML(
  "table.dataTable tr.selected td.yellow {
  background-color: yellow !important
  }
  td.yellow {
  background-color: yellow !important
  }"
)



js <- HTML(
  "function colorizeCell(i, j,id){
  var selector = 'tr:nth-child(' + i + ') td:nth-child(' + j + ')';
  $(id).find(selector).addClass('yellow');
  }"
)



colorizeCell <- function(i, j,id){
  sprintf("colorizeCell(%d, %d, %s)", i, j, id)
}



ui <- fluidPage(
 
  br(),
  DTOutput("crop"), useShinyjs(),
  tags$head(
    tags$style(css),
    tags$script(js)
  ),
  br(),
  
  DTOutput("livestock")
)

dat1 <- iris[1:5, ]
dat2 <- iris[6:15, ]
server <- function(input, output, session){
  
  output[["crop"]] <- renderDT({
    datatable(dat1, editable = TRUE)
  }, server = FALSE)
  
  output[["livestock"]] <- renderDT({
    datatable(dat2, editable = TRUE)
  }, server = FALSE)
  
  observeEvent(input[["crop_cell_edit"]], {
    info1 <- input[["crop_cell_edit"]]
    i <- info1[["row"]]
    j <- info1[["col"]]
    runjs(colorizeCell(i, j+1,"crop"))
  })
  
  observeEvent(input[["livestock_cell_edit"]], {
    info2 <- input[["livestock_cell_edit"]]
    i <- info2[["row"]]
    j <- info2[["col"]]
    runjs(colorizeCell(i, j+1,"livestock"))
  })
  
  
}

shinyApp(ui, server)