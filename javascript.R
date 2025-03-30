library(shiny)
library(shinyjs)
library(DT)

css <- HTML(
  "table.dataTable tr.selected td.highlighted {
    background-color: yellow !important;
  }
  td.highlighted {
    background-color: yellow !important;
  }"
)

js <- HTML(
  "function highlightCell(row, col, tableId) {
    var selector = 'tr:nth-child(' + row + ') td:nth-child(' + col + ')';
    $(tableId).find(selector).addClass('highlighted');
  }"
)

highlightCell <- function(row, col, tableId) {
  sprintf("highlightCell(%d, %d, %s)", row, col, tableId)
}

ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(css),
    tags$script(js)
  ),
  br(),
  DTOutput("crop_table"),
  br(),
  DTOutput("livestock_table")
)

crop_data <- iris[1:5, ]
livestock_data <- iris[6:15, ]
server <- function(input, output, session) {
  output[["crop_table"]] <- renderDT({
    datatable(
      crop_data,
      editable = TRUE,
      options = list(
        dom = 't',
        pageLength = 5
      )
    )
  }, server = FALSE)
  
  output[["livestock_table"]] <- renderDT({
    datatable(
      livestock_data,
      editable = TRUE,
      options = list(
        dom = 't',
        pageLength = 10
      )
    )
  }, server = FALSE)
  
  observeEvent(input[["crop_table_cell_edit"]], {
    info <- input[["crop_table_cell_edit"]]
    runjs(highlightCell(
      info[["row"]],
      info[["col"]] + 1,
      "#crop_table"
    ))
  })
  
  observeEvent(input[["livestock_table_cell_edit"]], {
    info <- input[["livestock_table_cell_edit"]]
    runjs(highlightCell(
      info[["row"]],
      info[["col"]] + 1,
      "#livestock_table"
    ))
  })
}

shinyApp(ui, server)