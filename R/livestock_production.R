#' Livestock Production Module
#' 
#' @param input Shiny input object
#' @param output Shiny output object
#' @param session Shiny session object
#' @importFrom data.table :=
#' @importFrom data.table setDT
#' @importFrom data.table copy
#' @importFrom shiny observeEvent
#' @importFrom shiny renderDataTable
#' @importFrom readxl read_excel

livestock_production <- function(input, output, session) {
  
  # Constants
  LIVESTOCK_ELEMENT_CODES <- c(
    "5510", "5318", "5319", "5320", 
    "5314", "5327", "5313", "5321"
  )
  LIVESTOCK_CLASSIFICATIONS <- c("LP", "LD", "L")
  
  #' Process initial livestock data
  #' @param end_year The target end year for processing
  process_livestock_data <- function(end_year) {
    # Get livestock data
    livestock_data <- subset(
      value_database$data,
      CPCCode %in% unique(
        subset(
          classification, 
          classification %in% LIVESTOCK_CLASSIFICATIONS
        )[, CPCCode]
      ) & ElementCode %in% LIVESTOCK_ELEMENT_CODES
    )
    
    setDT(livestock_data)
    
    # Remove duplicates
    livestock_data <- livestock_data[
      !duplicated(
        livestock_data, 
        by = c("CPCCode", "Commodity", "ElementCode", "Element", "Year")
      )
    ]
    
    # Handle slaughter data
    remove_slaughter <- livestock_data[
      , .(COUNT = .N), 
      by = c("CPCCode", "Year")
    ][
      , no_prod := ifelse(COUNT == 1 & ElementCode != "5510", 1, 0)
    ]
    slaughter_codes_to_remove <- unique(
      remove_slaughter[COUNT == 1 & no_prod == 1]$CPCCode
    )
    
    # Clean and filter data
    livestock_data[, c("CountryM49", "Country") := NULL]
    livestock_data <- livestock_data[
      Year %in% c(2010:end_year) &
      !is.na(Value) &
      !CPCCode %in% slaughter_codes_to_remove &
      ElementCode != "5327"
    ]
    
    wide_format(livestock_data)
  }
  
  #' Handle year validation and data processing
  #' @param data Processed livestock data
  #' @param end_year Target end year
  validate_and_process_years <- function(data, end_year) {
    year_cols <- grep("^[[:digit:]]{4}$", names(data), value = TRUE)
    max_year <- max(as.numeric(year_cols))
    
    if (end_year > max_year + 1) {
      years_to_fill <- (max_year + 1):end_year
      value$data_livestock <- NULL
      
      if (length(years_to_fill) > 0) {
        years_msg <- paste(
          years_to_fill[1:(length(years_to_fill) - 1)], 
          collapse = ", "
        )
        sendSweetAlert(
          session = session,
          title = "Error!",
          text = sprintf(
            "Please compile Livestock Production data for the year(s) %s first.",
            years_msg
          ),
          type = "error"
        )
      }
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Event Handlers
  observeEvent(input$startContinue, {
    end_year <- input$endyear
    livestock_data <- process_livestock_data(end_year)
    
    if (validate_and_process_years(livestock_data, end_year)) {
      livestock_data <- visualize_data_production(
        livestock_data, 
        end_year, 
        session
      )
      livestock_data[
        , hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)
      ]
      value$data_livestock <- livestock_data
      Add_table_version("livestock", copy(value$data_livestock))
    }
  })
  
  observeEvent(input$add_Livestock, {
    showModal(viewLivestockTriplets())
  })
  
  observeEvent(input$LivestockInsert, {
    removeModal()
    handle_livestock_insert()
  })
  
  # Table editing handlers
  proxy_livestock <- dataTableProxy('livestock')
  
  observeEvent(input$livestock_cell_edit, {
    info <- input$livestock_cell_edit
    row_idx <- info$row
    col_idx <- info$col + 1
    new_value <- info$value
    
    value$data_livestock[row_idx, (col_idx) := new_value]
    replaceData(
      proxy_livestock, 
      value$data_livestock, 
      resetPaging = FALSE, 
      rownames = FALSE
    )
    
    runjs(colorizeCell(row_idx, col_idx, "livestock"))
    Add_table_version("livestock", copy(value$data_livestock))
  })
  
  # Undo functionality
  observeEvent(input$undoLivestock, {
    new_version <- Pop_table_version("livestock")
    if (!is.null(new_version)) {
      value$data_livestock <- new_version
    }
  })
  
  # Delete functionality
  observeEvent(input$delete_btn_livestock, {
    if (!is.null(input$livestock_rows_selected)) {
      selected_data <- value$data_livestock[
        as.numeric(input$livestock_rows_selected)
      ]
      cpc_codes_to_remove <- unique(selected_data$CPCCode)
      
      # Update main data
      value$data_livestock <- value$data_livestock[
        !CPCCode %in% cpc_codes_to_remove
      ]
      
      # Update database
      value_database$data <- value_database$data[
        !(CPCCode %in% cpc_codes_to_remove & 
          ElementCode %in% LIVESTOCK_ELEMENT_CODES)
      ]
      
      Add_table_version("livestock", copy(value$data_livestock))
    }
  })
  
  # Download handler
  output$downloadLivestock <- downloadHandler(
    filename = function() "livestock_production.xlsx",
    content = function(file) {
      download_data <- value$data_livestock[!is.na(CPCCode)]
      download_data[, hidden := NULL]
      write.xlsx(download_data, file, row.names = FALSE)
    }
  )
  
  # Upload handlers
  observeEvent(input$uploadLivestockModal, {
    showModal(uploadLivestock())
  })
  
  output$livestockCountry <- renderDataTable({
    req(input$fileLivestock)
    inFile <- input$fileLivestock
    file_path <- paste(inFile$datapath, ".xlsx", sep = "")
    file.rename(inFile$datapath, file_path)
    
    df_livestockCountry$data_livestockCountry <- read_excel(file_path, 1)
    datatable(
      df_livestockCountry$data_livestockCountry,
      options = list(lengthMenu = c(5, 30, 50), pageLength = 5)
    )
  })
} 