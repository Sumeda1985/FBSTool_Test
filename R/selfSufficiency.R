generate_self_sufficiency_tab <- function(input,output,session){
  
  EC_PRODUCTION = 5510
  EC_IMPORT = 5610
  EC_EXPORT = 5910
  EC_STOCK_VARIATION = 5071
  
  plot_theme <- theme_bw() +
    theme(panel.border = element_blank(),
          axis.line = element_line())
  
  # colors used in the tool
  ggblue <- "#003399";
  ggorange <- "#E98300"
  
  get_years <- function(data) {
    cols <- colnames(data)
    year_col <- !grepl("\\D", cols)
    as.numeric(cols[year_col])
  }
  
  # hide/show grouping in SSR settings
  observe({
    
    ssr_level <- input$select_ssr_level 
    
    if(ssr_level == 'Commodity') {
      
      show(id = 'select_ssr_commodity', time = 0)
      hide(id = 'select_ssr_fbs_group', time = 0)
      
    } else { # FBS group
      
      hide(id = 'select_ssr_commodity', time = 0)
      show(id = 'select_ssr_fbs_group', time = 0)
      
    }
    
  })
  
  ssr <- function(production, import, export, stock_variation) {
    result <- 100 * replace_na(production, 0) / (replace_na(production, 0) + replace_na(import, 0) - replace_na(export, 0))
    result[is.na(production) & is.na(import) & is.na(export)] <- NA
    
    return(result)
  }
  
  idr <- function(production, import, export) {
    result <- 100 * (replace_na(import, 0) - replace_na(export, 0)) / (replace_na(production, 0) + replace_na(import, 0) - replace_na(export, 0))
    result[is.na(production) & is.na(import) & is.na(export)] <- NA
    
    return(result)
  }
  
  # returns the ssr when ssr = TRUE and idr when ssr = FALSE
  get_ssr_idr <- function(data, level, ssr = TRUE) {
    
    if(is.null(data)) {
      return(NULL)
    }
    
    if(ssr) {
      target = 'SSR'
    } else {
      target = 'IDR'
    }
    
    if(level == "Commodity") {
      data <- data %>%
        rename(food_code = `CPCCode`,
               food_name = `Commodity`)
    } else {
      data <- data %>%
        rename(food_code = `FBS Code`,
               food_name = `FBS Group`) %>%
        mutate(food_name = tolower(food_name))
    }
    
    
    years <- as.character(get_years(data))
    
    
    # df_fbs_balanced$data_fbs_balanced
    data %>%
      dplyr::select(food_code, food_name, ElementCode, years) %>%
      # using reshape as pivot_longer is not available in this version of tidyr
      as.data.frame %>%
      reshape(varying = years,
              times = years,
              v.names = "value",
              timevar = "year",
              direction = "long") %>%
      dplyr::select(-id) %>%
      filter(ElementCode %in% c(EC_PRODUCTION, EC_IMPORT, EC_EXPORT, EC_STOCK_VARIATION)) %>%
      complete(nesting(food_code, food_name), ElementCode, year, fill = list(value = NA)) %>%
      spread(key = "ElementCode", value = "value") %>%
      mutate(
        SSR = ssr(!!sym(as.character(EC_PRODUCTION)), !!sym(as.character(EC_IMPORT)), !!sym(as.character(EC_EXPORT)), !!sym(as.character(EC_STOCK_VARIATION))),
        IDR = idr(!!sym(as.character(EC_PRODUCTION)), !!sym(as.character(EC_IMPORT)), !!sym(as.character(EC_EXPORT)))
          ) %>%
      mutate(
        SSR = round(SSR, 0),
        IDR = round(IDR, 0)
      ) %>%
      dplyr::select(food_code, food_name, year, target) %>%
      spread(key = "year", value = target)
  }
  
  
  rename_ssr_idr_food_code_name <- function(df, level) {
    if(is.null(df)) {
      return(NULL)
    }

    if(level == "Commodity") {
      df <- df %>%
        rename(`CPCCode` = food_code,
               `Commodity` = food_name)
    } else {
      df <- df %>%
        rename(`FBS Code` = food_code,
               `FBS Group` = food_name)
    }
    
    return(df)
  }
  
  get_ssr_food <- function() {
    if(input$select_ssr_level == 'Commodity') {
      return(input$select_ssr_commodity)
    } else {
      return(input$select_ssr_fbs_group)
    }
  }
  
  get_ssr_code_name <- function(data) {
    if(input$select_ssr_level == 'Commodity') {
      return("CPC")
    } else {
      return("FBS Code")
    }
  }
  
  ssr_data <- reactive({
    
    if(input$select_ssr_level == "Commodity") {
      data = value$data_sua_balanced
      
    } else {
      data = value$data_fbs_balanced 
    }
    
    return(get_ssr_idr(data = data, level = input$select_ssr_level, ssr = TRUE))
    
  })
  
  idr_data <- reactive({
    
    if(input$select_ssr_level == "Commodity") {
      data = value$data_sua_balanced
    } else {
      data = value$data_fbs_balanced
    }
    
    return(get_ssr_idr(data = data, level = input$select_ssr_level, ssr = FALSE))
    
  })

  observe({
    data <- value
    
    if(!is.null(data$data_sua_balanced)) {
      commodities <- unique(data$data_sua_balanced$Commodity)
      updateSelectInput(session, "select_ssr_commodity", choices = commodities)
    }
    
  })
  
  observe({
    data <- value
    
    if(!is.null(data$data_fbs_balanced)) {
      fbs <- unique(data$data_fbs_balanced$`FBS Group`) %>% tolower
      updateSelectInput(session, "select_ssr_fbs_group", choices = fbs)
    }
    
  })
  
  output$table_ssr <- renderDataTable({
    
    print("test")
    data <- ssr_data()
    
    validate(
      need(!is.null(data), "Please run the balancing plugin")
    )
    
    datatable(rename_ssr_idr_food_code_name(
      data,
      input$select_ssr_level
    ))
     
  })
  
  output$table_idr <- renderDataTable({
    
    print("test2")
    data <- idr_data()
    
    validate(
      need(!is.null(data), "Please run the balancing plugin")
    )
    
    datatable(rename_ssr_idr_food_code_name(
      data,
      input$select_ssr_level
    ))
    
  })
  
  get_ssr_plot <- function() {
    ssr <- ssr_data()
    
    if(is.null(ssr)) {
      return(NULL)
    }
    
    food <- get_ssr_food()
    show_ratio <- input$check_ssr_show_ratio
    
    ssr_long <- ssr %>%
      filter(food_name == !!food) %>%
      as.data.frame %>%
      reshape(varying = as.character(get_years(ssr)),
              times = as.character(get_years(ssr)),
              v.names = "value",
              timevar = "year",
              direction = "long") %>%
      mutate(year = as.numeric(year))
    
    figure <- ggplot(ssr_long, aes(x = year, y = value)) +
      plot_theme +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks = get_years(ssr),labels = function(x) sprintf("%d", x)) +
      scale_y_continuous(expand = c(0.1, 0)) +
      xlab('') +
      ylab('SSR') +
      geom_point(color = ggorange) +
      geom_line(color = ggorange) +
      ggtitle(paste0(food, " (", get_ssr_code_name(), ": ", ssr_long$food_code[1], ")")) 
    
    if(show_ratio) {
      figure <- figure + geom_text(aes(label = round(value, 2)), vjust = -1)
    }
    
    
    return(figure)
  }
  
  output$ssr_plot <- renderPlot({
    get_ssr_plot()
  })
  
  output$download_ssr_plot <- downloadHandler(
    filename = "ssr.png",
    content = function(file) {
      ggsave(file, plot = get_ssr_plot(), width = 6, height = 4, dpi = 600)
    }
  )
  
  get_idr_plot <- function() {
    idr <- idr_data()
    
    if(is.null(idr)) {
      return(NULL)
    }
    
    food <- get_ssr_food()
    show_ratio <- input$check_ssr_show_ratio
    
    idr_long <- idr %>%
      filter(food_name == !!food) %>%
      as.data.frame %>%
      reshape(varying = as.character(get_years(idr)),
              times = as.character(get_years(idr)),
              v.names = "value",
              timevar = "year",
              direction = "long") %>%
      mutate(year = as.numeric(year))
    
    figure <- ggplot(idr_long, aes(x = year, y = value)) +
      plot_theme +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()) +
      scale_x_continuous(breaks = get_years(idr),labels = function(x) sprintf("%d", x)) +
      scale_y_continuous(expand = c(0.1, 0)) +
      xlab('') +
      ylab('IDR') +
      geom_point(color = ggorange) +
      geom_line(color = ggorange) +
      ggtitle(paste0(food, " (", get_ssr_code_name(), ": ", idr_long$food_code[1], ")")) 
    
    if(show_ratio) {
      figure <- figure + geom_text(aes(label = round(value, 2)), vjust = -1)
    }
    
    return(figure)
  }
  
  output$idr_plot <- renderPlot({
    get_idr_plot()
  })
  
  output$download_idr_plot <- downloadHandler(
    filename = "idr.png",
    content = function(file) {
      ggsave(file, plot = get_idr_plot(), width = 6, height = 4, dpi = 600)
    }
  )
  
  
  
  get_ssr_idr_plot <- function() {
    ssr <- ssr_data()
    idr <- idr_data()
    
    if(is.null(idr)) {
      return(NULL)
    }
    
    food <- get_ssr_food()
    show_ratio <- input$check_ssr_show_ratio
    
    idr_long <- idr %>%
      filter(food_name == !!food) %>%
      as.data.frame %>%
      reshape(varying = as.character(get_years(idr)),
              times = as.character(get_years(idr)),
              v.names = "value",
              timevar = "year",
              direction = "long") %>%
      mutate(year = as.numeric(year))
    
    ssr_long <- ssr %>%
      filter(food_name == !!food) %>%
      as.data.frame %>%
      reshape(varying = as.character(get_years(ssr)),
              times = as.character(get_years(ssr)),
              v.names = "value",
              timevar = "year",
              direction = "long") %>%
      mutate(year = as.numeric(year))
    
    data <- rbind(ssr_long %>% mutate(metric = 'SSR'),
                  idr_long %>% mutate(metric = 'IDR'))
    
    figure <- ggplot(data, aes(x = year, y = value, fill = metric)) +
      plot_theme +
      theme(panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            legend.position = 'bottom',
            legend.margin=margin(t=-15)) +
      scale_x_continuous(breaks = get_years(ssr), labels = function(x) sprintf("%d", x)) +
      scale_y_continuous(expand = c(0.15, 0)) +
      xlab('') +
      ylab('') +
      geom_bar(stat = 'identity', position = position_dodge()) +
      ggtitle(paste0(food, " (", get_ssr_code_name(), ": ", idr_long$food_code[1], ")")) +
      scale_fill_manual('', values = c(ggblue, ggorange))
    
    if(show_ratio) {
      figure <- figure + geom_text(aes(label = round(value, 2),
                             vjust = -1 * sign(value+0.00001)),
                         position = position_dodge(width = 1))
    }
    
    return(figure)
  }
  
  output$ssr_idr_plot <- renderPlot({
    get_ssr_idr_plot()
  })
  
  output$download_ssr_idr_plot <- downloadHandler(
    filename = "ssr_idr.png",
    content = function(file) {
      ggsave(file, plot = get_ssr_idr_plot(), width = 6, height = 4, dpi = 600)
    }
  )
  
  prepare_ssr_idr_sheet <- function(wb, data, tab) {
    addWorksheet(wb, tab)
    
    writeData(wb, tab, data %>% mutate_all(~ ifelse(is.nan(.x), NA, .x)))
    
    addStyle(wb,
             sheet = tab,
             style = createStyle(fgFill = "#99CCFF", 
                                 textDecoration = "bold",
                                 fontName = "arial",
                                 fontSize = 8),
             rows = 1,
             cols = 1:ncol(data))
    
    addStyle(wb,
             sheet = tab,
             style = createStyle(fgFill = "#e6f2ff", 
                                 textDecoration = "bold",
                                 fontName = "arial",
                                 fontSize = 8),
             rows = rep(2:(nrow(data)+1), 2),
             cols = rep(1:2, each = nrow(data)))
    
    addStyle(wb,
             sheet = tab,
             style = createStyle(fgFill = "#FFFF99", 
                                 fontName = "arial",
                                 fontSize = 8),
             rows = rep(2:(nrow(data)+1), ncol(data)-2),
             cols = rep(3:ncol(data), each = nrow(data)))
    
    setColWidths(wb, sheet = tab, cols = 2, widths = 25)
  }
  
  prepare_ssr_idr_excel <- function(ssr, idr) {
    
    require(openxlsx)
    wb <- createWorkbook()
    
    ## SSR
    prepare_ssr_idr_sheet(wb, ssr, 'SSR')
    prepare_ssr_idr_sheet(wb, idr, 'IDR')
    
    return(wb)
  }
  
  
  output$download_table_ssr <- downloadHandler(
    filename = "ssr_idr.xlsx",
    content = function(filename) {
      ssr = ssr_data() %>% 
        rename_ssr_idr_food_code_name(input$select_ssr_level)
      idr = idr_data() %>% 
        rename_ssr_idr_food_code_name(input$select_ssr_level)
      
      wb <- prepare_ssr_idr_excel(ssr, idr)
      saveWorkbook(wb, filename, overwrite = FALSE, returnValue = FALSE)
    }
  )
  
  output$download_table_idr <- downloadHandler(
    filename = "ssr_idr.xlsx",
    content = function(filename) {
      ssr = ssr_data() %>% 
        rename_ssr_idr_food_code_name(input$select_ssr_level)
      idr = idr_data() %>% 
        rename_ssr_idr_food_code_name(input$select_ssr_level)
      
      wb <- prepare_ssr_idr_excel(ssr, idr)
      saveWorkbook(wb, filename, overwrite = FALSE, returnValue = FALSE)
    }
  )
  
}


