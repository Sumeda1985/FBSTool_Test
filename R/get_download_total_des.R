get_download_total_des <- function(by = 'level',input,session) {
  fbs_all_levels <- data.table(value$data_des)
  #write.csv(fbs_all_levels,"fbs_all_levels.csv",row.names = F)
  fbs_all_levels[,hidden := NULL]
  fbs_all_levels[,`FBS Code` := as.character(`FBS Code`)]
  fbs_all_levels[,ElementCode := as.character(ElementCode)]
  fbsTree <- data.table(dbReadTable(concore, "fbs_tree"))
  fbs_last_level <- copy(fbs_all_levels)
  
  fbs_1_level <- fbs_last_level[`FBS Code` %in% unique(fbsTree$id1)]
  fbs_2_level <- fbs_last_level[`FBS Code` %in% unique(fbsTree$id2)]
  fbs_3_level <- fbs_last_level[`FBS Code` %in% unique(fbsTree$id3)]
  fbs_4_level <- fbs_last_level[`FBS Code` %in% unique(fbsTree$id4)]
  fbs_fish    <- fbs_last_level[`FBS Code` %in% c("2960")]
  
  des_commodity <- value$sua_balanced_plugin
  #write.csv(des_commodity,"des_commodity.csv",row.names = F)
  #des_commodity <- des_commodity[measuredElementSuaFbs %in% c("664","674","684","261","271","281","665")]
  # des_commodity <- des_commodity[timePointYears %in% c(as.numeric(input$startYear):as.numeric(input$endYear))]
  des_commodity[, c("geographicAreaM49","flagObservationStatus") := NULL]
  des_commodity <- data.table(des_commodity)
  setnames(des_commodity, names(des_commodity),c("ElementCode","CPCCode","Year","Value"))
  des_commodity <- merge(des_commodity,all_cpc, by = "CPCCode", all.x = TRUE)
  des_commodity[, ElementCode := as.character(ElementCode)]
  des_commodity <- merge(des_commodity,all_elements, by = "ElementCode", all.x = TRUE)
  setcolorder(des_commodity, c("CPCCode","Commodity","ElementCode","Element", "Year","Value"))
  des_commodity[,Value := round(Value,0)]
  des_commodity =  dcast.data.table(des_commodity, CPCCode+Commodity+ElementCode+Element ~ Year, value.var = c("Value"))
  des_commodity <- des_commodity %>%
    filter(ElementCode %in% fbs_4_level$ElementCode) %>%
    arrange(CPCCode, Commodity, ElementCode)
  
  
  
  des_commodity <- des_commodity[order(`CPCCode`,factor(ElementCode, 
                                                        levels = c("664","665" ,"674","684","261","271","281",new_nutrient_element)))]
  require(openxlsx)
  wb <- createWorkbook()
  
  if(by == 'level') {
    df_list <- list(fbs_1_level=fbs_1_level, fbs_2_level=fbs_2_level,fbs_3_level=fbs_3_level,fbs_4_level=fbs_4_level,des_commodity=des_commodity, fish_seafood = fbs_fish)
    tab_names <- c('Level 1_Grand Total', 'Level 2_Anim.Veget.', 'Level 3_FBS Aggreg.', 'Level 4_FBS', 'Level 5_SUA', 'FBS Fish')
    
    for(i in 1:length(df_list)) {
      
      data <- df_list[[i]]
      name <- tab_names[i]
      addWorksheet(wb, name)
      
      # lowercase descriptions
      data[[2]] <- tolower(data[[2]])
      
      writeData(wb, name,data,
                startCol = 1,
                startRow = 1,
                colNames = TRUE)
      
      # comma separator for thousands
      year_columns <- 2:ncol(data)
      addStyle(wb,
               sheet = name,
               style = createStyle(
                 numFmt = '#,##0.00'
               ),
               rows = rep(1:nrow(data), length(year_columns)) + 1,
               cols = rep(year_columns, each = nrow(data))
      )
      
      # diving lines between groups
      # not for grand total
      if(i != 1) {
        new_element <- which(data[[2]] != c('', data[[2]][1:(nrow(data) - 1)]))
        
        addStyle(wb,
                 sheet = name,
                 style = createStyle(
                   borderStyle = 'thin',
                   border = 'Bottom'
                 ),
                 rows = rep(new_element, ncol(data)),
                 cols = rep(1:ncol(data), each = length(new_element)),
                 stack = TRUE
        )
      }
      
      
      # first four columns bold
      addStyle(wb,
               sheet = name,
               style = createStyle(
                 textDecoration = "bold"
               ),
               rows = rep(0:nrow(data), 4) + 1,
               cols = rep(1:4, each = nrow(data)+1),
               stack = TRUE
      )
      
      # highlight top row
      addStyle(wb,
               sheet = name,
               style = createStyle(
                 fgFill = "#99CCFF"
               ),
               rows = 1,
               cols = 1:ncol(data),
               stack = TRUE
      )
    }
  } else {
    for(year in as.numeric(as.numeric(2020) : as.numeric(2022))) {
      data_sheet = Prepare_data_des_report(
        year = year,
        fbs1 = fbs_1_level,
        fbs2 = fbs_2_level,
        fbs3 = fbs_3_level,
        fbs_tree = fbsTree,
        sua_balanced = value$data_sua_balanced_with_nut,
        fbs_balanced = value$data_fbs_balanced,
        population_data = data.table(dbReadTable(con, "pop_sws"))[StatusFlag== 1][,c("StatusFlag","LastModified") := NULL],
        elements = data.table(dbReadTable(concore, "elements")),
        country = value_database$data$Country[1]
      )
      
      Add_FBS_year_sheet_formatted(wb, as.character(year), data_sheet)
    }
  }
  
  return(wb)
}


