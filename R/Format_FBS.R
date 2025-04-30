require(openxlsx)
Add_FBS_year_sheet_formatted <- function(
  wb, # openxlsx workbook
  year,
  data # created by Prepare_data_des_report
)
{
  tab = data$data_table
  element_name = data$element_name
  element_unit = data$element_unit
  num_cols = data$num_cols
  population = data$population
  country = data$country
  
  tab$commodity[tab$commodity == 'ANIMAL PRODUCTS - DEMAND' & !is.na(tab$commodity)] <- 'ANIMAL PROD. (DEMAND)'
  
  addWorksheet(wb, year)
  
  style_fbs_group_name <- createStyle(fgFill = "#99CCFF", 
                                      textDecoration = "bold",
                                      fontName = "arial",
                                      fontSize = 8)
  style_commodity_name <- createStyle(fgFill = "#CCFFFF",
                                      textDecoration = "bold",
                                      fontName = "arial",
                                      fontSize = 8)
  
  style_fbs_group_supply <- createStyle(fgFill = "#FFCC00", 
                                        textDecoration = "bold",
                                        fontName = "arial",
                                        fontSize = 8)
  style_commodity_supply <- createStyle(fgFill = "#FFFF99",
                                        fontName = "arial",
                                        fontSize = 8)
  
  col_supply = 1 + 1:(num_cols[1])
  
  style_fbs_group_utilization <- createStyle(fgFill = "#FF9900", 
                                             textDecoration = "bold",
                                             fontName = "arial",
                                             fontSize = 8)
  style_commodity_utilization <- createStyle(fgFill = "#FFD1A3",
                                             fontName = "arial",
                                             fontSize = 8)
  
  col_utilization = num_cols[1] + 1 + 1:(num_cols[2])
  style_fbs_group_des <- createStyle(fgFill = "#99CC00", 
                                     textDecoration = "bold",
                                     fontName = "arial",
                                     fontSize = 8)
  style_commodity_des <- createStyle(fgFill = "#CCFFCC",
                                     fontName = "arial",
                                     fontSize = 8)
  
  col_des = num_cols[1] + num_cols[2] + 1 + 1:(num_cols[3])
  
  row <- 1
  
  add_style_commodity <- function(rows) {
    addStyle(wb,
             sheet = year,
             style = style_commodity_name,
             rows = rows,
             cols = rep(1, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_commodity_supply,
             rows = rep(rows, each = length(col_supply)),
             cols = rep(col_supply, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_commodity_utilization,
             rows = rep(rows, each = length(col_utilization)),
             cols = rep(col_utilization, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_commodity_des,
             rows = rep(rows, each = length(col_des)),
             cols = rep(col_des, length(rows)))
  }
  
  add_style_fbs_group <- function(rows) {
    addStyle(wb,
             sheet = year,
             style = style_fbs_group_name,
             rows = rows,
             cols = rep(1, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_fbs_group_supply,
             rows = rep(rows, each = length(col_supply)),
             cols = rep(col_supply, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_fbs_group_utilization,
             rows = rep(rows, each = length(col_utilization)),
             cols = rep(col_utilization, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_fbs_group_des,
             rows = rep(rows, each = length(col_des)),
             cols = rep(col_des, length(rows)))
  }
  
  # Header
  addStyle(wb, year, 
           style = createStyle(textDecoration = "bold",
                               fontName = "arial",
                               fontSize = 14),
           rows = row:(row+2),
           cols = 2)
  writeData(wb, year,
            x = c(country, 'Food Balance Sheet'),
            startCol = 2,
            startRow = row,
            colNames = FALSE)
  writeData(wb, year,
            x = as.numeric(year),
            startCol = 2,
            startRow = row+2,
            colNames = FALSE)
  addStyle(wb, year, 
           style = createStyle(halign = 'left'),
           rows = row+2,
           cols = 2,
           stack = TRUE)
  
  
  addStyle(wb, year, 
           style = createStyle(textDecoration = "bold",
                               fontName = "arial",
                               fontSize = 13),
           rows = row+4,
           cols = 2:4)
  writeData(wb, year,
            x = c("Population ('000):"),
            startCol = 2,
            startRow = row+4,
            colNames = FALSE)
  writeData(wb, year,
            x = round(population/1000),
            startCol = 4,
            startRow = row+4,
            colNames = FALSE)
  
  
  row <- row + 6
  
  # Colnames group
  writeData(wb, year, 
            x = c(rep('DOMESTIC SUPPLY', length(col_supply)),
                  rep('DOMESTIC UTILIZATION', length(col_utilization)),
                  rep('AVAILABILITY PER CAPITA PER DAY', length(col_des))) %>% t,
            startCol = 2,
            startRow = row,
            colNames = FALSE)
  
  addStyle(wb, year, 
           style = createStyle(fgFill = "#FFCC00", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               borderStyle = 'medium',
                               border = 'TopBottomLeftRight'),
           rows = row,
           cols = c(col_supply))
  mergeCells(wb, year, c(col_supply), row)
  
  addStyle(wb, year, 
           style = createStyle(fgFill = "#FF9900", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               borderStyle = 'medium',
                               border = 'TopBottomLeftRight'),
           rows = row,
           cols = c(col_utilization))
  mergeCells(wb, year, c(col_utilization), row)
  
  addStyle(wb, year, 
           style = createStyle(fgFill = "#99CC00", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               borderStyle = 'medium',
                               border = 'TopBottomLeftRight'),
           rows = row,
           cols = col_des)
  mergeCells(wb, year, c(col_des), row)
  top_group_row <- row
  
  row <- row + 3
  
  # Product
  writeData(wb, year, 
            x = t(element_name),
            startCol = 2,
            startRow = row,
            colNames = FALSE)
  addStyle(wb, year,
           style = createStyle(textDecoration = "bold",
                               fontName = "arial",
                               fontSize = 8,
                               halign = 'center'),
           rows = row,
           cols = c(col_supply, col_utilization, col_des))
  row <- row + 1
  
  addStyle(wb, year, 
           style = createStyle(fgFill = "#99CCFF", 
                               textDecoration = "bold",
                               fontName = "arial",
                               fontSize = 8),
           rows = row,
           cols = 1)
  addStyle(wb, year, 
           style = createStyle(fgFill = "#FFFF99", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               border = 'TopBottomLeftRight',
                               borderStyle = 'medium'),
           rows = row,
           cols = c(col_supply, col_utilization))
  addStyle(wb, year, 
           style = createStyle(fgFill = "#CCFFCC", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               border = 'TopBottom',
                               borderStyle = 'medium'),
           rows = row,
           cols = col_des)
  addStyle(wb, year, style = style_fbs_group_name, rows = row, cols = 1)
  writeData(wb, year, 
            x = t(c("Product", element_unit)),
            startCol = 1,
            startRow = row,
            colNames = FALSE)
  mergeCells(wb, year, c(col_supply, col_utilization), row)
  
  row <- row + 2
  
  
  # Add grand total
  writeData(wb, 
            sheet = year, 
            x = tab %>%
              filter(level == 1) %>%
              dplyr::select(commodity:last_col()), 
            startCol = 1, 
            startRow = row,
            colNames = FALSE)
  add_style_fbs_group(row)
  
  row <- row + 2
  
  # Add level 2
  writeData(wb, 
            sheet = year, 
            x = tab %>%
              filter(level == 2) %>%
              dplyr::select(commodity:last_col()), 
            startCol = 1, 
            startRow = row,
            colNames = FALSE)
  add_style_fbs_group(row:(row+1))
  row <- row + 3
  
  # Add level 3/5
  data3 <- tab %>%
    filter(level == 3)
  for(i in 1:nrow(data3)) {
    writeData(wb, 
              sheet = year, 
              x = data3[i,,drop=FALSE] %>%
                dplyr::select(commodity:last_col()), 
              startCol = 1, 
              startRow = row,
              colNames = FALSE)
    add_style_fbs_group(row)
    row <- row + 1
    
    child_commodities <- tab %>%
      filter(FBSCode == data3$FBSCode[i],
             level == 5) %>%
      dplyr::select(commodity:last_col())
    
    writeData(wb, 
              sheet = year, 
              x = child_commodities, 
              startCol = 1, 
              startRow = row,
              colNames = FALSE)
    add_style_commodity(row:(row + nrow(child_commodities) - 1))
    row <- row + nrow(child_commodities) + 1
  }
  
  # borders thin
  rows = (top_group_row+5):(row-2)
  cols = c(1, col_supply, col_utilization, col_des)
  addStyle(wb, year,
           style = createStyle(
             border = 'TopBottomLeftRight',
             borderStyle = 'thin'
           ),
           rows = rep(rows, each = length(cols)),
           cols = rep(cols, length(rows)),
           stack = TRUE)
  
  # thousand separator
  addStyle(wb,
           sheet = year,
           style = createStyle(
             numFmt = '#,##0.00'
           ),
           rows = rep(rows, each = length(col_des)),
           cols = rep(col_des, length(rows)),
           stack = TRUE)
  addStyle(wb,
           sheet = year,
           style = createStyle(
             numFmt = '#,##0'
           ),
           rows = rep(rows, each = length(c(col_supply, col_utilization))),
           cols = rep(c(col_supply, col_utilization), length(rows)),
           stack = TRUE)
  
  
  # vertical lines
  addStyle(wb, year,
           style = createStyle(
             border = 'left',
             borderStyle = 'medium'
           ),
           rows = rep(top_group_row:(row-2), 4),
           cols = rep(c(col_supply[1], col_utilization[1], col_des[1], tail(col_des, 1)+1), 
                      each = length(top_group_row:(row-2))),
           stack = TRUE)
  
  
}


require(openxlsx)
Add_FBS_year_sheet_formatted2 <- function(
  wb, # openxlsx workbook
  year,
  data # created by Prepare_data_des_report
)
{
  tab = data$data_table
  element_name = data$element_name
  element_unit = data$element_unit
  num_cols = data$num_cols
  population = data$population
  country = data$country
  
  tab$FBSGroup[tab$FBSGroup == 'ANIMAL PRODUCTS - DEMAND'  & !is.na(tab$FBSGroup)] <- 'ANIMAL PROD. (DEMAND)'
  
  addWorksheet(wb, year)
  
  style_fbs_group_name <- createStyle(fgFill = "#99CCFF", 
                                      textDecoration = "bold",
                                      fontName = "arial",
                                      fontSize = 8)
  style_commodity_name <- createStyle(fgFill = "#CCFFFF",
                                      textDecoration = "bold",
                                      fontName = "arial",
                                      fontSize = 8)
  
  style_fbs_group_supply <- createStyle(fgFill = "#FFCC00", 
                                        textDecoration = "bold",
                                        fontName = "arial",
                                        fontSize = 8)
  style_commodity_supply <- createStyle(fgFill = "#FFFF99",
                                        fontName = "arial",
                                        fontSize = 8)
  
  col_supply = 1 + 1:(num_cols[1])
  
  style_fbs_group_utilization <- createStyle(fgFill = "#FF9900", 
                                             textDecoration = "bold",
                                             fontName = "arial",
                                             fontSize = 8)
  style_commodity_utilization <- createStyle(fgFill = "#FFD1A3",
                                             fontName = "arial",
                                             fontSize = 8)
  
  col_utilization = num_cols[1] + 1 + 1:(num_cols[2])
  style_fbs_group_des <- createStyle(fgFill = "#99CC00", 
                                     textDecoration = "bold",
                                     fontName = "arial",
                                     fontSize = 8)
  style_commodity_des <- createStyle(fgFill = "#CCFFCC",
                                     fontName = "arial",
                                     fontSize = 8)
  
  col_des = num_cols[1] + num_cols[2] + 1 + 1:(num_cols[3])
  
  row <- 1
  
  add_style_commodity <- function(rows) {
    addStyle(wb,
             sheet = year,
             style = style_commodity_name,
             rows = rows,
             cols = rep(1, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_commodity_supply,
             rows = rep(rows, each = length(col_supply)),
             cols = rep(col_supply, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_commodity_utilization,
             rows = rep(rows, each = length(col_utilization)),
             cols = rep(col_utilization, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_commodity_des,
             rows = rep(rows, each = length(col_des)),
             cols = rep(col_des, length(rows)))
  }
  
  add_style_fbs_group <- function(rows) {
    addStyle(wb,
             sheet = year,
             style = style_fbs_group_name,
             rows = rows,
             cols = rep(1, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_fbs_group_supply,
             rows = rep(rows, each = length(col_supply)),
             cols = rep(col_supply, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_fbs_group_utilization,
             rows = rep(rows, each = length(col_utilization)),
             cols = rep(col_utilization, length(rows)))
    addStyle(wb,
             sheet = year,
             style = style_fbs_group_des,
             rows = rep(rows, each = length(col_des)),
             cols = rep(col_des, length(rows)))
  }
  
  # Header
  addStyle(wb, year, 
           style = createStyle(textDecoration = "bold",
                               fontName = "arial",
                               fontSize = 14),
           rows = row:(row+2),
           cols = 2)
  writeData(wb, year,
            x = c(country, 'Food Balance Sheet'),
            startCol = 2,
            startRow = row,
            colNames = FALSE)
  writeData(wb, year,
            x = as.numeric(year),
            startCol = 2,
            startRow = row+2,
            colNames = FALSE)
  addStyle(wb, year, 
           style = createStyle(halign = 'left'),
           rows = row+2,
           cols = 2,
           stack = TRUE)
  
  
  addStyle(wb, year, 
           style = createStyle(textDecoration = "bold",
                               fontName = "arial",
                               fontSize = 13),
           rows = row+4,
           cols = 2:4)
  writeData(wb, year,
            x = c("Population ('000):"),
            startCol = 2,
            startRow = row+4,
            colNames = FALSE)
  writeData(wb, year,
            x = round(population/1000),
            startCol = 4,
            startRow = row+4,
            colNames = FALSE)
  
  
  row <- row + 6
  
  # Colnames group
  writeData(wb, year, 
            x = c(rep('DOMESTIC SUPPLY', length(col_supply)),
                  rep('DOMESTIC UTILIZATION', length(col_utilization)),
                  rep('AVAILABILITY PER CAPITA PER DAY', length(col_des))) %>% t,
            startCol = 2,
            startRow = row,
            colNames = FALSE)
  
  addStyle(wb, year, 
           style = createStyle(fgFill = "#FFCC00", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               borderStyle = 'medium',
                               border = 'TopBottomLeftRight'),
           rows = row,
           cols = c(col_supply))
  mergeCells(wb, year, c(col_supply), row)
  
  addStyle(wb, year, 
           style = createStyle(fgFill = "#FF9900", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               borderStyle = 'medium',
                               border = 'TopBottomLeftRight'),
           rows = row,
           cols = c(col_utilization))
  mergeCells(wb, year, c(col_utilization), row)
  
  addStyle(wb, year, 
           style = createStyle(fgFill = "#99CC00", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               borderStyle = 'medium',
                               border = 'TopBottomLeftRight'),
           rows = row,
           cols = col_des)
  mergeCells(wb, year, c(col_des), row)
  top_group_row <- row
  
  row <- row + 3
  
  # Product
  writeData(wb, year, 
            x = t(element_name),
            startCol = 2,
            startRow = row,
            colNames = FALSE)
  addStyle(wb, year,
           style = createStyle(textDecoration = "bold",
                               fontName = "arial",
                               fontSize = 8,
                               halign = 'center'),
           rows = row,
           cols = c(col_supply, col_utilization, col_des))
  row <- row + 1
  
  addStyle(wb, year, 
           style = createStyle(fgFill = "#99CCFF", 
                               textDecoration = "bold",
                               fontName = "arial",
                               fontSize = 8),
           rows = row,
           cols = 1)
  addStyle(wb, year, 
           style = createStyle(fgFill = "#FFFF99", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               border = 'TopBottomLeftRight',
                               borderStyle = 'medium'),
           rows = row,
           cols = c(col_supply, col_utilization))
  addStyle(wb, year, 
           style = createStyle(fgFill = "#CCFFCC", 
                               textDecoration = "bold",
                               halign = "center",
                               fontName = "arial",
                               fontSize = 8,
                               border = 'TopBottom',
                               borderStyle = 'medium'),
           rows = row,
           cols = col_des)
  addStyle(wb, year, style = style_fbs_group_name, rows = row, cols = 1)
  writeData(wb, year, 
            x = t(c("Product",element_unit)),
            startCol = 1,
            startRow = row,
            colNames = FALSE)
  mergeCells(wb, year, c(col_supply, col_utilization), row)
  
  row <- row + 2
  
  
  # Add grand total
  writeData(wb,
            sheet = year,
            x = tab %>%
              filter(level == 1) %>%
              dplyr::select(FBSGroup:last_col()),
            startCol = 1,
            startRow = row,
            colNames = FALSE)
  add_style_fbs_group(row)
  
  row <- row + 2
  
  # Add level 2
  writeData(wb,
            sheet = year,
            x = tab %>%
              filter(level == 2) %>%
              dplyr::select(FBSGroup:last_col()),
            startCol = 1,
            startRow = row,
            colNames = FALSE)
  add_style_fbs_group(row:(row+1))
  row <- row + 3
  
  # Add level 3/4
  data3 <- tab %>%
    filter(level == 3)
  for(i in 1:nrow(data3)) {
    writeData(wb, 
              sheet = year, 
              x = data3[i,,drop=FALSE] %>%
                dplyr::select(FBSGroup:last_col()), 
              startCol = 1, 
              startRow = row,
              colNames = FALSE)
    add_style_fbs_group(row)
    row <- row + 1
    
    child_commodities <- tab %>%
      filter(parent == data3$`FBS Code`[i],
             level == 4) %>%
      dplyr::select(FBSGroup:last_col())
    
    writeData(wb, 
              sheet = year, 
              x = child_commodities, 
              startCol = 1, 
              startRow = row,
              colNames = FALSE)
    add_style_commodity(row:(row + nrow(child_commodities) - 1))
    row <- row + nrow(child_commodities) + 1
  }
  
  # borders thin
  rows = (top_group_row+5):(row-2)
  cols = c(1, col_supply, col_utilization, col_des)
  addStyle(wb, year,
           style = createStyle(
             border = 'TopBottomLeftRight',
             borderStyle = 'thin'
           ),
           rows = rep(rows, each = length(cols)),
           cols = rep(cols, length(rows)),
           stack = TRUE)
  
  # thousand separator
  addStyle(wb,
           sheet = year,
           style = createStyle(
             numFmt = '#,##0.00'
           ),
           rows = rep(rows, each = length(col_des)),
           cols = rep(col_des, length(rows)),
           stack = TRUE)
  addStyle(wb,
           sheet = year,
           style = createStyle(
             numFmt = '#,##0'
           ),
           rows = rep(rows, each = length(c(col_supply, col_utilization))),
           cols = rep(c(col_supply, col_utilization), length(rows)),
           stack = TRUE)
  
  
  # vertical lines
  addStyle(wb, year,
           style = createStyle(
             border = 'left',
             borderStyle = 'medium'
           ),
           rows = rep(top_group_row:(row-2), 4),
           cols = rep(c(col_supply[1], col_utilization[1], col_des[1], tail(col_des, 1)+1), 
                      each = length(top_group_row:(row-2))),
           stack = TRUE)
  
  
}