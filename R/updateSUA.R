updateSUA <- function(input,output,session){
   sua <-data.table(dbReadTable(con, "dbcountry"))[!is.na(CPCCode)][, c("CountryM49",
             "Country","StatusFlag","Commodity","Element","LastModified"):= NULL][Year %in% 2010:input$endyear]
  food_processing_data <- Calculate_Food_Processing(input,output,session)[,c("Commodity","Element") := NULL]
  sua <-rbind(sua,food_processing_data)
  sua_elements <- c("5510","5610","5910","5071", "5141", "5525","5520","5165","5166", "5016","5113", "5023")
  sua <- sua[ElementCode %in% sua_elements]
  sua[, Value := round(Value,0)]
  data_residual <- copy(sua)
  #####compute imbalance
  data_residual[,
                `:=`(
                  supply =
                    sum(Value[ElementCode %chin% c("5510","5610")],
                        - Value[ElementCode %chin% c("5910","5071")],
                        na.rm = TRUE),
                  # All elements that are NOT supply elements
                  utilizations =
                    sum(Value[!(ElementCode %chin% c("5510","5610","5910","5071","5113"))],
                        na.rm = TRUE)
                ),
                by = c("CPCCode","Year")
  ][,
    imbalance := supply - utilizations
  ][ , c("supply","utilizations") := NULL
  ][, c("ElementCode","Value", "Flag") := NULL][, ElementCode := c("5166")]
  setnames(data_residual, "imbalance","Value")
  data_residual[, Flag := c("I")]
  data_residual <- unique(data_residual)
  sua <- rbind(sua, data_residual)
  sua <- dcast.data.table(sua, CPCCode+ElementCode ~ Year,
                          value.var = c("Value", "Flag"))
  sua <- merge(sua, all_cpc, by ="CPCCode", all.x = TRUE)
  sua <- merge(sua, all_elements, by ="ElementCode", all.x = TRUE)
  # Store column patterns
  flag_pattern = "^Flag"
  value_pattern = "^Value"
  
  # Get column names once
  flag_cols = grep(flag_pattern, names(sua), value = TRUE)
  value_cols = grep(value_pattern, names(sua), value = TRUE)
  # Convert column types 
  sua[, (flag_cols) := lapply(.SD, as.character),
       .SDcols = (flag_cols)]
  sua[, (value_cols) := lapply(.SD, as.numeric),
       .SDcols = (value_cols)]
 # Prepare new column names
  new_flag_names = gsub("_", " ", flag_cols)
  new_value_names = gsub("^.*?_", "", value_cols)
  # Reorder and rename columns in one step
  new_order = c("CPCCode", "Commodity", "ElementCode", "Element",
                as.vector(rbind(value_cols, flag_cols)))
  setcolorder(sua, new_order)
  setnames(sua,
           old = c(flag_cols, value_cols),
           new = c(new_flag_names, new_value_names))
  sua <- merge(sua, fbsTree, by= c("CPCCode"), all.x = TRUE)
  # Define the desired order for ElementCode
  element_levels <- c("5113", "5510", "5610", "5910", "5071", "5141", "5023", "5525", "5520", "5016", "5165", "5166")
  # Order data by CPCCode, ElementCode (with specified levels), then by id4
  sua <-sua[order(CPCCode, factor(ElementCode, levels = element_levels))]
  sua <-sua[order(id4)][, id4 := NULL]
  sua[, hidden := ifelse(CPCCode != shift(CPCCode, type = "lead"), 1, 0)] 
  value$data_sua_unbalanced <- sua
  }