Prepare_data_fbs_report <- function(year, fbs_tree, fbs_balanced, fbs_balanced_final,
                                    commodity_names, nutrient_data, population_data,
                                    elements, country) {
  
  year <- as.character(year)
  
  # Add proteins and fat manually
  # to be removed once these elements are added to nutrients
  protein_fat <- fbs_balanced_final %>%
    filter(timePointYears == as.numeric(year),
           measuredElementSuaFbs %in% c(664, 674,684),
           measuredItemFbsSua %in% fbs_balanced$`FBS Code`) %>%
    rename(`FBS Code` = measuredItemFbsSua,
           ElementCode = measuredElementSuaFbs,
           value = Value) %>%
    dplyr::select(`FBS Code`, ElementCode, value) 
  
  
  ### Data preparation 
  elements$ElementCode = as.character(elements$ElementCode)
  
  data <- rbind(
    fbs_balanced %>% 
      rename(value = sym(year)) %>%
      dplyr::select(`FBS Code`, ElementCode, value),
    nutrient_data %>% 
      filter(year == !!year) %>%
      dplyr::select(-year),
    protein_fat
  ) %>%
    left_join(
      fbs_tree %>%
        mutate(id3 = paste0('S', id3),
               id4 = paste0('S', id4)) %>%
        dplyr::select(id3, id4) %>%
        rename(`FBS Code` = id4,
               parent = id3) %>%
        distinct,
      by = "FBS Code"
    ) %>%
    mutate(level = ifelse(is.na(parent), 3, 4)) %>%
    complete(nesting(`FBS Code`, `parent`, `level`), ElementCode, fill = list(value = 0)) %>%
    left_join(
      commodity_names %>%
        rename(`FBS Code` = CPCCode,
               `FBSGroup` = Commodity),
      by = 'FBS Code'
    ) %>%
    left_join(elements, by = 'ElementCode') %>%
    filter(!(ElementType %in% c('other', 'des total'))) %>%
    filter(include_fbs == 1) %>%
    dplyr::select(-include_fbs)
  
  data$level[data$`FBS Code` %in% paste0('S', fbs_tree$id2)] <- 2
  
  total <- data %>%
    filter(level == 2) %>%
    group_by(ElementCode) %>%
    summarise(`FBS Code` = 2901,
              parent = NA,
              level = 1,
              value = sum(value),
              FBSGroup = "Grand Total",
              ElementType = ElementType[1],
              Unit = Unit[1],
              Name = Name[1])
  
  data <- rbind(data,
                total)
  
  ## Add element names
  if(!all(data$ElementCode %in% elements$ElementCode)) {
    unrecognized <- data$ElementCode[!(data$ElementCode %in% elements$ElementCode)] %>%
      unique
    cat("Warning! Unrecognized ElementCode(s): ", unrecognized, '\n')
    data <- data %>% filter(!(ElementCode %in% unrecognized))
  }
  
  # long to wide format
  data_table = data %>%
    dplyr::select(`FBS Code`, parent, level, FBSGroup, value, ElementCode) %>%
    mutate(ElementCode = factor(ElementCode, elements$ElementCode)) %>%
    spread(key = "ElementCode", value = "value")
  
  # column names to be used in Excel
  index_element <- match(data_table %>% 
                           dplyr::select(FBSGroup :last_col()) %>% 
                           dplyr::select(-FBSGroup ) %>% colnames,
                         elements$ElementCode)
  element_name <- elements$Name[index_element]
  element_unit <- elements$Unit[index_element]
  
  # number of cols used for supply, utilization and nutritional value
  used_elements <- elements %>% filter(ElementCode %in% colnames(data_table))
  
  num_cols <- c(sum(used_elements$ElementType == 'supply'),
                sum(used_elements$ElementType == 'utilization'),
                sum(used_elements$ElementType == 'des')) 
  
  data_table[data_table[['level']] %in% c(1, 2), 
             colnames(data_table) %in% (
               elements %>% filter(ElementType %in% c('supply', 'utilization')) %>% pull(ElementCode)
             )] <- NA
  
  population <- population_data %>%
    filter(timePointYears == !!year) %>%
    pull(Value) * 1000
  
  return(
    list(
      data_table = data_table,
      element_name = element_name,
      element_unit = element_unit,
      num_cols = num_cols,
      population = population,
      country = country
    )
    
  )
}
