Prepare_data_des_report <- function(year, fbs1, fbs2, fbs3, fbs_tree, 
                                    sua_balanced, fbs_balanced, population_data,
                                    elements, country) {
  
  #write.csv(value$data_sua_balanced_with_nut,"data_sua_balanced_with_nut.csv",row.names = F)
  #write.csv(value$data_fbs_balanced,"data_fbs_balanced.csv",row.names = F) 
 year <- as.character(year)
  ### Data preparation for level 1, 2 and 3 of the Food balance sheet
  # List ID4 for each FBS group
  fbs_child <- rbind(
    fbs_tree %>%
      dplyr::select(id1, id4) %>%
      rename(child = id4, parent = id1) %>%
      distinct,
    fbs_tree %>%
      dplyr::select(id2, id4) %>%
      rename(child = id4, parent = id2) %>%
      distinct,
    fbs_tree %>%
      dplyr::select(id3, id4) %>%
      rename(child = id4, parent = id3) %>%
      distinct
  ) %>%
    mutate(child = as.character(child)) %>%
    filter(!is.na(child))
  sua_by_fbs_code <- value$data_fbs_balanced %>%
    mutate(FBSCode = as.numeric(str_replace(`FBS Code`, 'S', ''))) %>%
    rename(value = sym(year)) %>%
    filter(FBSCode %in% fbs_child$parent) %>%
    dplyr::select(FBSCode, ElementCode, value) %>%
    mutate(FBSCode = as.character(FBSCode))%>%
    complete(FBSCode = unique(fbs_child$parent), 
             ElementCode = unique(fbs_balanced$ElementCode),
             fill = list(value = 0))
  # Extract the nutritional value of each FBSCode using the precalculated
  # fbs1, fbs2 and fbs4
  des_by_fbs_code <- rbind(
    fbs1 %>% 
      rename(FBSCode = `FBS Code`,
             value = sym(year)) %>%
      mutate(name = "Grand total",
             level = 1) %>%
      dplyr::select(FBSCode, name, level, ElementCode, value),
    fbs2 %>% 
      rename(FBSCode = `FBS Code`,
             value = sym(year),
             name = `FBS Group`) %>%
      mutate(level = 2) %>%
      dplyr::select(FBSCode,  name, level, ElementCode,value),
    fbs3 %>% 
      rename(FBSCode = `FBS Code`,
             value = sym(year),
             name = `FBS Group`) %>%
      mutate(level = 3) %>%
      dplyr::select(FBSCode, name, level, ElementCode, value)
  )
  name_fbs_code <- des_by_fbs_code %>%
    dplyr::select(FBSCode, name, level) %>%
    mutate(CPCCode = 0) %>%
    distinct
   des_by_fbs_code <- des_by_fbs_code %>% dplyr::select(-name, -level)
  ### Data preparation for the individual commodities 
  data_commodity <- sua_balanced %>% 
    rename(value = sym(year),
           child = `FBS Code`) %>%
    mutate(child = as.character(child))%>%
    left_join(fbs_tree %>% 
                dplyr::select(id3, id4) %>%
                rename(child = id4,
                       FBSCode = id3) %>%
                mutate(child = as.character(child)),
              by = 'child') %>%
    left_join(all_cpc %>% dplyr::select(-Commodity), by = "CPCCode")  %>%
    dplyr::select(CPCCode, FBSCode, ElementCode, value, Commodity) %>%
    mutate(value = replace_na(value, 0))
  
  name_commodity <- data_commodity %>%
    dplyr::select(CPCCode, FBSCode, Commodity) %>%
    mutate(level = 5) %>%
    rename(name = Commodity) %>%
    distinct
  
  data_commodity <- data_commodity %>% dplyr::select(-Commodity)
  
  ## combine data
  elements$ElementCode = as.character(elements$ElementCode)
  combined <- rbind(sua_by_fbs_code %>%
                      mutate(CPCCode = 0),
                    des_by_fbs_code %>%
                      mutate(CPCCode = 0),
                    data_commodity) %>%
    group_by(FBSCode, ElementCode, CPCCode) %>%
    slice(1) %>%
    ungroup() %>%
    complete(nesting(FBSCode, CPCCode), ElementCode, fill = list(value = 0)) %>%
    left_join(elements, by = 'ElementCode') %>%
    filter(!(ElementType %in% c('other', 'des total'))) %>%
    left_join(rbind(
      name_commodity %>% 
        mutate(name = as.character(name),
               CPCCode = as.character(CPCCode)) %>%
        rename(commodity = name),
      name_fbs_code %>% 
        mutate(name = as.character(name)) %>%
        rename(commodity = name)
    ), by = c('CPCCode', 'FBSCode'))
  
  if(!all(combined$ElementCode %in% elements$ElementCode)) {
    unrecognized <- combined$ElementCode[!(combined$ElementCode %in% elements$ElementCode)] %>%
      unique
    cat("Warning! Unrecognized ElementCode(s): ", unrecognized, '\n')
    combined <- combined %>% filter(!(ElementCode %in% unrecognized))
  }
  # long to wide format
  data_table = combined %>%
    dplyr::select(CPCCode, FBSCode, level, commodity, value, ElementCode) %>%
    mutate(ElementCode = factor(ElementCode, elements$ElementCode)) %>%
    spread(key = "ElementCode", value = "value")
  # column names to be used in Excel
  index_element <- match(data_table %>% 
                           dplyr::select(commodity:last_col()) %>% 
                           dplyr::select(-commodity) %>% colnames,
                         elements$ElementCode)
  element_name <- elements$Name[index_element]
  element_unit <- elements$Unit[index_element]
  
  # number of cols used for supply, utilization and nutritional value
  used_elements <- elements %>% filter(ElementCode %in% colnames(data_table))
  num_cols <- c(sum(used_elements$ElementType == 'supply'),
                sum(used_elements$ElementType == 'utilization'),
                sum(used_elements$ElementType == 'des')) 
  # don't show supply & utilization numbers at total and animal/veget level
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