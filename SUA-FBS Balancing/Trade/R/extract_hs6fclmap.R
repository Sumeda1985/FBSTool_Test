extract_hs6fclmap <- function (maptable = NULL, parallel = FALSE) 
{
  stopifnot(!is.null(maptable))
  if (!"reporter" %in% colnames(maptable) & "area" %in% colnames(maptable)) {
    maptable <- rename_(maptable, .dots = list(reporter = ~area))
  }
  maptable <- select_(maptable, ~reporter, ~flow, ~fromcode, 
                      ~tocode, ~fcl)
  # flog.trace("HS6 map: calculation of HS ranges", name = "dev")
  maptable <- maptable %>% dplyr::mutate_at(vars(ends_with("code")), 
                                            funs(str_sub(., end = 6L))) %>% dplyr::mutate_at(vars(ends_with("code")), 
                                                                                             as.integer) %>% dplyr::mutate_(hsrange = ~tocode - fromcode)
  maptable_0range <- maptable %>% filter_(~hsrange == 0) %>% 
    select_(~reporter, ~flow, hs6 = ~fromcode, ~fcl)
  # flog.trace("HS6 map: convert HS ranges into explicit HS codes", 
  #            name = "dev")
  maptable_range <- maptable %>% filter_(~hsrange > 0) %>% 
    rowwise() %>% dplyr::mutate(hs6 = list(fromcode:tocode)) %>% 
    tidyr::unnest() %>% dplyr::select(reporter, flow, hs6, fcl)
  # flog.trace("HS6 map: counting FCL matches per HS6", name = "dev")
  bind_rows(maptable_0range, maptable_range) %>% group_by(reporter, 
                                                          flow, hs6) %>% dplyr::mutate(fcl_links = n_distinct(fcl)) %>% 
    ungroup() %>% distinct()
}
