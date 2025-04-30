hsInRange <- function (uniqhs, maptable, parallel = FALSE) 
{
  df <- uniqhs %>% dplyr::mutate(datumid = row_number()) %>% 
    dplyr::mutate(fromcodeext = hsext, tocodeext = hsext) %>% 
    as.data.table()
  maptable <- as.data.table(maptable)
  setkey(maptable, reporter, flow, fromcodeext, tocodeext)
  foverlaps(df, maptable) %>% tbl_df() %>%  dplyr::select(reporter, 
                                                  flow, datumid, hs, hsext, fcl, recordnumb)
}
