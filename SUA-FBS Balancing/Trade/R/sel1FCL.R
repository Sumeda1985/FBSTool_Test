sel1FCL <- function (hsfclmatch, maptable, cur_yr = NULL) 
{
  stopifnot(!is.null(cur_yr))
  stopifnot(is.integer(cur_yr))
  hsfclmatch <- hsfclmatch %>% left_join(maptable %>% select_(~startyear, 
                                                              ~endyear, ~recordnumb, ~fromcodeext, ~tocodeext), by = "recordnumb") %>% 
    mutate_(hsrange = ~tocodeext - fromcodeext) %>% mutate_(inrange = ~startyear <= 
                                                              cur_yr & endyear >= cur_yr, rangedist = ~pmin.int(abs(startyear - 
                                                                                                                      cur_yr), abs(endyear - cur_yr)), rangedist = ~ifelse(inrange, 
                                                                                                                                                                           0, rangedist)) %>% group_by(datumid) %>% filter_(~rangedist == 
                                                                                                                                                                                                                              min(rangedist)) %>% filter_(~hsrange == min(hsrange)) %>% 
    filter_(~recordnumb == max(recordnumb)) %>% ungroup()
  hsfclmatch %>% select_(~reporter, ~flow, ~datumid, ~hs, 
                         ~hsext, ~fcl, ~recordnumb)
}
