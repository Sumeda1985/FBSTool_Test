mapHS2FCL <- function (tradedata, maptable, hs6maptable, year = NULL, parallel = FALSE) 
{
  stopifnot(!is.null(year))
  stopifnot(is.integer(year))
  tradedataname <- lazyeval::expr_text(tradedata)
  hs6maptable <- hs6maptable %>% filter_(~fcl_links == 1L)
  
  uniqhs <- tradedata %>% select_(~reporter, ~flow, ~hs6, 
                                  ~hs) %>% distinct
  uniqhs <- anti_join(uniqhs, hs6maptable, by = c("reporter", 
                                                  "flow", "hs6"))
  
  
  hslength <- maxHSLength(uniqhs, maptable)
  
  uniqhs <- uniqhs %>% left_join(hslength, by = c("reporter", 
                                                  "flow")) %>% dplyr::mutate_(hsextchar = ~stringr::str_pad(hs, 
                                                                                                            width = maxhslength, side = "right", pad = "0"), hsext = ~as.numeric(hsextchar))
  
  maptable <- hslength %>% left_join(maptable, by = c(reporter = "area", 
                                                      "flow")) %>% dplyr::mutate(from_gt_to = as.numeric(fromcode) > 
                                                                                   as.numeric(tocode), fromcode = ifelse(from_gt_to, tocode, 
                                                                                                                         fromcode), tocode = ifelse(from_gt_to, fromcode, tocode)) %>% 
    dplyr::select(-from_gt_to) %>% dplyr::mutate_(fromcodeextchar = ~stringr::str_pad(fromcode, 
                                                                               width = maxhslength, side = "right", pad = "0"), tocodeextchar = ~stringr::str_pad(tocode, 
                                                                                                                                                                  width = maxhslength, side = "right", pad = "9")) %>% 
    dplyr::mutate_(fromcodeext = ~as.numeric(fromcodeextchar), 
                   tocodeext = ~as.numeric(tocodeextchar))
  
  uniqhs <- hsInRange(uniqhs, maptable, parallel = parallel)
  hs2fcl_mapped_links <- uniqhs
  
  
  uniqhs <- sel1FCL(uniqhs, maptable, cur_yr = year)
  uniqhs
}
