suppressPackageStartupMessages(library(data.table))
library(stringr)
# library(magrittr)
# library(scales)
library(tidyr, warn.conflicts = FALSE)
# library(futile.logger)
suppressPackageStartupMessages(library(dplyr, warn.conflicts = FALSE))
# library(faosws)
library(faoswsUtil)
library(faoswsTrade)
# library(faoswsFlag)
# # library(bit64)
# 
# basedir <-getwd()


files = dir("SUA-FBS Balancing/Trade/R",full.names = TRUE)
for(i in files){
  source(i, local = TRUE)
}

start_time <- Sys.time()

if (CheckDebug()) {
  mydir <- "SUA-FBS Balancing/"

  SETTINGS <- faoswsModules::ReadSettings(file.path(mydir, "sws.yml"))

  SetClientFiles(SETTINGS[["certdir"]])

  GetTestEnvironment(baseUrl = SETTINGS[["server"]], token = SETTINGS[["token"]])
  R_SWS_SHARE_PATH <- "//hqlprsws1.hq.un.fao.org/sws_r_share"
}
# 

generate_hs6mapping <- TRUE

# Parallel backend will be used only if required packages are installed.
# It will be switched to FALSE if packages are not available.
multicore <- TRUE

M49_code <- c("400")

# The main trademap (i.e., the "historical" one) is:
  
  # hsfclmap3 <- ReadDatatable('hsfclmap5')
  # 
  # write.csv(hsfclmap3,"SUA-FBS Balancing/Trade/hsfclmap3.csv",row.names = F)
  
  hsfclmap3 <- fread("SUA-FBS Balancing/Trade/Data/hsfclmap3.csv")
  
  hsfclmap3 <- hsfclmap3[area %in% fao_code_country[`M49 Code` == M49_code]$`Country Code`]

# this one is used to "patch" the previous one (i.e., to apply corrections) for missing links:
  
  # add_map <- ReadDatatable('hsfclmap4')
  # write.csv(add_map,"SUA-FBS Balancing/Trade/add_map.csv",row.names = F)

  
  add_map <- fread("SUA-FBS Balancing/Trade/Data/add_map.csv")
  
  add_map <- add_map[reporter_fao %in% fao_code_country[`M49 Code` == M49_code]$`Country Code`]
  
  # There are some corrections that need to be applied to the main trademap
  
  
  

#data("hsfclmap3", package = "hsfclmap", envir = environment())
# NOTE: it is pulling now v5
# FCL, startyear, endyear codes can be overwritten by corrections

hsfclmap3[!is.na(correction_fcl), fcl := correction_fcl]

hsfclmap3[!is.na(correction_startyear), startyear := correction_startyear]

hsfclmap3[!is.na(correction_endyear), endyear := correction_endyear]

hsfclmap3[, grep('correction', names(hsfclmap3)) := NULL]

##' 1. Extend the `endyear` for those combinations of `area` / `flow` /
##' `fromcode` / `tocode` for which `endyear` < `year`.

# Extend endyear to 2050
hsfclmap3[, maxy := max(endyear), .(area, flow, fromcode, tocode)]

hsfclmap3[, extend := ifelse(maxy < 2050, TRUE, FALSE), .(area, flow, fromcode, tocode)]

hsfclmap3[endyear == maxy & extend, endyear := 2050]

hsfclmap3[, c("maxy", "extend") := NULL]
# / Extend endyear to 2050


# Unmapped codes are added here:


# ADD UNMAPPED CODES


# Check that all FCL codes are valid

# fcl_codes <- as.numeric(ReadDatatable('fcl_2_cpc')$fcl)
# 
# write.csv(fcl_codes,"SUA-FBS Balancing/Trade/fcl_codes.csv",row.names = F)

fcl_codes <-as.vector(fread("SUA-FBS Balancing/Trade/Data/fcl_codes.csv")$x)


stopifnot(length(fcl_codes) > 0)



fcl_diff <- setdiff(unique(add_map$fcl), fcl_codes)


fcl_diff <- fcl_diff[!is.na(fcl_diff)]


fcl_diff <- setdiff(fcl_diff, 0)


if (length(fcl_diff) > 0) {
  warning(paste('Invalid FCL codes:', paste(fcl_diff, collapse = ', ')))
}


# Check that years are in a valid range


if (min(add_map$year) < 2000) {
  warning('The minimum year should not be lower than 2000.')
}


if (max(add_map$year) > as.numeric(format(Sys.Date(), '%Y'))) {
  warning('The maximum year should not be greater than the current year.')
}


# Check that there are no duplicate codes


if (nrow(add_map[, .N, .(reporter_fao, year, flow, hs)][N > 1]) > 0) {
  warning('Removing duplicate HS codes by reporter/year/flow.')
  
  add_map[, `:=`(n = .N, hs_ext_perc = sum(!is.na(hs_extend))/.N), .(reporter_fao, year, flow, hs)]
  
  
  # Prefer cases where hs_extend is available
  add_map <- add_map[hs_ext_perc == 0 | (hs_ext_perc > 0 & !is.na(hs_extend) & n == 1)]
  
  
  add_map[, c("n", "hs_ext_perc") := NULL]
}


# Raise warning if countries were NOT in mapping.


if (length(setdiff(unique(add_map$reporter_fao), hsfclmap3$area)) > 0) {
  warning('Some countries were not in the original mapping.')
}


add_map <-
  add_map[,
          .(area = reporter_fao,
            flow,
            fromcode = gsub(' ', '', hs),
            tocode = gsub(' ', '', hs),
            fcl = as.numeric(fcl),
            startyear = year,
            endyear = 2050L,
            recordnumb = NA_integer_,
            area_name = reporter_name
          )
          ]


max_record <- max(hsfclmap3$recordnumb)


add_map$recordnumb <- (max_record + 1):(max_record + nrow(add_map))


##' 1. Add additional codes that were not present in the HS-FCL
##' original mapping file.


hsfclmap3 <- rbind(add_map, hsfclmap3)


hsfclmap3[, `:=`(startyear = as.integer(startyear), endyear = as.integer(endyear))]


# / ADD UNMAPPED CODES

# Keep only codes for which `startyear` is less than current year and for which `endyear` is greater than current year:




##' 1. Keep HS-FCL links for which `startyear` <= `year` & `endyear` >= `year`
year <- as.integer("2016")

hsfclmap <- hsfclmap3[startyear <= year & endyear >= year]


hsfclmap3 <- tbl_df(hsfclmap3)


hsfclmap[, c("startyear", "endyear") := NULL]


# Workaround issue #123
hsfclmap[, fromgtto := as.numeric(fromcode) > as.numeric(tocode)]


from_gt_to <- hsfclmap$recordnumb[hsfclmap$fromgtto]


# if (length(from_gt_to) > 0)
#   flog.warn(paste0("In following records of hsfclmap fromcode greater than tocode: ",
#                    paste0(from_gt_to, collapse = ", ")))


hsfclmap <- hsfclmap[fromgtto == FALSE][, fromgtto := NULL]


stopifnot(nrow(hsfclmap) > 0)


hsfclmap <- tbl_df(hsfclmap)


# Generate an HS6 trademap, useful if no specific links are available:

if (generate_hs6mapping) {
  
  
  # hs6fclmap ####
  
  
  # flog.trace("[%s] Extraction of HS6 mapping table", PID, name = "dev")
  
  
  ##'     1. Universal (all years) HS6 mapping table.
  
  
  # flog.trace("[%s] Universal (all years) HS6 mapping table", PID, name = "dev")
  
  
  hs6fclmap_full <- extract_hs6fclmap(hsfclmap3, parallel = multicore)
  
  
  ##'     1. Current year specific HS6 mapping table.
  
  
  # flog.trace("[%s] Current year specific HS6 mapping table", PID, name = "dev")
  
  
  hs6fclmap_year <- extract_hs6fclmap(hsfclmap, parallel = multicore)
  
  
  hs6fclmap <- bind_rows(hs6fclmap_full, hs6fclmap_year) %>%
    filter_(~fcl_links == 1L) %>%
    distinct()
  
  
} else {
  
  
  # A dummy zero-row dataframe needs to be created
  hs6fclmap <-
    data_frame(
      reporter  = integer(),
      flow      = integer(),
      hs6       = integer(),
      fcl       = double(),
      fcl_links = integer()
    )
  
  
}

# trest
reportdir = "C:/Users/Siriwardenas/Documents/FBSTool-Updated-Current/SUA-FBS Balancing/Trade/"

tldata <- fread("SUA-FBS Balancing/Trade/Data/country_trade.csv")


# Extract HS6-FCL mapping table:


##'     1. Extract HS6-FCL mapping table.


tldatahs6links <- mapHS6toFCL(tldata, hs6fclmap)


# Extract specific HS-FCL mapping table:

##'     1. Extract specific HS-FCL mapping table.


tldata[,hs6 := as.integer(hs6)]

tldatalinks <- mapHS2FCL(tradedata   = tldata,
                         maptable    = hsfclmap3,
                         hs6maptable = hs6fclmap,
                         year        = year,
                         parallel    = multicore)

# Add FCL codes:


##'     1. Use HS6-FCL or HS-FCL mapping table.


tldata$map_src <- NA_character_


tldata <- add_fcls_from_links(tldata,
                              hs6links = tldatahs6links,
                              links    = tldatalinks)

setDT(tldata)

# If HS6 standard exists, use it:

##'     1. Use HS6 starndard for unmapped codes.


##' - `hs6standard`: HS6standard will be used as last resort for mapping.

# flog.trace("[%s] Reading in 'standard_hs12_6digit' datatable", PID, name = "dev")


# hs6standard <- ReadDatatable('standard_hs12_6digit')
# 
# 
# write.csv(hs6standard,"SUA-FBS Balancing/Trade/Data/hs6standard.csv",row.names = F)

hs6standard <- fread("SUA-FBS Balancing/Trade/Data/hs6standard.csv")


stopifnot(nrow(hs6standard) > 0)

hs6standard <-
  hs6standard[
    !duplicated(hs2012_code)
    ][,
      .(hs6 = as.integer(hs2012_code), hs2012_code, faostat_code)
      ]







tldata <- tldata %>%
  left_join(
    hs6standard %>% dplyr::select(-hs2012_code),
    by = 'hs6'
  ) %>%
  dplyr::mutate(
    fcl     = ifelse(is.na(fcl) & !is.na(faostat_code), faostat_code, fcl),
    map_src = ifelse(is.na(fcl) & !is.na(faostat_code), 'standard', map_src)
  ) %>%
  dplyr::select(-faostat_code)


setDT(tldata)


tldata[,fcl := addHeadingsFCL(fcl)]


hs_chapters <- c(sprintf("%02d", 01:24), 33, 35, 38, 40:41, 43, 50:53)



tldata$HSChapters = substring(tldata$hs,1,2)
tldata = tldata[HSChapters %in% hs_chapters,]

#hs codes that did not map 

codes_unmapped <- tldata[is.na(fcl)]

tldata <- tldata[!is.na(fcl)]

fcl2cpc <- fread("Trade/Data/fcl_2_cpc.csv")


fcl2cpc[, fcl := addHeadingsFCL(fcl2cpc$fcl)]

tldata=tldata[!is.na(fcl),]



tldata = merge(tldata,fcl2cpc,
               by = "fcl",
               all.x = TRUE)


tldata <- tldata[,c("Year","hs","QTY","flow","cpc"),with= F]


tldata[flow == 1, flow1 := "import"]
tldata[flow == 2, flow1 := "export"]


tldata[, flow:= NULL]

setnames(tldata, c("flow1"),c("flow"))
commodityName=read_excel("Data/Reference File.xlsx", "SUA_Commodities")
setDT(commodityName)


tldata <- merge(tldata,commodityName,by.x = c("cpc"),by.y = c("CPCCode"),all.x = TRUE)



missing_mapping <- tldata[is.na(cpc)]

tldata <- tldata[!is.na(cpc)]


write.xlsx(missing_mapping,"SUA-FBS Balancing/Trade/missing_mapping.xlsx",row.names = F)
write.xlsx(tldata,"SUA-FBS Balancing/Trade/mapped_tradeData.xlsx",row.names = F)





