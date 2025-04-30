getNutritiveFactors_ESN = function (nutrientDomain = NULL, nutrientDataset = NULL, 
          geographicAreaM49 = NULL, measuredElement = NULL, measuredItemCPC = NULL, 
          timePointYearsSP = NULL) 
{
  if (!exists("swsContext.datasets")) {
    stop("swsContext objects not defined!  Please run GetTestEnvironment.")
  }
  if (is.null(geographicAreaM49)) {
    geographicAreaM49 = GetCodeList(nutrientDomain, nutrientDataset, 
                                    "geographicAreaM49")[, code]
  }
  if (is.null(measuredElement)) {
    measuredElement = GetCodeList(nutrientDomain, nutrientDataset, 
                                  "measuredElement")[, code]
  }
  if (is.null(measuredItemCPC)) {
    measuredItemCPC = GetCodeList(nutrientDomain, nutrientDataset, 
                                  "measuredItemCPC")[, code]
  }
  if (is.null(timePointYearsSP)) {
    timePointYearsSP = GetCodeList(nutrientDomain, nutrientDataset, 
                                   "timePointYearsSP")[, code]
  }
  areaKeys = unique(c(geographicAreaM49, 0))
  yearKeys = unique(c(timePointYearsSP, 0))
  nutrientKey = DatasetKey(domain = nutrientDomain, dataset = nutrientDataset, 
                           dimensions = list(geographicAreaM49 = Dimension(name = "geographicAreaM49", 
                                                                           keys = areaKeys), measuredElement = Dimension(name = "measuredElement", 
                                                                                                                         keys = measuredElement), measuredItemCPC = Dimension(name = "measuredItemCPC", 
                                                                                                                                                                              keys = measuredItemCPC), timePointYearsSP = Dimension(name = "timePointYearsSP", 
                                                                                                                                                                                                                                    keys = yearKeys)))
  allData = GetData(nutrientKey)
  allData = allData[ , c(1,2,3,4,5), with=FALSE]
  
  output = allData[timePointYearsSP != "0" & geographicAreaM49 != 
                     "0", ]
  toInclude = allData[timePointYearsSP == "0" & geographicAreaM49 != 
                        "0", ]
  toInclude[, `:=`(timePointYearsSP, NULL)]
  toInclude[, `:=`(mergeDummy, 1)]
  yearDT = data.table(timePointYearsSP = yearKeys[yearKeys != 
                                                    "0"], mergeDummy = 1)
  toInclude = merge(toInclude, yearDT, by = "mergeDummy", allow.cartesian = TRUE)
  toInclude[, `:=`(mergeDummy, NULL)]
  output = merge(output, toInclude, all = TRUE, by = c("geographicAreaM49", 
                                                       "measuredElement", "measuredItemCPC", "timePointYearsSP"), 
                 suffixes = c("", ".new"))

    output[is.na(Value), `:=`(c("Value"), list(Value.new))]
 
  output[, `:=`(c("Value.new"), NULL)]
  toInclude = allData[timePointYearsSP != "0" & geographicAreaM49 == 
                        "0", ]
  toInclude[, `:=`(geographicAreaM49, NULL)]
  toInclude[, `:=`(mergeDummy, 1)]
  areaDT = data.table(geographicAreaM49 = areaKeys[areaKeys != 
                                                     "0"], mergeDummy = 1)
  toInclude = merge(toInclude, areaDT, by = "mergeDummy", allow.cartesian = TRUE)
  toInclude[, `:=`(mergeDummy, NULL)]
  output = merge(output, toInclude, all = TRUE, by = c("geographicAreaM49", 
                                                       "measuredElement", "measuredItemCPC", "timePointYearsSP"), 
                 suffixes = c("", ".new"))
  
    output[is.na(Value), `:=`(c("Value"), list(Value.new))]
 
  output[, `:=`(c("Value.new"), NULL)]
  toInclude = allData[timePointYearsSP == "0" & geographicAreaM49 == 
                        "0", ]
  toInclude[, `:=`(c("geographicAreaM49", "timePointYearsSP"), 
                   NULL)]
  toInclude[, `:=`(mergeDummy, 1)]
  toInclude = merge(toInclude, areaDT, by = "mergeDummy", allow.cartesian = TRUE)
  toInclude = merge(toInclude, yearDT, by = "mergeDummy", allow.cartesian = TRUE)
  toInclude[, `:=`(mergeDummy, NULL)]
  output = merge(output, toInclude, all = TRUE, by = c("geographicAreaM49", 
                                                       "measuredElement", "measuredItemCPC", "timePointYearsSP"), 
                 suffixes = c("", ".new"))
  
    output[is.na(Value), `:=`(c("Value"), list(Value.new))]
  
 
  output[, `:=`(c("Value.new"), NULL)]
  return(output)
}
