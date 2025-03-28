#This function convert a nomarlized table to a denormalized table.
wide_format=function(data){
  data =  dcast.data.table(data, CPCCode+Commodity+ElementCode+Element ~ Year,
                           value.var = c("Value","Flag"))
  data[, (grep("^Flag", names(data), value=TRUE)) := lapply(.SD, as.character), 
       .SDcols = (grep("^Flag", names(data), value=TRUE))]
  data[, (grep("^Value", names(data), value=TRUE)):= lapply(.SD, as.numeric), 
       .SDcols = (grep("^Value", names(data), value=TRUE))]
  setcolorder(data,
              c("CPCCode", "Commodity", "ElementCode", "Element", 
                grep("^Value", names(data), value=TRUE),
                grep("^Flag", names(data), value=TRUE)))
  names(data)[grep("^Flag",names(data))] <- gsub("_"," ",
                                                 grep("^Flag", names(data), 
                                                      value=TRUE))
  names(data)[grep("^Value",names(data))] <- gsub("^.*?_","", 
                                                  grep("^Value", names(data), 
                                                       value=TRUE))
  return(data[order(CPCCode)])
}
