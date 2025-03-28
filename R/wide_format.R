#This function convert a nomarlized table to a denormalized table.

wide_format=function(data){
data =  dcast.data.table(data, CPCCode+Commodity+ElementCode+Element ~ Year,
                           value.var = c("Value","Flag"))
data[, (names(data)[grep("^Flag",names(data))]) := lapply(.SD, as.character), .SDcols = 
       (names(data)[grep("^Flag",names(data))])]
data[, (names(data)[grep("^Value",names(data))]):= lapply(.SD, as.numeric), 
     .SDcols = (names(data)[grep("^Value",names(data))])]
addorder <- as.vector(rbind(names(data)[grep("^Value",names(data))],
                            names(data)[grep("^Flag",names(data))] ))
setcolorder(data,
            c("CPCCode", "Commodity", "ElementCode", "Element", addorder))
names(data)[grep("^Flag",names(data))] <- gsub("_"," ",grep("^Flag", names(data), value=TRUE))
names(data)[grep("^Value",names(data))] <- gsub("^.*?_","", grep("^Value", names(data), value=TRUE))
data[order(CPCCode)]
return(data)
}
