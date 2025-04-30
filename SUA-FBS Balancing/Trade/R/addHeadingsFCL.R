##' addHeadingsFCL
##'
##' Function to add headings to fcl codes in order to have 4 digits fcl codes
##' 
##' @export
##' 


addHeadingsFCL = function(measuredItemFCL){ 
  ifelse(nchar(measuredItemFCL) == 2, 
         paste0("00",measuredItemFCL, sep = ""),
         ifelse(nchar(measuredItemFCL) == 3,
                paste0("0",measuredItemFCL, sep = ""),
                measuredItemFCL))
}
