
yearErrors = function(END_YEAR, minyear, maxyear, session){

if (END_YEAR == ""){
  
  stop("Please select a year for which you want to compile data and FBS.")

}


if(minyear > END_YEAR){
  print(paste("Please select a year after ", minyear, ".", sep = ""))
}


if(END_YEAR > maxyear +1){
  END_YEAR=as.numeric(END_YEAR)
   yearsToFill = (maxyear + 1):END_YEAR
   if(length(yearsToFill) > 0){
     # stop(paste("Please compile FBS for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""))
     
     sendSweetAlert(
       session = session,
       title = "Error!!",
       text = paste("Please compile FBS for the year(s) ",paste(yearsToFill[1:(length(yearsToFill)-1)],collapse = ", ") , " first.", sep = ""),
       type = "error"
     )
     
     
     
   }
   
   
   
 
   # else if (END_YEAR == maxyear +1){
   #   
   #   
   # } else{
   #   stop(paste("Please compile FBS for the year ", 
   #              paste(yearsToFill[1:(length(yearsToFill)-1)], 
   #              collapse = ", "), " first before compiling FBS for ",END_YEAR,".", sep = ""))
   # }
}
}
  