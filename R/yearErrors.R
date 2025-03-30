#' Validate Year Selection
#' 
#' This function validates the selected year against minimum and maximum allowed
#' years, ensuring proper sequencing of FBS compilation.
#' 
#' @param END_YEAR The selected end year
#' @param minyear The minimum allowed year
#' @param maxyear The maximum allowed year
#' @param session The Shiny session object
#' @importFrom shinyWidgets sendSweetAlert
#' @export
yearErrors <- function(END_YEAR, minyear, maxyear, session) {
  # Check if year is selected
  if (END_YEAR == "") {
    stop("Please select a year for which you want to compile data and FBS.")
  }
  
  # Check if year is after minimum allowed year
  if (minyear > END_YEAR) {
    warning(sprintf("Please select a year after %d.", minyear))
    return()
  }
  
  # Check if year is within allowed range
  if (END_YEAR > maxyear + 1) {
    END_YEAR <- as.numeric(END_YEAR)
    yearsToFill <- (maxyear + 1):END_YEAR
    
    if (length(yearsToFill) > 0) {
      missing_years <- paste(
        yearsToFill[1:(length(yearsToFill) - 1)],
        collapse = ", "
      )
      
      sendSweetAlert(
        session = session,
        title = "Error!",
        text = sprintf(
          "Please compile FBS for the year(s) %s first.",
          missing_years
        ),
        type = "error"
      )
    }
  }
}
