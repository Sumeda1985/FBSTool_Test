#' Package Management
#' 
#' This module handles package dependencies for the Shiny application.
#' It ensures all required packages are installed and loaded.
#' 
#' @keywords internal
#' @importFrom utils installed.packages
#' @importFrom utils install.packages
#' @importFrom base require

#' Check and Install Required Packages
#' 
#' This function checks if all required packages are installed and loads them.
#' If any packages are missing, it will attempt to install them from CRAN.
#' 
#' @export
packageRequirevInstall <- function() {
  # List of required CRAN packages
  packagesCran <- c(
    # Shiny and UI components
    "shiny",
    "shinyBS",
    "shinydashboard",
    "shinyjs",
    "shinybusy",
    "shinythemes",
    "shinyWidgets",
    "shinyFiles",
    
    # Data manipulation and analysis
    "data.table",
    "dplyr",
    "tidyr",
    "reshape",
    "reshape2",
    "readr",
    "readxl",
    "stringr",
    "xtable",
    "xts",
    "zoo",
    
    # Database
    "RSQLite",
    "DBI",
    "dbplyr",
    
    # Visualization
    "ggplot2",
    "plotly",
    "dygraphs",
    "lattice",
    
    # Other utilities
    "devtools",
    "rhandsontable",
    "datasets",
    "openxlsx",
    "DT",
    "imputeTS",
    "V8",
    "igraph",
    "MASS",
    "countrycode"
  )
  
  # Check for missing packages
  missing_packages <- setdiff(packagesCran, rownames(installed.packages()))
  
  # Install missing packages
  if (length(missing_packages) > 0) {
    message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  }
  
  # Load all required packages
  lapply(packagesCran, require, character.only = TRUE)
}













