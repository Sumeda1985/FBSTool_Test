# # This function checks wether the packages required for the shiny app are installed and apllies either require or 
# # install.packages from both Cran and GitHub
# 




packageRequirevInstall = function(){
  packagesCran = c("shiny", "data.table", "shinyBS", "devtools", "ggplot2", "xts", "dygraphs","rhandsontable","datasets",
                    "xtable","shinyFiles", "readxl", "stringr","dplyr","reshape2","readr","openxlsx","shinydashboard","DT","imputeTS", "shinyBS",
                   "igraph","zoo","shinyjs", "shinybusy","V8","reshape","shinythemes", "shinyWidgets", "plotly", "MASS", "lattice","tidyr","RSQLite")
  
# if (length(setdiff(packagesCran, rownames(installed.packages()))) > 0) {
    
#install.packages(setdiff(packagesCran, rownames(installed.packages()))) 
    
#  }
  lapply(packagesCran, require, character.only = T)
  
}













