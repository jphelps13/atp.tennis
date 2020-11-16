
## 
loadPackages <- function(){
  
  packages <- c("data.table", "microbenchmark", "lubridate", "ggplot2", 
                "plotly", "ggrepel", "stringr", "zoo", "stringi",
                "shiny", "shinythemes", "Rtsne", "boot", "glmnet")
  
               
  for(i in packages){
    library(i, character.only = T)
  }
}

if(F){
  packages <- c("data.table", "microbenchmark", "tictoc",
                "profvis", "lubridate", "chron", "ggplot2", "plotly",
                "RODBC", "xgboost", "caret", "rcpp", "devtools")
  install.packages(packages)

}

sumn  <- function(x){sum(x, na.rm = T)}
meann <- function(x){mean(x, na.rm = T)}
