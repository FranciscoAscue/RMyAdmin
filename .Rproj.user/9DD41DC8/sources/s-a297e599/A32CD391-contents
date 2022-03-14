################################################################################
## ubuntu 20.04 / 21.10
## debian 10
# Install dependencies 
# . libprotobuf-dev
# . libjq-dev
# . protobuf-compiler
# . libudunits2-dev
# . libgdal-dev

################################################################################
# Install missing packages

missingPackages <- function(pkg){
    if( !is.element(pkg,rownames(installed.packages() ) ) ){
      message(pkg, "-----> Package is not installed ")
      if(pkg == "htmlwidgets"){
        install.packages(pkg, version = "1.5.4")
      }else{
        install.packages(pkg)
      }
    }
}
################################################################################

dependencies <- c("shiny","shinycssloaders","pheatmap","plotly","fossil",
                  "remotes","dplyr","DT","htmlwidgets","viridisLite","viridis",
                  "RColorBrewer")

################################################################################
# Package R dependencies
################################################################################
for(i in dependencies){
  missingPackages(i)
  library(i, character.only = TRUE)
}
library(splines)
library(writexl)
library(shinythemes)
library(stringr)
library(readr)
library(shinyjs)
################################################################################

