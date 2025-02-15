##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check enviro
# R CMD Rd2pdf enviro
# R CMD build enviro --resave-data
library(devtools)
library(roxygen2)
directory<-paste0(dirname(rstudioapi::getActiveDocumentContext()$path),"/")
setwd(directory)
# usethis::create_package("crypto")
document()
install()
library(enviro)
pimoroni()
