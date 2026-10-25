##########################################################################################
# DIRECTORIES
##########################################################################################
# R CMD check enviro
# R CMD Rd2pdf enviro
# R CMD build enviro --resave-data
library(devtools)
library(roxygen2)
rm(list = ls())
graphics.off()
cat("\014")
##########################################################################################
# FUNCTIONS PACKAGE
##########################################################################################
directory <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(directory)
# usethis::create_package("enviro")
document()
if ("enviro" %in% loadedNamespaces()) unloadNamespace("enviro") # an old version still loaded would make the reinstalled one look corrupt
if ("enviro" %in% rownames(installed.packages())) remove.packages("enviro")
install()
library(enviro)

pkgdown::build_site(getwd())
system("R CMD Rd2pdf . --force --no-preview --output=enviro.pdf")
if (.Platform$OS.type == "windows") shell.exec("enviro.pdf") else system("evince enviro.pdf")
system("R CMD build . --resave-data")

pimoroni(prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
                          "http://pip1.crabdance.com:1505/api/v1/query_range"))
