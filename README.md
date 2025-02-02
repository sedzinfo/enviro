# enviro
Displays data of pimoroni enviro from prometheus

# Installation Instructions

install.packages("devtools")

library(devtools)

install_github("sedzinfo/enviro")

# Usage

library(enviro)

#' substitute the url from your prometheus server

pimoroni(prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range","http://pip1.crabdance.com:1505/api/v1/query_range"))


![Alt text]([https://github.com/sedzinfo/enviro/blob/main/script.png](https://github.com/sedzinfo/enviro/blob/main/enviro1.png))
