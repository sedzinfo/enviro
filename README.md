# enviro
Displays data of pimoroni enviro from prometheus

## Examples of how to setup your own Pimoroni Enviro
[Getting Started with Enviro Plus](https://learn.pimoroni.com/article/getting-started-with-enviro-plus)  
[Enviro Plus Python GitHub Repository](https://github.com/pimoroni/enviroplus-python)  

In order to create your own server you need to: 
1. Install pimoroni drivers in pi  
2. Deploy a prometheus server in pi  
3. After you deploy the server use the url as an argument in prometheus_url (see bellow)  

# Installation Instructions
## install R
installation instructions can be found here: https://cran.r-project.org/  
## install rstudio (optional but a good idea)  
installation instructions can be found here: https://posit.co/downloads/  

open R studio and type in the console:
```
install.packages("devtools")  
library(devtools)  
install_github("sedzinfo/enviro")  
```

# Usage
```
library(enviro)
```

#' substitute the url from your prometheus server  
```
pimoroni(prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range","http://pip1.crabdance.com:1505/api/v1/query_range"))
```

# Screenshots
![Alt text](https://github.com/sedzinfo/enviro/blob/main/enviro1.png)
![Alt text](https://github.com/sedzinfo/enviro/blob/main/enviro2.png)
