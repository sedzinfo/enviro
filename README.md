# enviro
Displays data of pimoroni enviro from prometheus

![Alt text](https://github.com/sedzinfo/enviro/blob/main/screenshots/pimoroni_front.png)
![Alt text](https://github.com/sedzinfo/enviro/blob/main/screenshots/pimoroni_back.png)

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
## install RStudio IDE (optional but a good idea)  
installation instructions can be found here: https://posit.co/downloads/  

open RStudio and type in the console:
```
install.packages("devtools")
library(devtools)
install_github("sedzinfo/enviro")
```

# Usage
```
library(enviro)
```

substitute the url from your prometheus server  
```
pimoroni(prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range","http://pip1.crabdance.com:1505/api/v1/query_range"))
```

# Screenshots
![Alt text](https://github.com/sedzinfo/enviro/blob/main/screenshots/enviro1.png)
![Alt text](https://github.com/sedzinfo/enviro/blob/main/screenshots/enviro2.png)

---

![Stars](https://img.shields.io/github/stars/sedzinfo/enviro)
![Watchers](https://img.shields.io/github/watchers/sedzinfo/enviro)
![Repo Size](https://img.shields.io/github/repo-size/sedzinfo/enviro)  
![Open Issues](https://img.shields.io/github/issues/sedzinfo/enviro)
![Forks](https://img.shields.io/github/forks/sedzinfo/enviro)
![Last Commit](https://img.shields.io/github/last-commit/sedzinfo/enviro)
![Contributors](https://img.shields.io/github/contributors/sedzinfo/enviro)
![License](https://img.shields.io/github/license/sedzinfo/enviro)
![Release](https://img.shields.io/github/v/release/sedzinfo/enviro)
![Workflow Status](https://img.shields.io/github/actions/workflow/status/sedzinfo/enviro/main.yml)
