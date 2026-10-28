# Data Visualization for Enviro Using Prometheus

Displays data of pimoroni enviro from prometheus

![Alt
text](https://github.com/sedzinfo/golemenviro/raw/main/screenshots/pimoroni_front.png)![Alt
text](https://github.com/sedzinfo/golemenviro/raw/main/screenshots/pimoroni_back.png)

## Examples of how to setup your own Pimoroni Enviro

[Getting Started with Enviro
Plus](https://learn.pimoroni.com/article/getting-started-with-enviro-plus)\
[Enviro Plus Python GitHub
Repository](https://github.com/pimoroni/enviroplus-python)

In order to create your own server you need to: 1. Install pimoroni
drivers in pi\
2. Deploy a prometheus server in pi\
3. After you deploy the server use the url as an argument in
prometheus_url (see bellow)

# Installation Instructions

## install R

installation instructions can be found here:
<https://cran.r-project.org/>\
\## install RStudio IDE (optional but a good idea)\
installation instructions can be found here:
<https://posit.co/downloads/>

open RStudio and type in the console:

    install.packages("devtools")
    library(devtools)
    install_github("sedzinfo/golemenviro")

# Usage

    library(golemenviro)

substitute the url from your prometheus server

    pimoroni(prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range","http://pip1.crabdance.com:1505/api/v1/query_range"))

When a server cannot be reached, its gauges and charts say so instead of
showing an error, and
[`get_data()`](https://sedzinfo.github.io/golemenviro/reference/get_data.md)
returns `NULL` with a message.

# Screenshots

![Alt
text](https://github.com/sedzinfo/golemenviro/raw/main/screenshots/enviro1.png)![Alt
text](https://github.com/sedzinfo/golemenviro/raw/main/screenshots/enviro2.png)

# Development

The package is a [golem](https://thinkr-open.github.io/golem/) app:

| File | Contents |
|----|----|
| `R/run_app.R` | [`pimoroni()`](https://sedzinfo.github.io/golemenviro/reference/pimoroni.md), which starts the app |
| `R/app_ui.R`, `R/app_server.R` | The navbar page and the server, which connect the modules |
| `R/mod_current.R` | The Current tab: a gauge with the latest reading of each sensor |
| `R/mod_timeseries.R` | The Time Series tab: a chart per sensor |
| `R/fct_data.R` | [`get_data()`](https://sedzinfo.github.io/golemenviro/reference/get_data.md), which downloads the readings from Prometheus |
| `R/fct_plots.R` | [`plot_time_series()`](https://sedzinfo.github.io/golemenviro/reference/plot_time_series.md) and [`plot_gauge()`](https://sedzinfo.github.io/golemenviro/reference/plot_gauge.md) |
| `inst/app/www/` | The CSS, the full screen button of the charts and the favicon |
| `tests/testthat/` | Tests, which run offline on saved Prometheus answers |

Run the tests with
[`devtools::test()`](https://devtools.r-lib.org/reference/test.html),
and the full package check with
[`devtools::check()`](https://devtools.r-lib.org/reference/check.html).

## Deploying to shinyapps.io

`app.R` starts the app with the two servers above; change them there for
your own. It also sets `analytics = TRUE`, which adds the cookie banner
of the sedzinfo.github.io sites and, once a visitor accepts, Google
Analytics;
[`pimoroni()`](https://sedzinfo.github.io/golemenviro/reference/pimoroni.md)
leaves them out unless asked, so running the app yourself sends nothing.
Run
[`rsconnect::deployApp()`](https://rstudio.github.io/rsconnect/reference/deployApp.html)
from the package folder, or open `app.R` in RStudio and click Publish.
`.rscignore` keeps the upload to the files the app needs.

------------------------------------------------------------------------

![Stars](https://img.shields.io/github/stars/sedzinfo/golemenviro)![Watchers](https://img.shields.io/github/watchers/sedzinfo/golemenviro)![Repo
Size](https://img.shields.io/github/repo-size/sedzinfo/golemenviro)![Open
Issues](https://img.shields.io/github/issues/sedzinfo/golemenviro)![Forks](https://img.shields.io/github/forks/sedzinfo/golemenviro)![Last
Commit](https://img.shields.io/github/last-commit/sedzinfo/golemenviro)![Contributors](https://img.shields.io/github/contributors/sedzinfo/golemenviro)![License](https://img.shields.io/github/license/sedzinfo/golemenviro)![Workflow
Status](https://img.shields.io/github/actions/workflow/status/sedzinfo/golemenviro/build-tarball.yml)![R-CMD-check](https://github.com/sedzinfo/golemenviro/actions/workflows/R-CMD-check.yaml/badge.svg)
