# Run the Pimoroni Enviro app

Starts a Shiny app with the readings of Pimoroni Enviro and Enviro+
sensors exported to Prometheus: gauges with the latest readings in the
Current tab, and a chart per sensor in the Time Series tab.

## Usage

``` r
pimoroni(
  prometheus_url,
  timezone = "Europe/Bucharest",
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
)
```

## Arguments

- prometheus_url:

  Character vector of Prometheus query_range urls, one per device, such
  as "http://pip1.crabdance.com:1507/api/v1/query_range".

- timezone:

  The time zone the readings are shown in, while the app runs.

- onStart:

  See
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- options:

  See
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- enableBookmarking:

  See
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- uiPattern:

  See
  [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html).

- ...:

  Further options passed to
  [`golem::with_golem_options()`](https://thinkr-open.github.io/golem/reference/with_golem_options.html).

## Value

A [`shiny::shinyApp()`](https://rdrr.io/pkg/shiny/man/shinyApp.html)
object. Printing it, or calling `pimoroni()` at the console, opens the
app in the browser.

## Examples

``` r
if (interactive()) {
  pimoroni(prometheus_url = c(
    "http://pip1.crabdance.com:1507/api/v1/query_range",
    "http://pip1.crabdance.com:1505/api/v1/query_range"
  ))
}
```
