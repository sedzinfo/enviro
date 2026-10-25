# Current module - UI

A server picker, the time the data was downloaded, and a gauge with the
latest reading of each sensor.

## Usage

``` r
mod_current_ui(id, prometheus_url)
```

## Arguments

- id:

  Character. Module namespace id.

- prometheus_url:

  Character vector of Prometheus urls to choose from.

## Value

A `tagList` of Shiny UI elements.

## Examples

``` r
if (FALSE) { # \dontrun{
shiny::fluidPage(
  golemenviro:::mod_current_ui("current", "http://pip1.crabdance.com:1507/api/v1/query_range")
)
} # }
```
