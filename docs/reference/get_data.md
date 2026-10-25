# Get Data from Prometheus

Downloads the readings of all the sensors of a Pimoroni Enviro or
Enviro+ from Prometheus, at about 1000 points in time between
`start_time` and `end_time`.

## Usage

``` r
get_data(
  start_time = format(Sys.time() - 60 * 60 * 24 * 7, "%Y-%m-%dT%H:%M:%SZ"),
  end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
  urlq
)
```

## Arguments

- start_time:

  start time in this format: "2025-01-31T11:48:08Z"

- end_time:

  end time in this format: "2025-02-07T11:48:33Z"

- urlq:

  Prometheus url

## Value

A data frame with the Date and the Timestamp of the readings and one
column per sensor, or `NULL`, with a message, when the server cannot be
reached or sends no data. Sensors whose readings never change are left
out, as the device does not have them.

## Examples

``` r
prometheus_url <- c(
  "http://pip1.crabdance.com:1507/api/v1/query_range",
  "http://pip1.crabdance.com:1505/api/v1/query_range"
)
start_time <- format(Sys.time() - 60 * 60 * 24 * 7, "%Y-%m-%dT%H:%M:%SZ")
end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[2])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity
#> 1 2026-10-18 18:14:16 1792336456    25.13277 42.15526 1012.445   0        34
#> 2 2026-10-18 18:24:16 1792337056    25.27922 41.88077 1012.478   0        37
#> 3 2026-10-18 18:34:16 1792337656    25.40728 41.76985 1012.530   0        42
#> 4 2026-10-18 18:44:16 1792338256    25.21341 42.12622 1012.702   0        40
#> 5 2026-10-18 18:54:16 1792338856    25.23148 42.13796 1012.794   0        41
#> 6 2026-10-18 19:04:16 1792339456    25.15825 42.28977 1012.872   0        33
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-18 18:14:16 1792336456    26.68690 38.00500 1011.278   0         9 77315.30 275851.9  91500.75  10    13   13
#> 2 2026-10-18 18:24:16 1792337056    26.60806 38.04985 1011.368   0        11 77931.24 277310.8  91832.61   8    10   10
#> 3 2026-10-18 18:34:16 1792337656    26.63212 37.98843 1011.419   0        10 78113.48 277762.3  91788.27   6     7    7
#> 4 2026-10-18 18:44:16 1792338256    26.63149 38.16691 1011.530   0         9 76615.72 271877.6  90994.78  23    36   37
#> 5 2026-10-18 18:54:16 1792338856    26.58779 38.11126 1011.585   0         7 77207.19 273521.9  91302.35   8    10   10
#> 6 2026-10-18 19:04:16 1792339456    26.64447 38.04417 1011.711   0         8 76544.38 271441.9  90994.78   6    10   10
```
