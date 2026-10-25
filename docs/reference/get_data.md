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
#>                  Date  Timestamp Temperature Humidity Pressure     Lux Proximity
#> 1 2026-10-18 08:39:31 1792301971    25.11664 38.70482 1013.145 1.77770        36
#> 2 2026-10-18 08:49:31 1792302571    24.16151 41.22523 1013.327 2.19250        38
#> 3 2026-10-18 08:59:31 1792303171    24.05667 42.19328 1013.342 2.50815        42
#> 4 2026-10-18 09:09:31 1792303771    24.98439 40.31372 1013.406 1.16185        41
#> 5 2026-10-18 09:19:31 1792304371    25.05374 40.18495 1013.511 1.16185        36
#> 6 2026-10-18 09:29:31 1792304971    24.93536 40.75222 1013.554 1.16185        40
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-18 08:39:31 1792301971    25.53017 37.39044 1012.041   0        11 83504.60 314526.3  96357.40  12    15   15
#> 2 2026-10-18 08:49:31 1792302571    25.88197 36.79866 1012.176   0         8 82875.58 312862.3  96475.25   9    13   13
#> 3 2026-10-18 08:59:31 1792303171    25.76259 37.22807 1012.169   0        11 82641.16 312310.9  96028.38   9    13   13
#> 4 2026-10-18 09:09:31 1792303771    25.82845 37.14981 1012.288   0         9 82485.32 311898.5  95841.01  14    20   20
#> 5 2026-10-18 09:19:31 1792304371    25.83447 37.36141 1012.360   0         6 83071.54 313692.4  95934.64  14    19   19
#> 6 2026-10-18 09:29:31 1792304971    25.76227 37.62339 1012.398   0        12 82174.68 311213.1  95514.22  13    17   17
```
