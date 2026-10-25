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
#> 1 2026-10-18 16:56:22 1792331782    25.20987 41.54152 1012.315 0.88715        42
#> 2 2026-10-18 17:06:22 1792332382    25.14277 41.55315 1012.305 0.88715        41
#> 3 2026-10-18 17:16:22 1792332982    25.41857 41.24334 1012.242 0.88715        36
#> 4 2026-10-18 17:26:22 1792333582    25.48018 41.14388 1012.247 0.88715        46
#> 5 2026-10-18 17:36:22 1792334182    25.42083 41.30770 1012.280 0.00000        36
#> 6 2026-10-18 17:46:22 1792334782    25.19729 41.67599 1012.339 0.00000        41
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-18 16:56:22 1792331782    26.66695 37.38025 1011.168   0         5 77369.42 271441.9  91743.97   5     7    7
#> 2 2026-10-18 17:06:22 1792332382    26.72934 37.49716 1011.146   0         8 76901.83 271441.9  91478.68  10    18   18
#> 3 2026-10-18 17:16:22 1792332982    26.73060 37.58643 1011.103   0         6 76973.56 272533.3  91412.50   8    10   10
#> 4 2026-10-18 17:26:22 1792333582    26.78285 37.56390 1011.111   0        12 76901.83 272533.3  91302.35   8    11   11
#> 5 2026-10-18 17:36:22 1792334182    26.66157 37.80984 1011.190   0        11 77153.20 273632.1  91324.36   6     7    7
#> 6 2026-10-18 17:46:22 1792334782    26.86455 37.56358 1011.206   0        14 77261.22 274405.6  91434.55   7     8    8
```
