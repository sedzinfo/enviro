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
#> 1 2026-10-19 00:18:21 1792358301    25.33631 42.99191 1014.299   0        38
#> 2 2026-10-19 00:28:21 1792358901    25.60470 42.60104 1014.345   0        47
#> 3 2026-10-19 00:38:21 1792359501    25.58083 42.29672 1014.407   0        41
#> 4 2026-10-19 00:48:21 1792360101    25.51341 42.34920 1014.434   0        40
#> 5 2026-10-19 00:58:21 1792360701    25.52760 42.09767 1014.424   0        39
#> 6 2026-10-19 01:08:21 1792361301    25.61405 41.88133 1014.487   0        36
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-19 00:18:21 1792358301    28.64196 34.98857 1013.070   0        15 76866.00 268210.5  90036.45  14    20   20
#> 2 2026-10-19 00:28:21 1792358901    28.79428 34.74016 1013.133   0         6 77858.48 271441.9  90754.02  14    18   18
#> 3 2026-10-19 00:38:21 1792359501    28.80061 34.54373 1013.186   0        13 77405.52 270898.8  90754.02  15    18   18
#> 4 2026-10-19 00:48:21 1792360101    28.66824 34.67984 1013.182   0         7 77713.20 272314.5  91148.40  15    20   20
#> 5 2026-10-19 00:58:21 1792360701    28.81676 34.20689 1013.217   0         9 78259.64 274627.3  91611.20  12    17   18
#> 6 2026-10-19 01:08:21 1792361301    28.84494 34.11676 1013.238   0        11 78241.35 275517.0  91876.97  12    19   19
```
