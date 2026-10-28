# Get Data from Prometheus

Downloads the readings of all the sensors of a Pimoroni Enviro or
Enviro+ from Prometheus, at about 1000 points in time between
`start_time` and `end_time`.

## Usage

``` r
get_data(
  start_time = format(Sys.time() - 60 * 60 * 24 * 7, "%Y-%m-%dT%H:%M:%SZ", tz =
    "UTC"),
  end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  urlq
)
```

## Arguments

- start_time:

  start time, in UTC, in this format: "2025-01-31T11:48:08Z"

- end_time:

  end time, in UTC, in this format: "2025-02-07T11:48:33Z"

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
start_time <- format(Sys.time() - 60 * 60 * 24 * 7, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[2])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity
#> 1 2026-10-21 22:52:56 1792612376    27.07920 53.82797 1014.632   0        40
#> 2 2026-10-21 23:03:01 1792612981    26.97855 54.14105 1014.547   0        44
#> 3 2026-10-21 23:13:06 1792613586    26.95436 54.16960 1014.544   0        38
#> 4 2026-10-21 23:23:11 1792614191    26.91985 54.23880 1014.508   0        38
#> 5 2026-10-21 23:33:16 1792614796    27.01339 53.56875 1014.586   0        42
#> 6 2026-10-21 23:43:21 1792615401    27.00404 53.60360 1014.573   0        43
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-21 22:52:56 1792612376    29.36142 46.08954 1013.362   0        10 67617.21 248479.5  81117.42  35    45   46
#> 2 2026-10-21 23:03:01 1792612981    29.57897 45.40917 1013.270   0        10 67107.67 247916.1  81212.86  32    39   40
#> 3 2026-10-21 23:13:06 1792613586    29.38897 45.88277 1013.294   0         9 67663.74 249897.0  81098.34  29    34   35
#> 4 2026-10-21 23:23:11 1792614191    29.36269 45.73684 1013.293   0         9 67928.08 250944.9  81576.77  24    30   30
#> 5 2026-10-21 23:33:16 1792614796    29.49980 45.34098 1013.338   0        14 67292.47 248761.9  81730.58  24    30   30
#> 6 2026-10-21 23:43:21 1792615401    29.48745 45.36323 1013.338   0        12 67215.40 248667.7  82077.89  17    23   23
```
