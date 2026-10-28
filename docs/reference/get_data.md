# Get Data from Prometheus

Downloads the readings of all the sensors of a Pimoroni Enviro or
Enviro+ from Prometheus, at about 1000 points in time between
`start_time` and `end_time`. When these are an hour or more apart (a
window of about 6 weeks or more), each point is the average of the
readings since the previous one, so that the day and night cycle does
not show up as noise; otherwise it is the reading at that time.

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
#> 1 2026-10-21 22:58:18 1792612698    27.03694 53.99060 1014.615   0        40
#> 2 2026-10-21 23:08:23 1792613303    26.99533 54.09473 1014.585   0        41
#> 3 2026-10-21 23:18:28 1792613908    26.95339 54.20467 1014.510   0        43
#> 4 2026-10-21 23:28:33 1792614513    26.90952 54.00459 1014.595   0        37
#> 5 2026-10-21 23:38:38 1792615118    26.65791 54.63457 1014.621   0        35
#> 6 2026-10-21 23:48:43 1792615723    27.00404 53.70305 1014.451   0        39
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-21 22:58:18 1792612698    29.33989 46.14522 1013.369   0        13 66954.09 247261.5  80755.93  34    42   43
#> 2 2026-10-21 23:08:23 1792613303    29.53432 45.63268 1013.312   0         8 66969.43 247448.3  80926.92  33    42   42
#> 3 2026-10-21 23:18:28 1792613908    29.54762 45.31915 1013.275   0         4 66969.43 248103.7  81003.06  29    37   38
#> 4 2026-10-21 23:28:33 1792614513    29.61887 45.13512 1013.342   0        12 67307.89 249044.9  81634.41  24    31   31
#> 5 2026-10-21 23:38:38 1792615118    29.52767 45.22928 1013.387   0         8 68006.04 250849.3  82524.24  20    26   26
#> 6 2026-10-21 23:48:43 1792615723    29.39404 45.63087 1013.257   0         9 67200.00 249233.8  81615.19  17    23   23
```
