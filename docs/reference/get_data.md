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
#> 1 2026-10-21 22:38:49 1792611529    27.02985 54.14835 1014.642   0        39
#> 2 2026-10-21 22:48:54 1792612134    26.98630 54.22900 1014.590   0        39
#> 3 2026-10-21 22:58:59 1792612739    27.01855 54.19482 1014.612   0        41
#> 4 2026-10-21 23:09:04 1792613344    27.01985 54.00182 1014.589   0        41
#> 5 2026-10-21 23:19:09 1792613949    26.95855 53.90068 1014.502   0        40
#> 6 2026-10-21 23:29:14 1792614554    26.91694 53.71237 1014.607   0        41
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-21 22:38:49 1792611529    29.44945 46.19716 1013.394   0        12 67061.56 247168.3  80964.98  47    60   61
#> 2 2026-10-21 22:48:54 1792612134    29.31741 46.30724 1013.329   0        11 67092.29 247728.8  80717.99  39    47   47
#> 3 2026-10-21 22:58:59 1792612739    29.23222 46.31160 1013.360   0        16 66862.13 246888.8  80717.99  35    43   43
#> 4 2026-10-21 23:09:04 1792613344    29.53970 45.50391 1013.323   0        12 67648.22 249328.4  81231.97  31    37   37
#> 5 2026-10-21 23:19:09 1792613949    29.55554 45.24082 1013.259   0        21 67107.67 248291.4  81117.42  28    34   37
#> 6 2026-10-21 23:29:14 1792614554    29.48809 45.38004 1013.375   0         9 67277.05 249044.9  81788.34  23    27   27
```
