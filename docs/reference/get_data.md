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
#> 1 2026-10-18 16:45:05 1792331105    25.43696 41.07952 1012.309 0.88715        41
#> 2 2026-10-18 16:55:05 1792331705    25.20599 41.59999 1012.303 0.88715        41
#> 3 2026-10-18 17:05:05 1792332305    25.09664 41.69924 1012.331 0.88715        39
#> 4 2026-10-18 17:15:05 1792332905    25.35212 41.30767 1012.247 0.88715        37
#> 5 2026-10-18 17:25:05 1792333505    25.39373 41.26089 1012.266 0.00000        38
#> 6 2026-10-18 17:35:05 1792334105    25.44954 41.16728 1012.278 0.00000        36
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-18 16:45:05 1792331105    26.65144 37.23526 1011.178   0        10 79982.34 288134.1  93537.25  11    13   13
#> 2 2026-10-18 16:55:05 1792331705    26.67645 37.32999 1011.165   0        10 77785.80 274849.3  92010.21   6     7    7
#> 3 2026-10-18 17:05:05 1792332305    26.78918 37.40204 1011.178   0         8 77045.36 271550.7  91478.68  17    26   27
#> 4 2026-10-18 17:15:05 1792332905    26.71224 37.64229 1011.090   0         9 76883.92 272095.9  91390.46   7     9   10
#> 5 2026-10-18 17:25:05 1792333505    26.75467 37.65329 1011.157   0         7 76955.62 272642.9  91346.39   8    10   10
#> 6 2026-10-18 17:35:05 1792334105    26.81420 37.53588 1011.144   0         9 78149.99 276075.5  91832.61   6     8    8
```
