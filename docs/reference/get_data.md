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
#> 1 2026-10-18 08:54:20 1792302860    24.01893 41.89578 1013.339 1.77770        40
#> 2 2026-10-18 09:04:20 1792303460    24.88568 40.54769 1013.397 0.00000        41
#> 3 2026-10-18 09:14:20 1792304060    25.25019 39.95053 1013.396 1.16185        39
#> 4 2026-10-18 09:24:20 1792304660    25.08567 40.09132 1013.535 1.16185        44
#> 5 2026-10-18 09:34:20 1792305260    24.66214 42.20665 1013.535 1.16185        43
#> 6 2026-10-18 09:44:20 1792305860    24.83729 42.30642 1013.524 1.16185        34
filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-18 08:54:20 1792302860    25.70369 37.26172 1012.173   0        10 83741.95 315504.0  96522.44  10    12   12
#> 2 2026-10-18 09:04:20 1792303460    25.87183 36.92682 1012.203   0         9 82797.35 312862.3  96075.30  11    14   14
#> 3 2026-10-18 09:14:20 1792304060    25.71161 37.45098 1012.305   0        11 83445.39 314665.7  96122.24  14    19   20
#> 4 2026-10-18 09:24:20 1792304660    25.75151 37.52321 1012.432   0         7 82155.31 310939.7  95421.11  11    17   17
#> 5 2026-10-18 09:34:20 1792305260    26.00198 37.16017 1012.374   0        10 82155.31 309443.1  95397.85  11    14   14
#> 6 2026-10-18 09:44:20 1792305860    25.93168 37.22733 1012.373   0        11 83189.38 311761.2  95958.06  11    14   14
```
