# Get Data from Prometheus

Get Data from Prometheus

## Usage

``` r
get_data(
  start_time = format(Sys.time() - 60 * 60 * 24 * 7, "%Y-%m-%dT%H:%M:%SZ"),
  end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
  urlq = prometheus_url[1]
)
```

## Arguments

- start_time:

  start time in this format: "2025-01-31T11:48:08Z"

- end_time:

  end time in this format: "2025-02-07T11:48:33Z"

- urlq:

  Prometheus url

## Examples

``` r
prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
                 "http://pip1.crabdance.com:1505/api/v1/query_range")
start_time<-format(Sys.time()-60*60*24*7,"%Y-%m-%dT%H:%M:%SZ")
end_time<-format(Sys.time(),"%Y-%m-%dT%H:%M:%SZ")
filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[2])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity
#> 1 2026-10-11 23:30:19 1791750619    30.45507 41.63572 1001.984   0        38
#> 2 2026-10-11 23:40:19 1791751219    30.45184 42.01431 1002.134   0        37
#> 3 2026-10-11 23:50:19 1791751819    30.42958 42.27456 1002.167   0        43
#> 4 2026-10-12 00:00:19 1791752419    30.26054 42.97205 1002.127   0        39
#> 5 2026-10-12 00:10:19 1791753019    29.83569 42.23877 1002.151   0        39
#> 6 2026-10-12 00:20:19 1791753619    28.94922 41.30058 1002.107   0        38
filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-11 23:30:19 1791750619    32.47493 35.33623 1000.805   0         8 71338.50 261321.3  81003.06   3     3    3
#> 2 2026-10-11 23:40:19 1791751219    32.60065 35.56675 1000.887   0        15 70846.85 259897.4  80490.79   3     4    4
#> 3 2026-10-11 23:50:19 1791751819    32.40811 36.13973 1001.006   0         7 70732.67 259594.0  80302.03   4     5    5
#> 4 2026-10-12 00:00:19 1791752419    32.58672 35.83836 1000.910   0         7 70586.18 259190.3  80245.51   2     3    4
#> 5 2026-10-12 00:10:19 1791753019    32.52402 32.34265 1000.933   0        10 73820.86 270790.5  82524.24   2     3    3
#> 6 2026-10-12 00:20:19 1791753619    31.51509 32.51835 1000.913   0         1 76937.69 275182.8  83386.23   1     3    3
```
