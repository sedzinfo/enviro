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
#> 1 2026-10-11 21:19:18 1791742758    30.25538 52.78191 1001.672   0        43
#> 2 2026-10-11 21:29:18 1791743358    30.31313 52.63002 1001.631   0        47
#> 3 2026-10-11 21:39:18 1791743958    30.09183 53.66168 1001.872   0        37
#> 4 2026-10-11 21:49:18 1791744558    30.06409 54.95650 1001.870   0        34
#> 5 2026-10-11 21:59:18 1791745158    30.24893 57.26401 1002.004   0        40
#> 6 2026-10-11 22:09:18 1791745758    29.99021 50.38911 1002.114   0        45
filtered_df<-get_data(start_time=start_time,end_time=end_time,urlq=prometheus_url[1])
head(filtered_df)
#>                  Date  Timestamp Temperature Humidity Pressure     Lux Proximity      NH3 Reducing Oxidising PM1 PM2.5 PM10
#> 1 2026-10-11 21:19:18 1791742758    32.27796 45.58206 1000.439 0.00000        10 66541.34 249612.4  76206.57   6     9    9
#> 2 2026-10-11 21:29:18 1791743358    32.76564 44.45921 1000.418 0.00000        11 66373.98 249517.7  75395.81   6     8    8
#> 3 2026-10-11 21:39:18 1791743958    33.09214 43.82391 1000.560 0.00000         5 66328.41 247728.8  74924.55   6     6    6
#> 4 2026-10-11 21:49:18 1791744558    32.71529 45.00042 1000.664 1.16185        11 67015.48 250372.4  75747.09   5     7    7
#> 5 2026-10-11 21:59:18 1791745158    33.12222 43.84673 1000.699 0.00000        15 66602.31 248761.9  75116.14   6     8    9
#> 6 2026-10-11 22:09:18 1791745758    32.63929 39.78348 1000.796 0.00000         7 69235.07 259089.5  77369.42   5     7    7
```
