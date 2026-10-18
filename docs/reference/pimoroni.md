# enviro

enviro

## Usage

``` r
pimoroni(prometheus_url, timezone = "Europe/Bucharest")
```

## Arguments

- prometheus_url:

  prometheus url

- timezone:

  the timezone prometheus exports data

## Examples

``` r
if (interactive()) pimoroni(prometheus_url=c("http://pip1.crabdance.com:1507/api/v1/query_range",
                          "http://pip1.crabdance.com:1505/api/v1/query_range"))
```
