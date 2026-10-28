# Query one metric from Prometheus

Query one metric from Prometheus

## Usage

``` r
query_prometheus(urlq, metric, start_time, end_time, step)
```

## Arguments

- urlq:

  Prometheus query_range url.

- metric:

  Name of the metric, one of `metrics`.

- start_time, end_time:

  Start and end time, in UTC, in this format: "2025-01-31T11:48:08Z".

- step:

  Time between two readings, such as "10m".

## Value

The `values` of the result: one matrix of times and values per series of
the metric, or `NULL` when the answer cannot be read. An error when the
server cannot be reached, takes longer than 15 seconds or answers with
an error.

## Examples

``` r
if (FALSE) { # \dontrun{
golemenviro:::query_prometheus(
  "http://pip1.crabdance.com:1507/api/v1/query_range", "temperature",
  format(Sys.time() - 3600, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), "1m"
)
} # }
```
