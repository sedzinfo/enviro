# Plot Time Series

Plots the readings of one sensor, with their moving average, their mean,
and a summary of the last reading, mean, minimum, maximum, standard
deviation, range and dates on the right.

## Usage

``` r
plot_time_series(data, variable = "Temperature", scale = "", k = 30)
```

## Arguments

- data:

  data frame from get_data()

- variable:

  one of "Temperature" "Humidity" "Pressure" "Lux" "Proximity" "NH3"
  "Reducing" "Oxidising" "PM1" "PM2.5" "PM10"

- scale:

  title of the y axis

- k:

  integer width of the rolling window. Must be odd for rollmedian

## Value

A plotly object.

## Examples

``` r
if (FALSE) { # \dontrun{
prometheus_url <- c(
  "http://pip1.crabdance.com:1507/api/v1/query_range",
  "http://pip1.crabdance.com:1505/api/v1/query_range"
)
filtered_df <- get_data(urlq = prometheus_url[1])
plot_time_series(data = filtered_df, variable = "Temperature")
plot_time_series(data = filtered_df, variable = "Humidity")
plot_time_series(data = filtered_df, variable = "Pressure")
plot_time_series(data = filtered_df, variable = "PM2.5")
} # }
```
