# Time Series module - server

Downloads the readings of the chosen number of days when the server or
the number of days changes, and renders a chart for each sensor. The
charts of the sensors the device does not have stay empty, and when the
server cannot be reached the charts say so.

## Usage

``` r
mod_timeseries_server(id)
```

## Arguments

- id:

  Character. Module namespace id, must match the id used in
  [`mod_timeseries_ui()`](https://sedzinfo.github.io/golemenviro/reference/mod_timeseries_ui.md).

## Value

None. Called for its side effect of registering the module server.

## Examples

``` r
if (FALSE) { # \dontrun{
# inside app_server():
function(input, output, session) {
  mod_timeseries_server("timeseries")
}
} # }
```
