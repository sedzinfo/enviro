# Current module - server

Downloads the readings of the last 7 days when the server changes, and
renders a gauge with the latest reading of each sensor. The gauges of
the sensors the device does not have stay empty, and when the server
cannot be reached the gauges say so.

## Usage

``` r
mod_current_server(id)
```

## Arguments

- id:

  Character. Module namespace id, must match the id used in
  [`mod_current_ui()`](https://sedzinfo.github.io/enviro/reference/mod_current_ui.md).

## Value

None. Called for its side effect of registering the module server.

## Examples

``` r
if (FALSE) { # \dontrun{
# inside app_server():
function(input, output, session) {
  mod_current_server("current")
}
} # }
```
