test_that("the time series tab draws the sensors the device has and leaves the others empty", {
  local_mocked_bindings(get_data = function(...) readings)
  testServer(mod_timeseries_server, {
    session$setInputs(server_timeseries = "http://x", days = "7")
    expect_no_error(output$plot_temperature)
    expect_no_error(output$plot_pm25)
    expect_error(output$plot_nh3)
  })
})

test_that("the current tab shows the last reading of each sensor", {
  local_mocked_bindings(get_data = function(...) readings)
  testServer(mod_current_server, {
    session$setInputs(server_gauge = "http://x")
    expect_equal(nrow(last_reading()), 1)
    expect_equal(last_reading()$Temperature, 23)
    expect_no_error(output$gauge_temperature)
  })
})

test_that("the current tab gives the gauges all the readings, not only the last", {
  rows <- 0
  local_mocked_bindings(
    get_data = function(...) readings,
    plot_gauge = function(data, variable) {
      rows <<- nrow(data)
      plotly::plot_ly(type = "indicator", mode = "number", value = 1)
    }
  )
  testServer(mod_current_server, {
    session$setInputs(server_gauge = "http://x")
    output$gauge_temperature
    expect_equal(rows, nrow(readings))
  })
})

test_that("the current tab shows when the last reading was taken, in red when it is old", {
  # the saved readings are from 2023
  local_mocked_bindings(get_data = function(...) readings)
  testServer(mod_current_server, {
    session$setInputs(server_gauge = "http://x")
    expect_match(output$last_date$html, "reading-old", fixed = TRUE)
    expect_match(output$last_date$html, "Last reading: 2023-11-1\\d .* days ago")
  })
  # the same readings, as if the last one had come 5 minutes ago
  local_mocked_bindings(get_data = function(...) transform(readings, Date = Sys.time() - 60 * c(45, 35, 25, 15, 5)))
  testServer(mod_current_server, {
    session$setInputs(server_gauge = "http://x")
    expect_match(output$last_date$html, "Last reading:", fixed = TRUE)
    expect_no_match(output$last_date$html, "reading-old", fixed = TRUE)
  })
})

test_that("the current tab downloads the readings again every 5 minutes", {
  downloads <- 0
  local_mocked_bindings(get_data = function(...) {
    downloads <<- downloads + 1
    readings
  })
  testServer(mod_current_server, {
    session$setInputs(server_gauge = "http://x")
    last_reading()
    session$elapse(4 * 60 * 1000)
    last_reading()
    expect_equal(downloads, 1)
    session$elapse(60 * 1000)
    last_reading()
    expect_equal(downloads, 2)
  })
})

test_that("both tabs say when the server cannot be reached", {
  local_mocked_bindings(get_data = function(...) NULL)
  testServer(mod_timeseries_server, {
    session$setInputs(server_timeseries = "http://x", days = "7")
    expect_error(output$plot_temperature, "The Prometheus server http://x cannot be reached right now")
  })
  testServer(mod_current_server, {
    session$setInputs(server_gauge = "http://x")
    expect_error(output$gauge_temperature, "The Prometheus server http://x cannot be reached right now")
  })
})

test_that("the time series tab downloads once per change of server or days", {
  downloads <- 0
  local_mocked_bindings(get_data = function(...) {
    downloads <<- downloads + 1
    readings
  })
  testServer(mod_timeseries_server, {
    session$setInputs(server_timeseries = "http://x", days = "1")
    for (days in c("2", "3", "4")) session$setInputs(days = days)
    expect_equal(downloads, 4)
  })
})
