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
