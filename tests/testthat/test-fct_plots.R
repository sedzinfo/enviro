test_that("the time series and the gauges build for every sensor", {
  for (variable in c("Temperature", "Humidity", "PM2.5")) {
    expect_no_error(plotly::plotly_build(plot_time_series(readings, variable, k = 3)))
    expect_no_error(plotly::plotly_build(plot_gauge(readings, variable)))
  }
})

test_that("a gauge without an ideal range shows the last reading", {
  gas <- transform(readings, NH3 = c(100, 110, 120, 130, 140))
  gauge <- plotly::plotly_build(plot_gauge(gas, "NH3"))$x$data[[1]]
  expect_equal(gauge$value, 140)
})

test_that("the summary of a time series shows the mean and the standard deviation", {
  plot <- plotly::plotly_build(plot_time_series(readings, "Temperature", k = 3))
  summary <- plot$x$layout$annotations[[1]]$text
  expect_match(summary, paste0(intToUtf8(0x03bc), ": 22"), fixed = TRUE)
  expect_match(summary, paste0(intToUtf8(0x03c3), ":"), fixed = TRUE)
})
