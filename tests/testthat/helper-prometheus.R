# Saved Prometheus answers, so the tests need neither the internet nor a Pimoroni

timestamps <- 1700000000 + 600 * 0:4

# the `values` of one series, as Prometheus sends them: times and values as text
series <- function(values, times = timestamps) {
  list(cbind(as.character(times), as.character(values)))
}

# three sensors, a constant one the device does not have, and PM2.5 split in two series
answers <- list(
  temperature = series(c(21, 21.5, 22, 22.5, 23)),
  humidity = series(c(50, 51, 52, 53, 54)),
  pressure = series(rep(1013, 5)),
  PM25 = c(series(c(5, 6), timestamps[1:2]), series(c(7, 8, 9), timestamps[3:5]))
)

answer_from <- function(answers) {
  function(urlq, metric, start_time, end_time, step, ...) answers[[metric]]
}

# what get_data() makes of these answers
readings <- data.frame(
  Date = as.POSIXct(timestamps),
  Timestamp = timestamps,
  Temperature = c(21, 21.5, 22, 22.5, 23),
  Humidity = c(50, 51, 52, 53, 54),
  PM2.5 = c(5, 6, 7, 8, 9)
)
