test_that("get_data() combines the sensors, joins a split series and drops the constant ones", {
  local_mocked_bindings(query_prometheus = answer_from(answers))
  data <- get_data("2023-11-14T22:13:20Z", "2023-11-14T23:00:00Z", urlq = "http://x")
  expect_equal(data, readings, ignore_attr = TRUE)
})

test_that("get_data() averages series of the same metric at the same time", {
  local_mocked_bindings(query_prometheus = answer_from(list(
    temperature = c(series(c(20, 21, 22, 23, 24)), series(c(22, 23, 24, 25, 26))),
    humidity = series(c(50, 51, 52, 53, 54))
  )))
  data <- get_data(urlq = "http://x")
  expect_equal(data$Temperature, c(21, 22, 23, 24, 25))
})

test_that("get_data() returns NULL with a message when the server cannot be reached", {
  local_mocked_bindings(query_prometheus = function(...) stop("Could not connect to server"))
  expect_message(data <- get_data(urlq = "http://x"), "Could not get data from http://x: Could not connect")
  expect_null(data)
})

test_that("get_data() stops at the first failed request instead of trying every metric", {
  requests <- 0
  local_mocked_bindings(query_prometheus = function(...) {
    requests <<- requests + 1
    stop("Timeout was reached")
  })
  expect_message(get_data(urlq = "http://x"))
  expect_equal(requests, 1)
})

test_that("get_data() returns NULL with a message when the server has no data", {
  local_mocked_bindings(query_prometheus = answer_from(list()))
  expect_message(data <- get_data(urlq = "http://x"), "No data from http://x")
  expect_null(data)
})
