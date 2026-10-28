test_that("get_data() combines the sensors, joins a split series and drops the constant ones", {
  local_mocked_bindings(query_prometheus = answer_from(answers))
  data <- get_data("2023-11-14T22:13:20Z", "2023-11-14T23:00:00Z", urlq = "http://x")
  expect_equal(data, readings, ignore_attr = TRUE)
})

test_that("get_data() asks for about 1000 readings, also in a window shorter than a day", {
  steps <- character()
  local_mocked_bindings(query_prometheus = function(urlq, metric, start_time, end_time, step, ...) {
    steps <<- c(steps, step)
    answers[[metric]]
  })
  get_data("2023-11-14T00:00:00Z", "2023-11-14T06:00:00Z", urlq = "http://x")
  get_data("2023-11-07T00:00:00Z", "2023-11-14T00:00:00Z", urlq = "http://x")
  expect_equal(unique(steps), c("22s", "605s"))
})

test_that("get_data() averages the readings over each step from steps of an hour", {
  averaged <- logical()
  local_mocked_bindings(query_prometheus = function(urlq, metric, start_time, end_time, step, average = FALSE) {
    averaged <<- c(averaged, average)
    answers[[metric]]
  })
  get_data("2023-08-14T00:00:00Z", "2023-11-14T00:00:00Z", urlq = "http://x") # 92 days, 2.2 hours apart
  expect_true(all(averaged))
  averaged <- logical()
  get_data("2023-10-14T00:00:00Z", "2023-11-14T00:00:00Z", urlq = "http://x") # 31 days, 45 minutes apart
  expect_false(any(averaged))
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
