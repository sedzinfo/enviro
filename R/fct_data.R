##########################################################################################
#
##########################################################################################
#' Metrics that the Pimoroni Enviro and Enviro+ export to Prometheus
#'
#' @noRd
metrics <- c(
  "temperature", "humidity", "pressure", "lux", "proximity",
  "NH3", "reducing", "oxidising", "PM1", "PM25", "PM10"
)

#' Column names of the metrics in the data from get_data()
#'
#' @noRd
metric_names <- c(
  date = "Date", timestamp = "Timestamp", temperature = "Temperature",
  humidity = "Humidity", pressure = "Pressure", lux = "Lux", proximity = "Proximity",
  reducing = "Reducing", oxidising = "Oxidising", PM25 = "PM2.5"
)
##########################################################################################
#
##########################################################################################
#' Query one metric from Prometheus
#'
#' @param urlq Prometheus query_range url.
#' @param metric Name of the metric, one of `metrics`.
#' @param start_time,end_time Start and end time, in this format:
#'   "2025-01-31T11:48:08Z".
#' @param step Time between two readings, such as "10m".
#'
#' @return The `values` of the result: one matrix of times and values per
#'   series of the metric, or `NULL` when the answer cannot be read. An error
#'   when the server cannot be reached, takes longer than 15 seconds or
#'   answers with an error.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' enviro:::query_prometheus(
#'   "http://pip1.crabdance.com:1507/api/v1/query_range", "temperature",
#'   format(Sys.time() - 3600, "%Y-%m-%dT%H:%M:%SZ"), format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"), "1m"
#' )
#' }
query_prometheus <- function(urlq, metric, start_time, end_time, step) {
  response <- httr::GET(
    urlq,
    query = list(query = metric, start = start_time, end = end_time, step = step),
    httr::timeout(15)
  )
  httr::stop_for_status(response)
  result <- tryCatch(
    jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"), flatten = TRUE, simplifyDataFrame = TRUE),
    error = function(e) NULL
  )
  result$data$result$values
}
##########################################################################################
#
##########################################################################################
#' @title Get Data from Prometheus
#' @description Downloads the readings of all the sensors of a Pimoroni Enviro
#'   or Enviro+ from Prometheus, at about 1000 points in time between
#'   `start_time` and `end_time`.
#' @param start_time start time in this format: "2025-01-31T11:48:08Z"
#' @param end_time end time in this format: "2025-02-07T11:48:33Z"
#' @param urlq Prometheus url
#' @return A data frame with the Date and the Timestamp of the readings and one
#'   column per sensor, or `NULL`, with a message, when the server cannot be
#'   reached or sends no data. Sensors whose readings never change are left
#'   out, as the device does not have them.
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content
#' @keywords prometheus query
#' @export
#' @examples
#' \dontrun{
#' prometheus_url <- c(
#'   "http://pip1.crabdance.com:1507/api/v1/query_range",
#'   "http://pip1.crabdance.com:1505/api/v1/query_range"
#' )
#' start_time <- format(Sys.time() - 60 * 60 * 24 * 7, "%Y-%m-%dT%H:%M:%SZ")
#' end_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
#' filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[2])
#' head(filtered_df)
#' filtered_df <- get_data(start_time = start_time, end_time = end_time, urlq = prometheus_url[1])
#' head(filtered_df)
#' }
get_data <- function(start_time = format(Sys.time() - 60 * 60 * 24 * 7, "%Y-%m-%dT%H:%M:%SZ"),
                     end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"),
                     urlq) {
  time_step <- paste0(as.numeric(round(diff(seq(as.POSIXct(start_time), as.POSIXct(end_time), length.out = 1000)), 0), units = "mins"), "m")[1]
  xtsdf <- list()
  for (qm in metrics) {
    values <- tryCatch(query_prometheus(urlq, qm, start_time, end_time, time_step), error = function(e) e)
    # a server that is offline fails on the first metric: stop there instead of waiting for each one
    if (inherits(values, "error")) {
      message("Could not get data from ", urlq, ": ", conditionMessage(values))
      return(NULL)
    }
    if (length(values) == 0) next
    # a metric can come in several series, for example after a restart: keep them all
    series <- lapply(values, function(v) data.frame(timestamp = as.numeric(v[, 1]), value = as.numeric(v[, 2])))
    time_series_data <- do.call(rbind, series)
    if (length(series) > 1) {
      time_series_data <- stats::aggregate(time_series_data["value"], by = time_series_data["timestamp"], FUN = mean)
    }
    time_series_data <- data.frame(
      date = as.POSIXct(time_series_data$timestamp),
      timestamp = time_series_data$timestamp,
      value = time_series_data$value
    )
    names(time_series_data) <- c("date", "timestamp", qm)
    xtsdf[[qm]] <- time_series_data
  }
  if (length(xtsdf) == 0) {
    message("No data from ", urlq)
    return(NULL)
  }
  filtered_df <- dfr <- Reduce(function(x, y) merge(x, y, by = c("date", "timestamp"), all = TRUE), xtsdf)
  cn <- data.frame(names = names(dfr), sd = sapply(dfr, stats::sd, na.rm = TRUE))
  filtered_df[, cn[cn$sd %in% 0, ]$names] <- NULL
  renamed <- names(filtered_df) %in% names(metric_names)
  names(filtered_df)[renamed] <- metric_names[names(filtered_df)[renamed]]
  return(filtered_df)
}
