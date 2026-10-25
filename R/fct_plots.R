##########################################################################################
#
##########################################################################################
#' Greek mu and sigma, and degrees Celsius
#'
#' Written as code points, as the R code of a package has to stay ASCII.
#'
#' @noRd
symbol_mean <- intToUtf8(0x03bc)
symbol_sd <- intToUtf8(0x03c3)
degrees_celsius <- paste0(intToUtf8(0x00b0), "C")
##########################################################################################
#
##########################################################################################
#' @title Plot Time Series
#' @description Plots the readings of one sensor, with their moving average,
#'   their mean, and a summary of the last reading, mean, minimum, maximum,
#'   standard deviation, range and dates on the right.
#' @param data data frame from get_data()
#' @param variable one of "Temperature" "Humidity" "Pressure" "Lux" "Proximity" "NH3" "Reducing" "Oxidising" "PM1" "PM2.5" "PM10"
#' @param scale title of the y axis
#' @param k integer width of the rolling window. Must be odd for rollmedian
#' @return A plotly object.
#' @import plotly
#' @importFrom zoo rollmean
#' @keywords plot timeseries
#' @export
#' @examples
#' \dontrun{
#' prometheus_url <- c(
#'   "http://pip1.crabdance.com:1507/api/v1/query_range",
#'   "http://pip1.crabdance.com:1505/api/v1/query_range"
#' )
#' filtered_df <- get_data(urlq = prometheus_url[1])
#' plot_time_series(data = filtered_df, variable = "Temperature")
#' plot_time_series(data = filtered_df, variable = "Humidity")
#' plot_time_series(data = filtered_df, variable = "Pressure")
#' plot_time_series(data = filtered_df, variable = "PM2.5")
#' }
plot_time_series <- function(data, variable = "Temperature", scale = "", k = 30) {
  smoothed <- zoo::rollmean(as.numeric(data[, variable]), k = k, align = "center", fill = NA)
  current_value <- round(as.numeric(data[nrow(data), variable]), 2)
  mean_value <- round(mean(as.numeric(data[, variable]), na.rm = TRUE), 2)
  min_value <- round(min(as.numeric(data[, variable]), na.rm = TRUE), 2)
  max_value <- round(max(as.numeric(data[, variable]), na.rm = TRUE), 2)
  sd_value <- round(stats::sd(as.numeric(data[, variable]), na.rm = TRUE), 2)
  range_value <- round(max_value - min_value, 2)
  plot_ts <- plot_ly(
    data = data,
    x = data[, "Date"],
    y = data[, variable],
    text = paste0("\nDate=", data[, "Date"], "\n", variable, "=", round(data[, variable], 2), "\n"),
    type = "scatter",
    mode = "lines",
    name = variable
  ) %>%
    add_lines(
      data = data,
      x = data[, "Date"],
      y = smoothed,
      text = paste0("\nDate=", data[, "Date"], "\n", variable, "=", round(smoothed, 2), "\n"),
      type = "scatter",
      mode = "lines",
      name = "Smoothing"
    ) %>%
    add_segments(
      x = min(data[, "Date"]),
      xend = max(data[, "Date"]),
      y = mean_value,
      yend = mean_value,
      name = "Mean"
    ) %>%
    add_annotations(
      text = paste(
        "\n\n",
        "last:", current_value, "\n\n",
        paste0(symbol_mean, ":"), mean_value, "\n",
        "min:", min_value, "\n",
        "max:", max_value, "\n",
        paste0(symbol_sd, ":"), sd_value, "\n",
        "range:", range_value, "\n\n",
        "Start:", min(format(data[, "Date"], "%Y-%m-%d")), "\n",
        "Stop:", max(format(data[, "Date"], "%Y-%m-%d")), "\n"
      ),
      xref = "paper",
      yref = "paper",
      x = 1.02,
      y = 0.1,
      xanchor = "left",
      yanchor = "bottom",
      showarrow = FALSE,
      font = list(size = 12),
      align = "left"
    ) %>%
    layout(
      autosize = TRUE,
      margin = list(l = 80, r = 80, b = 80, t = 50, pad = 50),
      legend = list(orientation = "v", xanchor = "left"),
      title = variable,
      xaxis = list(title = ""),
      yaxis = list(title = scale),
      template = "plotly_dark"
    )
  return(plot_ts)
}
##########################################################################################
#
##########################################################################################
#' @title Plot Gauge
#' @description Plots the last reading of one sensor as a gauge, with the
#'   ideal range in green and a short note on what the gas and particle
#'   sensors measure.
#' @param data data frame from get_data()
#' @param variable one of "Temperature" "Humidity" "Pressure" "Lux" "Proximity" "NH3" "Reducing" "Oxidising" "PM1" "PM2.5" "PM10"
#' @return A plotly object.
#' @import plotly
#' @keywords plot gauge
#' @export
#' @examples
#' \dontrun{
#' prometheus_url <- c(
#'   "http://pip1.crabdance.com:1507/api/v1/query_range",
#'   "http://pip1.crabdance.com:1505/api/v1/query_range"
#' )
#' filtered_df <- get_data(urlq = prometheus_url[1])
#' plot_gauge(data = filtered_df, variable = "Temperature")
#' plot_gauge(data = filtered_df, variable = "Humidity")
#' plot_gauge(data = filtered_df, variable = "PM2.5")
#' }
plot_gauge <- function(data, variable = "Temperature") {
  help_pm1 <- "Ultra fine particles"
  help_pm25 <- "Combustion particles,Organic compounds,Metals"
  help_pm10 <- "Dust,Pollen,Mould spores"
  help_NH3 <- "Mostly Ammonia,\n Hydrogen,Ethanol,Propane,Iso-butane"
  help_reducing <- "Mostly Carbon Monoxide,\n H2S,Ammonia,Ethanol,Hydrogen,Methane,Propane,Iso-butane"
  help_oxidising <- "Mostly Nitrogen Dioxide,\n NO,Hydrogen"
  names <- c(
    "Temperature", "Humidity", "Pressure", "Lux", "Proximity",
    "NH3", "Reducing", "Oxidising", "PM1", "PM2.5", "PM10"
  )
  scale <- c(degrees_celsius, "%", "hPa", "Lux", NA, "Ohms", "Ohms", "Ohms", "ug/m3", "ug/m3", "ug/m3")
  range_ideal_min <- c(20, 40, 1010, 1000, 0, NA, NA, NA, 0, 0, 0)
  range_ideal_max <- c(25, 60, 1015, 1500, 1, NA, NA, NA, 20, 20, 20)
  point_ideal <- c(24, 50, 1013, 1200, NA, NA, NA, NA, 0, 0, 0)
  range_min <- c(-50, 0, 900, 0, 0, NA, NA, NA, 0, 0, 0)
  range_max <- c(50, 100, 1100, 10000, 10, NA, NA, NA, 400, 400, 400)
  help <- c(rep("", 5), help_NH3, help_reducing, help_oxidising, help_pm1, help_pm25, help_pm10)
  df_reference <- data.frame(
    query = metrics, names = names, scale, range_min, range_max,
    range_ideal_min, range_ideal_max, point_ideal, help = help
  )
  dflast <- data[data$Timestamp %in% max(data$Timestamp), ]
  last_value <- dflast[, variable]
  value_mean <- mean(dflast[, variable], na.rm = TRUE)
  value_sd <- stats::sd(dflast[, variable], na.rm = TRUE)
  value_max <- max(dflast[, variable])
  value_min <- min(dflast[, variable])
  reference <- df_reference[df_reference$names %in% variable, ]

  range <- c(reference$range_min, reference$range_max)
  range_ideal <- c(reference$range_ideal_min, reference$range_ideal_max)
  point_ideal <- reference$point_ideal

  if (anyNA(range)) {
    range <- c(value_min - value_sd, value_max + value_sd)
  }
  if (anyNA(range_ideal)) {
    range_ideal <- c(value_mean - value_sd, value_mean + value_sd)
  }
  if (is.na(point_ideal)) {
    point_ideal <- value_mean
  }

  plot_g <- plot_ly(
    type = "indicator",
    mode = "gauge+number+delta",
    value = last_value,
    number = list(suffix = reference$scale),
    title = list(text = variable, font = list(size = 20)),
    delta = list(
      reference = point_ideal,
      increasing = list(color = "#404444"),
      decreasing = list(color = "#404444")
    ),
    gauge = list(
      axis = list(range = range, tickwidth = 1, tickcolor = "darkblue"),
      bar = list(color = "red"),
      bgcolor = "white",
      borderwidth = 2,
      bordercolor = "gray",
      steps = list(list(range = range_ideal, color = "green")),
      threshold = list(
        line = list(color = "darkblue", width = 1),
        thickness = 5,
        value = point_ideal
      )
    )
  ) %>%
    layout(
      margin = list(l = 80, r = 80, b = 80, t = 50, pad = 50),
      paper_bgcolor = "white",
      font = list(color = "darkblue")
    ) %>%
    add_annotations(
      text = reference$help,
      x = .5,
      y = -.1,
      showarrow = FALSE
    )
  return(plot_g)
}
