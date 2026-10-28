##########################################################################################
#
##########################################################################################
#' Time Series module - UI
#'
#' A server picker, the period to show (6 hours to 1 year), and a chart for each sensor,
#' with a full screen button (see `inst/app/www/plot_zoom.js`).
#'
#' @param id Character. Module namespace id.
#' @param prometheus_url Character vector of Prometheus urls to choose from.
#'
#' @return A `tagList` of Shiny UI elements.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' shiny::fluidPage(
#'   golemenviro:::mod_timeseries_ui("timeseries", "http://pip1.crabdance.com:1507/api/v1/query_range")
#' )
#' }
mod_timeseries_ui <- function(id, prometheus_url) {
  ns <- NS(id)
  chart <- function(width, output) {
    column(width = width, class = "sensor-box", output)
  }
  # the periods to choose from, in days
  periods <- c(
    "6 hours" = 0.25, "12 hours" = 0.5, "24 hours" = 1, "3 days" = 3, "1 week" = 7, "2 weeks" = 14,
    "1 month" = 30, "3 months" = 90, "6 months" = 180, "9 months" = 270, "1 year" = 365
  )
  tagList(
    fluidRow(
      column(width = 4, selectInput(ns("server_timeseries"), label = "Server", choices = c(prometheus_url), selected = prometheus_url[1], width = "600px")),
      column(width = 4, selectInput(ns("days"), label = "Period", choices = periods, selected = 7))
    ),
    fluidRow(div(
      class = "plotly-full-screen",
      chart(6, plotly::plotlyOutput(ns("plot_temperature"))),
      chart(6, plotly::plotlyOutput(ns("plot_humidity"))),
      chart(6, plotly::plotlyOutput(ns("plot_pressure"))),
      chart(3, plotly::plotlyOutput(ns("plot_lux"))),
      chart(3, plotly::plotlyOutput(ns("plot_proximity"))),
      chart(4, uiOutput(ns("plot_nh3"))),
      chart(4, uiOutput(ns("plot_reducing"))),
      chart(4, uiOutput(ns("plot_oxidising"))),
      chart(4, uiOutput(ns("plot_pm1"))),
      chart(4, uiOutput(ns("plot_pm25"))),
      chart(4, uiOutput(ns("plot_pm10")))
    ))
  )
}
##########################################################################################
#
##########################################################################################
#' Time Series module - server
#'
#' Downloads the readings of the chosen number of days when the server or the
#' number of days changes, and renders a chart for each sensor. The charts of
#' the sensors the device does not have stay empty, and when the server
#' cannot be reached the charts say so.
#'
#' @param id Character. Module namespace id, must match the id used in
#'   [mod_timeseries_ui()].
#'
#' @return None. Called for its side effect of registering the module server.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # inside app_server():
#' function(input, output, session) {
#'   mod_timeseries_server("timeseries")
#' }
#' }
mod_timeseries_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    timeseries_data <- reactive({
      start_time <- format(Sys.time() - as.numeric(input$days) * 60 * 60 * 24, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
      get_data(start_time = start_time, end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"), urlq = input$server_timeseries)
    })
    plot_output <- function(variable, scale = "", render = plotly::renderPlotly) {
      render({
        filtered_df <- timeseries_data()
        validate(need(filtered_df, paste("The Prometheus server", input$server_timeseries, "cannot be reached right now")))
        req(variable %in% names(filtered_df))
        plot_time_series(data = filtered_df, variable = variable, scale = scale) %>% plotly::as_widget()
      })
    }
    scale_pm <- "micrograms per cubic metre (ug/m3)"
    scale_g <- "Ohms"
    output$plot_temperature <- plot_output("Temperature", scale = degrees_celsius)
    output$plot_humidity <- plot_output("Humidity", scale = "Relative humidity (%)")
    output$plot_pressure <- plot_output("Pressure", scale = "hPa")
    output$plot_lux <- plot_output("Lux", scale = "LUX")
    output$plot_proximity <- plot_output("Proximity")
    output$plot_pm1 <- plot_output("PM1", scale = scale_pm, render = renderUI)
    output$plot_pm25 <- plot_output("PM2.5", scale = scale_pm, render = renderUI)
    output$plot_pm10 <- plot_output("PM10", scale = scale_pm, render = renderUI)
    output$plot_nh3 <- plot_output("NH3", scale = scale_g, render = renderUI)
    output$plot_reducing <- plot_output("Reducing", scale = scale_g, render = renderUI)
    output$plot_oxidising <- plot_output("Oxidising", scale = scale_g, render = renderUI)
  })
}
