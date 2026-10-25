##########################################################################################
#
##########################################################################################
#' Current module - UI
#'
#' A server picker, the time the data was downloaded, and a gauge with the
#' latest reading of each sensor.
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
#'   enviro:::mod_current_ui("current", "http://pip1.crabdance.com:1507/api/v1/query_range")
#' )
#' }
mod_current_ui <- function(id, prometheus_url) {
  ns <- NS(id)
  gauge <- function(output) {
    shinydashboard::box(width = 4, style = "background-color: #F8F8F8; padding-top: 20px;", output)
  }
  tagList(
    fluidRow(
      column(width = 8, selectInput(ns("server_gauge"), label = "Server", choices = c(prometheus_url), selected = prometheus_url[1], width = "600px")),
      column(width = 4, textOutput(ns("last_date")))
    ),
    fluidRow(
      gauge(plotly::plotlyOutput(ns("gauge_temperature"))),
      gauge(plotly::plotlyOutput(ns("gauge_humidity"))),
      gauge(plotly::plotlyOutput(ns("gauge_pressure"))),
      gauge(uiOutput(ns("gauge_nh3"))),
      gauge(uiOutput(ns("gauge_reducing"))),
      gauge(uiOutput(ns("gauge_oxidising"))),
      gauge(uiOutput(ns("gauge_pm1"))),
      gauge(uiOutput(ns("gauge_pm25"))),
      gauge(uiOutput(ns("gauge_pm10"))),
      gauge(plotly::plotlyOutput(ns("gauge_lux"))),
      gauge(plotly::plotlyOutput(ns("gauge_proximity")))
    )
  )
}
##########################################################################################
#
##########################################################################################
#' Current module - server
#'
#' Downloads the readings of the last 7 days when the server changes, and
#' renders a gauge with the latest reading of each sensor. The gauges of the
#' sensors the device does not have stay empty, and when the server cannot be
#' reached the gauges say so.
#'
#' @param id Character. Module namespace id, must match the id used in
#'   [mod_current_ui()].
#'
#' @return None. Called for its side effect of registering the module server.
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' # inside app_server():
#' function(input, output, session) {
#'   mod_current_server("current")
#' }
#' }
mod_current_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    gauge_data <- reactive({
      start_time <- format(Sys.time() - 60 * 60 * 24 * 7, "%Y-%m-%dT%H:%M:%SZ")
      get_data(start_time = start_time, end_time = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ"), urlq = input$server_gauge)
    })
    # the rows of the last reading
    last_reading <- reactive({
      filtered_df <- gauge_data()
      validate(need(filtered_df, paste("The Prometheus server", input$server_gauge, "cannot be reached right now")))
      filtered_df[filtered_df$Timestamp %in% max(filtered_df$Timestamp), ]
    })
    gauge_output <- function(variable, render = plotly::renderPlotly) {
      render({
        df_last <- last_reading()
        req(variable %in% names(df_last))
        plot_gauge(variable = variable, data = df_last) %>% plotly::as_widget()
      })
    }
    output$last_date <- renderText({
      gauge_data()
      format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
    })
    output$gauge_temperature <- gauge_output("Temperature")
    output$gauge_humidity <- gauge_output("Humidity")
    output$gauge_pressure <- gauge_output("Pressure")
    output$gauge_lux <- gauge_output("Lux")
    output$gauge_proximity <- gauge_output("Proximity")
    output$gauge_pm1 <- gauge_output("PM1", render = renderUI)
    output$gauge_pm25 <- gauge_output("PM2.5", render = renderUI)
    output$gauge_pm10 <- gauge_output("PM10", render = renderUI)
    output$gauge_nh3 <- gauge_output("NH3", render = renderUI)
    output$gauge_reducing <- gauge_output("Reducing", render = renderUI)
    output$gauge_oxidising <- gauge_output("Oxidising", render = renderUI)
  })
}
