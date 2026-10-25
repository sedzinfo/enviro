##########################################################################################
#
##########################################################################################
#' The application server-side logic
#'
#' Wires up the module of each tab. Each tab has its own server picker, and
#' downloads its own data.
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#'   DO NOT REMOVE.
#'
#' @import shiny
#' @noRd
#'
#' @examples
#' \dontrun{
#' shiny::shinyApp(ui = app_ui, server = app_server)
#' }
app_server <- function(input, output, session) {
  mod_current_server("current")
  mod_timeseries_server("timeseries")
}
