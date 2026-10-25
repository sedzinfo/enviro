##########################################################################################
#
##########################################################################################
#' The application User-Interface
#'
#' A navbar page with the Current tab, gauges with the latest readings, and
#' the Time Series tab, a chart per sensor. The servers to choose from are the
#' `prometheus_url` given to [pimoroni()].
#'
#' @param request Internal parameter for `{shiny}`. DO NOT REMOVE.
#'
#' @import shiny
#' @noRd
#'
#' @examples
#' \dontrun{
#' shiny::shinyApp(ui = app_ui, server = app_server)
#' }
app_ui <- function(request) {
  prometheus_url <- golem::get_golem_options("prometheus_url")
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Application UI logic
    navbarPage(
      "Prometheus Data Visualization",
      tabPanel("Current", mod_current_ui("current", prometheus_url)),
      tabPanel("Time Series", mod_timeseries_ui("timeseries", prometheus_url))
    )
  )
}
##########################################################################################
#
##########################################################################################
#' Add external resources to the application
#'
#' Internally used to add the favicon, the Font Awesome icons of the full
#' screen buttons, and the CSS and JavaScript files of `inst/app/www` inside
#' the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path favicon bundle_resources
#' @noRd
#'
#' @examples
#' \dontrun{
#' # called from inside app_ui():
#' shiny::tagList(golem_add_external_resources(), shiny::fluidPage())
#' }
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ext = "svg"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "enviro"
    ),
    # the Font Awesome icons of the full screen buttons of the charts
    htmltools::htmlDependencies(icon("expand"))
  )
}
