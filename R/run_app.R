##########################################################################################
#
##########################################################################################
#' Run the Pimoroni Enviro app
#'
#' Starts a Shiny app with the readings of Pimoroni Enviro and Enviro+ sensors
#' exported to Prometheus: gauges with the latest readings in the Current tab,
#' and a chart per sensor in the Time Series tab.
#'
#' @param prometheus_url Character vector of Prometheus query_range urls, one
#'   per device, such as "http://pip1.crabdance.com:1507/api/v1/query_range".
#' @param timezone The time zone the readings are shown in, while the app runs.
#' @param onStart See [shiny::shinyApp()].
#' @param options See [shiny::shinyApp()].
#' @param enableBookmarking See [shiny::shinyApp()].
#' @param uiPattern See [shiny::shinyApp()].
#' @param ... Further options passed to [golem::with_golem_options()].
#'
#' @return A [shiny::shinyApp()] object. Printing it, or calling `pimoroni()`
#'   at the console, opens the app in the browser.
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#'
#' @examples
#' if (interactive()) {
#'   pimoroni(prometheus_url = c(
#'     "http://pip1.crabdance.com:1507/api/v1/query_range",
#'     "http://pip1.crabdance.com:1505/api/v1/query_range"
#'   ))
#' }
pimoroni <- function(prometheus_url,
                     timezone = "Europe/Bucharest",
                     onStart = NULL,
                     options = list(),
                     enableBookmarking = NULL,
                     uiPattern = "/",
                     ...) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = function() {
        # for this app run only: times in the time zone of the data, and small values in full
        old_options <- base::options(scipen = 999)
        old_tz <- Sys.getenv("TZ", unset = NA)
        Sys.setenv(TZ = timezone)
        onStop(function() {
          base::options(old_options)
          if (is.na(old_tz)) Sys.unsetenv("TZ") else Sys.setenv(TZ = old_tz)
        })
        if (!is.null(onStart)) onStart()
      },
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(prometheus_url = prometheus_url, ...)
  )
}
