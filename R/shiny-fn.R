#' Launch the Time Series Labeler Tool
#' @import shiny shinydashboard
#' @export
#' @details
#' @examples
#' \dontrun{
#' label_ts()
#' }
label_ts <- function() {
        suppressPackageStartupMessages(
                shiny::runApp(system.file(package = "tslabeler"), 
                              launch.browser = TRUE)
        )
        }