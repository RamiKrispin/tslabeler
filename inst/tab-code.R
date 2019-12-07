tab_input_data <- function() {
        shiny::fluidPage(
                shiny::fluidRow(
                        shinydashboard::box(
                                width = 4,
                                height = 100,
                                solidHeader = TRUE,
                                shiny::selectInput(
                                        inputId = 'data_source',
                                        label = 'Select Data Source',
                                        choices = list(
                                                "R Data Frame / Data Table" = "data_frame",
                                                # "R Time Series" = "time_series",
                                                # "Installed Package Dataset" = "inst_pack",
                                                "Import CSV File" = "import"
                                        )
                                )
                        ),
                        shinydashboard::box(shiny::uiOutput("input_ui"))
                ),
                shiny::fluidRow(
                        shinydashboard::box(
                                width = 12,
                                solidHeader = TRUE,
                                title = "Sample data",
                                reactable::reactableOutput("sample_input")
                        )
                )
        )
}

tab_labeler <- function() {
        shiny::tabsetPanel(
                shiny::tabPanel(
                        title = "Overlayed View",
                        shiny::fluidRow(
                                shinydashboard::box(
                                        shiny::plotOutput("tsplot", brush = "user_brush_zoomed", height = "400px"),
                                        shiny::plotOutput("tsplot_zoomed", brush = "user_brush", height = "400px"),
                                        width = 12,
                                        solidHeader = T
                                ),
                        ),
                        shiny::fluidRow(
                                shiny::column(reactable::reactableOutput("outtable"), width = 8),
                                shiny::column(
                                        reactable::reactableOutput("metatable"),
                                        shiny::plotOutput("plot_anomalybar", height = "200px"),
                                        width = 4
                                )
                        )
                )
        )
}