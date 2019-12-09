# File Import  ---------------------------------------------------------------

input_data_importfile_ui <- function() {
        shinydashboard::box(
                shiny::fileInput(
                        inputId = 'filein_rawdata',
                        label = 'Choose CSV',
                        multiple = FALSE,
                        accept = c(
                                'text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv'
                        )
                ),
                shinyWidgets::prettyCheckboxGroup(
                        inputId = "chkbox_inputfileopts",
                        label = "Input file options",
                        choiceNames = c("Has Header?",
                                        "Has Groups?"),
                        choiceValues = c("header", "groups"),
                        selected = c("header", "groups"),
                        status = "info",
                        inline = TRUE
                ),
                shinyWidgets::prettyRadioButtons(
                        inputId = 'filein_sep',
                        label = 'Separator',
                        choices = c(
                                Comma = ',',
                                Semicolon = ';',
                                Tab = '\t'
                        ),
                        selected = ',',
                        inline = TRUE
                ),
                shinyWidgets::prettyRadioButtons(
                        inputId = 'filein_quote',
                        label = 'Quote',
                        choices = c(
                                None = '',
                                'Double Quote' = '"',
                                'Single Quote' = "'"
                        ),
                        selected = '"',
                        inline = TRUE
                ),
                solidHeader = TRUE,
                width = 12
        )
        
}

input_data_selectdataframe_ui <- function() {
        shinydashboard::box(
                shiny::selectInput(
                        inputId = "df_to_load",
                        label = "Select variable",
                        choices = env_tabs$existing_tables
                ),
                solidHeader = TRUE
        )
}