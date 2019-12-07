# File Import  ---------------------------------------------------------------

input_data_source_import <- function(){
        shiny::fluidRow(
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
                shinyWidgets::awesomeCheckbox(
                        inputId = "filein_csv_header",
                        label = "Header",
                        value = TRUE
                ),
                shinyWidgets::awesomeCheckbox(
                        inputId = "filein_hasgroup",
                        label = "Groups",
                        value = TRUE
                ),
                shiny::radioButtons(
                        inputId = 'filein_sep',
                        label = 'Separator',
                        choices = c(
                                Comma = ',',
                                Semicolon = ';',
                                Tab = '\t'
                        ),
                        selected = ','
                ),
                shiny::radioButtons(
                        inputId = 'filein_quote',
                        label = 'Quote',
                        choices = c(
                                None = '',
                                'Double Quote' = '"',
                                'Single Quote' = "'"
                        ),
                        selected = '"'
                ),
                solidHeader = TRUE, 
                width = 12
        )
        )
}

# input_data_source_df <- function(){
#             selectInput("df_to_load", "Select Data Frame",
#                         choices = prev_table$inputs_list)
# }
# 
