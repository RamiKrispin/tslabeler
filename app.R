suppressMessages(library(drake))
suppressMessages(library(data.table))
suppressMessages(library(lattice))
suppressMessages(library(ggplot2))
# suppressMessages(library(magrittr))
suppressMessages(library(shiny))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinydashboard))
suppressMessages(library(reactable))

options(shiny.maxRequestSize = 50 * 1024 ^ 2)

tab_plots <- function() {
    
}

sidebar <- dashboardSidebar(sidebarMenu(
    fileInput(
        inputId = "filein_rawdata",
        label = "Choose file",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
    ),
    uiOutput("cat_list"),
    uiOutput("daterange"),
    actionButton("mark", "Mark Anomaly"),
    prettyCheckbox(
        inputId = "chkbox_showanomalies",
        label = "Show Anomalies",
        value = TRUE
    )
))

body <- dashboardBody(tabsetPanel(
    tabPanel(
        "Current category",
        plotOutput("tsplot", brush = "user_brush", click = "plot_click"),
        h2('Selected points'),
        reactableOutput("outtable"),
        verbatimTextOutput("info")
    )
))

ui <- dashboardPage(
    header = shinydashboardPlus::dashboardHeaderPlus(title = "TS Labeler"),
    sidebar = sidebar,
    body = body,
    skin = "black"
)

server <- function(input, output) {
    values <- reactiveValues()
    
    observeEvent(input$filein_rawdata, {
        infile <- input$filein_rawdata
        if (is.null(infile))
            return(NULL)
        
        out <- fread(
            file = infile$datapath,
            col.names = c("ds", "cat", "value"),
            key = "ds"
        )
        out[, ds := lubridate::fast_strptime(ds,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC",
                                             lt = FALSE)]
        out[, cat := as.character(cat)]
        out[, value := as.numeric(value)]
        out[, anomaly := 0]
        out[, tag := NA]
        
        values$original <- out
    })

    output$cat_list <- renderUI({
        req(input$filein_rawdata)
        pickerInput(
            inputId = "picker_categories",
            label = "TS Category",
            choices = values$original[, unique(cat)],
            options = list(`live-search` = TRUE),
            multiple = TRUE
        )
    })
    
    output$daterange <- renderUI({
        req(input$filein_rawdata)
        dateRangeInput(
            'daterange',
            label = 'Date range',
            start = values$original[, min(ds)],
            end = values$original[, max(ds)]
        )
    })
    
    filtered_data <- reactive({
        values$original[cat %in% input$picker_categories &
                            ds >= as.POSIXct(input$daterange[1], tz = "UTC") &
                            ds <= as.POSIXct(input$daterange[2], tz = "UTC")]
    })
    
    output$tsplot <- renderPlot({
        withProgress({
            dat <- filtered_data()
            
            cat_filtered <- dat[, unique(cat)]
            
            plot(
                dat[cat == cat_filtered[1], ds],
                dat[cat == cat_filtered[1], value],
                type = "l",
                col = "red",
                lty = 1,
                lwd = 2,
                ylim = c(min(dat$value, na.rm = T),
                         max(dat$value, na.rm = T)),
                xlab = "Date",
                ylab = "Value"
            )
            if (input$chkbox_showanomalies) {
                subdat <- dat[cat == cat_filtered[1] &
                                  anomaly == 1]
                points(subdat[, ds],
                       subdat[, value],
                       col = "red",
                       pch = 19)
            }
            for (i in 1:length(cat_filtered)) {
                lines(
                    x = dat[cat == cat_filtered[i], ds],
                    y = dat[cat == cat_filtered[i], value],
                    type = "l",
                    col = i,
                    lty = 1,
                    lwd = 2
                )
                if (input$chkbox_showanomalies) {
                    subdat <- dat[cat == cat_filtered[i] &
                                      anomaly == 1]
                    points(subdat[, ds],
                           subdat[, value],
                           col = "red",
                           pch = 19)
                }
            }
            legend(
                "topleft",
                legend = cat_filtered,
                col = 1:length(cat_filtered),
                bg = "white",
                lwd = 2
            )
            
        }, message = "Loading graph...")
    })
    
    output$outtable <- renderReactable(reactable(selectedPoints()))
    
    selectedPoints <- reactive({
        brushedPoints(
            df = filtered_data(),
            brush = input$user_brush,
            xvar = "ds",
            yvar = "value"
        )
    })
    
    observeEvent(input$mark, {
        # dat <- processed_data()
        values$new <- values$original
        seldat <- selectedPoints()
        for (i in 1:nrow(seldat)) {
            values$new[ds == seldat[i, ds] &
                           cat == seldat[i, cat],
                       anomaly := 1]
        }
        values$original <- values$new
    })
}

shinyApp(ui, server, options = list(port = 4686))