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
        label = "Input data",
        multiple = FALSE,
        accept = c("text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")
    ),
    uiOutput("grp_list"),
    uiOutput("daterange"),
    actionButton("mark", "Mark Anomaly"),
    prettyCheckboxGroup(
        inputId = "chkbox_plotopts",
        label = "Plot Options",
        choiceNames = c("Show anomalies",
                    "Free Y scale",
                    "Show legend"),
        choiceValues = c("anomaly", "freey", "legend"),
        selected = c("anomaly", "freey", "legend"),
        status = "info"
    )
))

body <- dashboardBody(tabsetPanel(
    tabPanel(
        "Overlayed View",
        plotOutput("tsplot", brush = "user_brush", click = "user_click"),
        h3('Selection'),
        reactableOutput("outtable")
    ),
    tabPanel(
        "Faceted View",
        plotOutput("tsplot_faceted", height = "850px")
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
            header = TRUE
        )
        
        if(ncol(out)==3)
            setnames(out, c("ds", "grp", "value"))
        else if(ncol(out)==5)
            setnames(out, c("ds", "grp", "value", "anomaly", "tag"))
        else
            stop("Input file non-compliant")
            
        setkeyv(out, "ds")
        
        out[, ds := lubridate::fast_strptime(ds,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC",
                                             lt = FALSE)]
        out[, grp := as.character(grp)]
        out[, value := as.numeric(value)]
        
        if(ncol(out)==3){
            out[, anomaly := 0]
            out[, tag := NA]
        }
        
        values$original <- out
    })

    output$grp_list <- renderUI({
        req(input$filein_rawdata)
        pickerInput(
            inputId = "picker_group",
            label = "Group",
            choices = values$original[, unique(grp)],
            selected = values$original[, unique(grp)][1],
            options = list(`live-search` = TRUE,
                           `actions-box` = TRUE),
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
        values$original[grp %in% input$picker_group &
                            ds >= as.POSIXct(input$daterange[1], tz = "UTC") &
                            ds <= as.POSIXct(input$daterange[2], tz = "UTC")]
    })
    
    output$tsplot <- renderPlot({
        withProgress({
            dat <- filtered_data()
            
            grp_filtered <- dat[, unique(grp)]
            
            plot(
                dat[grp == grp_filtered[1], ds],
                dat[grp == grp_filtered[1], value],
                type = "l",
                col = "red",
                lty = 1,
                lwd = 2,
                ylim = c(min(dat$value, na.rm = T),
                         max(dat$value, na.rm = T)),
                xlab = "Date",
                ylab = "Value"
            )
            if ("anomaly" %in% input$chkbox_plotopts) {
                subdat <- dat[grp == grp_filtered[1] &
                                  anomaly == 1]
                points(subdat[, ds],
                       subdat[, value],
                       col = "red",
                       pch = 19)
            }
            for (i in 1:length(grp_filtered)) {
                lines(
                    x = dat[grp == grp_filtered[i], ds],
                    y = dat[grp == grp_filtered[i], value],
                    type = "l",
                    col = i,
                    lty = 1,
                    lwd = 2
                )
                if ("anomaly" %in% input$chkbox_plotopts) {
                    subdat <- dat[grp == grp_filtered[i] &
                                      anomaly == 1]
                    points(subdat[, ds],
                           subdat[, value],
                           col = "red",
                           pch = 19)
                }
            }
            if("legend" %in% input$chkbox_plotopts)
                legend(
                    "topleft",
                    legend = grp_filtered,
                    col = 1:length(grp_filtered),
                    bg = "white",
                    lwd = 2
                )
        }, message = "Loading graph...")
    })
    
    output$tsplot_faceted <- renderPlot({
        dat <- filtered_data()
        xyplot(value~ds|grp,
               dat,
               type = "l",
               scales = ifelse("freey" %in% input$chkbox_plotopts, "free", "same"),
               xlab = "Date",
               ylab = "Value",
               auto.key = list(columns = 5))
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
        values$new <- values$original
        seldat <- selectedPoints()
        for (i in 1:nrow(seldat)) {
            values$new[ds == seldat[i, ds] &
                           grp == seldat[i, grp],
                       anomaly := 1]
        }
        values$original <- values$new
    })
}

shinyApp(ui, server, options = list(port = 4686))