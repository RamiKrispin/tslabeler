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
    prettyCheckbox(
        inputId = "chkbox_overlaycategories",
        label = "Overlay Categories", 
        value = TRUE
    ),    
    prettyCheckbox(
        inputId = "chkbox_showlegend",
        label = "Show Legend", 
        value = FALSE
    ),
    uiOutput("daterange"),
    uiOutput("cat_list")
    # menuItem("Plots", tabName = "tab_plots")
))

body <- dashboardBody(
    tabsetPanel(
        tabPanel("Current category",
                 plotOutput("tsplot", brush = "user_brush", click = "plot_click"),
                 h2('Selected points'),
                 reactableOutput("outtable"),
                 verbatimTextOutput("info")
                 )
        )
)

ui <- dashboardPage(
    header = shinydashboardPlus::dashboardHeaderPlus(title = "TS Labeler"),
    sidebar = sidebar,
    body = body,
    skin = "black"
)

server <- function(input, output) {
    read_data <- reactive({
        req(input$filein_rawdata)
        infile <- input$filein_rawdata
        out <- fread(file = infile$datapath, 
              col.names = c("ds","cat","value"),
              key = "ds")
        out[,ds:=lubridate::fast_strptime(ds, format = "%Y-%m-%d %H:%M:%S",
                                          tz = "UTC", lt = FALSE)]
        out[,cat:=as.character(cat)]
        out[,value:=as.numeric(value)]
        out
    })
    
    output$cat_list <- renderUI({
        req(input$filein_rawdata)
        dat <- read_data()
        pickerInput(
            inputId = "picker_categories",
            label = "TS Category", 
            choices = dat[,unique(cat)],
            options = list(`live-search` = TRUE),
            multiple = TRUE
        )
    })
    
    output$cat_list <- renderUI({
        req(input$filein_rawdata)
        dat <- read_data()
        dateRangeInput('dateRange',
                       label = 'Date range',
                       start = dat[,min(ds)], 
                       end = dat[,max(ds)] 
        )
    })
    
    filtered_data <- reactive({
        dat <- read_data()
        dat[cat %in% input$picker_categories]
    })
    
    output$tsplot <- renderPlot({
        withProgress({
            dat <- filtered_data()
            
            cat_filtered <- dat[,unique(cat)]
            
            plot(dat[cat == cat_filtered[1],ds],
                 dat[cat == cat_filtered[1],value],
                 type="l",
                 col="red",lty=1,lwd=2, 
                 ylim=c( min(dat$value, na.rm=T),
                         max(dat$value, na.rm=T)),
                 xlab = "Date",
                 ylab = "Value")
            for(i in 1:length(cat_filtered)){ 
                lines(x = dat[cat==cat_filtered[i],ds],
                      y = dat[cat==cat_filtered[i],value],
                      type="l",
                      col=i,
                      lty=1,lwd=2)
            } 
        }, message = "Loading graph...")
    })
    
    output$outtable <- renderReactable(reactable(selectedPoints()))
    
    output$info <- renderText({
        paste0("x=", input$plot_click$x, "\ny=", input$plot_click$y)
    })
    
    selectedPoints <- reactive({
        pts <- brushedPoints(df = filtered_data(),
                             brush = input$user_brush,
                             xvar = "ds",
                             yvar = "value")
        print(pts)
        pts
    })
}

shinyApp(ui, server, options = list(port = 4686))