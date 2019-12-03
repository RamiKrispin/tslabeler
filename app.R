suppressMessages(library(drake))
suppressMessages(library(data.table))
suppressMessages(library(lattice))
suppressMessages(library(sparkline))
suppressMessages(library(shiny))
suppressMessages(library(shinyBS))
suppressMessages(library(shinyWidgets))
suppressMessages(library(shinydashboard))
suppressMessages(library(reactable))

options(shiny.maxRequestSize = 100 * 1024 ^ 2)

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
    uiOutput("dateslider"),
    prettyCheckboxGroup(
        inputId = "chkbox_plotopts",
        label = "Plot Options",
        choiceNames = c("Show anomalies",
                    "Free Y scale",
                    "Show legend"),
        choiceValues = c("anomaly", "freey", "legend"),
        selected = c("anomaly", "freey", "legend"),
        status = "info"
    ),
    hr(),
    actionBttn(
        inputId = "btn_newtag",
        label = NULL,
        style = "material-circle", 
        color = "primary",
        size = "xs",
        icon = icon("plus")
    ),
    uiOutput("taglist"),
    bsTooltip(id = "btn_newtag", title = "Add your own tag", 
              placement = "right", trigger = "hover", options = NULL),
    actionButton("mark", "Mark Anomaly", icon = icon("thumb-tack")),
    hr(),
    downloadBttn("download", label = "Download", style = "minimal", size = "s")
    # hr(),
    # sidebarMenuOutput("verinfo")
))

body <- dashboardBody(tabsetPanel(
    tabPanel(
        "Overlayed View",
        fluidRow(
            box(plotOutput("tsplot", brush = "user_brush_zoomed"),
                plotOutput("tsplot_zoomed", brush = "user_brush"),
                width = 12, solidHeader = T),
        ),
        fluidRow(
            column(reactableOutput("outtable"),width = 8),
            column(reactableOutput("metatable"),
                   plotOutput("plot_anomalybar", height = "200px"),
                   width = 4)
        ),
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
    reactive_flag <- reactiveVal(0)  
    
    observeEvent(input$filein_rawdata, {
        infile <- input$filein_rawdata
        if (is.null(infile))
            return(NULL)
        
        out <- fread(file = infile$datapath,
                     header = TRUE)
        
        if (ncol(out) == 3)
            setnames(out, c("ds", "grp", "value"))
        else if (ncol(out) == 5)
            setnames(out, c("ds", "grp", "value", "anomaly", "tag"))
        else
            stop("Input file non-compliant")
        
        setkeyv(out, "ds")
        
        out[, ds := lubridate::fast_strptime(ds,
                                             format = "%Y-%m-%d %H:%M:%S",
                                             tz = "UTC",
                                             lt = FALSE)]
        if (all(is.na(out[, ds])))
            stop("Could not parse date-time column. Format expected: %Y-%m-%d %H:%M:%S")
        
        out[, grp := as.character(grp)]
        out[, value := as.numeric(value)]
        
        values$tag_list <- c("spike",
                             "trend-change",
                             "level-shift",
                             "variance-shift",
                             "")
        
        values$total_pts <- out[, .N]
        values$total_grps <- out[, length(unique(grp))]
        
        if (ncol(out) == 3) {
            out[, anomaly := 0]
            out[, tag := ""]
            values$count_existing_anomalies <- 0
        } else if (ncol(out) == 5) {
            tags_in_file <- out[anomaly == 1, unique(tag)]
            custom_tags <-
                tags_in_file[!(tags_in_file %in% values$tag_list)]
            if (length(custom_tags) > 0)
                values$tag_list <- c(values$tag_list[values$tag_list!=""], 
                                     custom_tags,
                                     "")
            values$count_existing_anomalies <- out[anomaly == 1, .N]
        }
        
        values$original <- out
    })
    
    output$taglist <- renderUI({
        req(input$filein_rawdata)
        choice_names <- values$tag_list
        choice_names[choice_names==""] <- "remove tag"
        prettyRadioButtons(
            inputId = "radio_taglist",
            label = "Tags",
            choiceNames = choice_names,
            choiceValues = values$tag_list,
            selected = values$tag_list[1],
            inline = TRUE,
            status = "danger",
            fill = TRUE
        )
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
    
    output$dateslider <- renderUI({
        req(input$daterange)
        sliderTextInput(
            'dateslider',
            label = NULL,
            choices = seq(input$daterange[1],
                          input$daterange[2],
                          by = "1 day"),
            selected = c(input$daterange[1],
                         input$daterange[2])
        )
    })
    
    filtered_data <- reactive({
        reactive_flag()
        values$pts_selected_grps <-
            values$original[grp %in% input$picker_group, .N]
        values$original[grp %in% input$picker_group &
                            ds >= as.POSIXct(as.character(input$dateslider[1]), tz = "UTC") &
                            ds <= as.POSIXct(as.character(input$dateslider[2]), tz = "UTC")]
    })
    
    observeEvent(input$btn_newtag, {
        showModal(modalDialog(
            textInput(inputId = "textinput_customtag",
                      label = "What's your custom tag?"),
            footer = tagList(actionButton("btn_customtag_ok", "Add")),
            easyClose = TRUE
        ))
    })
    
    observeEvent(input$btn_customtag_ok, {
        if (input$textinput_customtag != "")
            values$tag_list <- c(values$tag_list,
                                 input$textinput_customtag)
    })
    
    output$tsplot <- renderPlot({
        req(input$filein_rawdata)
        withProgress({
            dat <- filtered_data()
            
            grp_filtered <- dat[, unique(grp)]
            tag_filtered <- dat[anomaly==1, unique(tag)]
            
            plot(
                dat[grp == grp_filtered[1], ds],
                dat[grp == grp_filtered[1], value],
                type = "l",
                ylim = c(min(dat$value, na.rm = T),
                         max(dat$value, na.rm = T)),
                xlab = "Date",
                ylab = "Value"
            )
            for (i in 1:length(grp_filtered)) {
                lines(
                    x = dat[grp == grp_filtered[i], ds],
                    y = dat[grp == grp_filtered[i], value],
                    type = "l",
                    col = i,
                    lty = 1,
                    lwd = 1.5
                )
                if ("anomaly" %in% input$chkbox_plotopts) {
                    subdat <- dat[grp == grp_filtered[i] &
                                      anomaly == 1]
                    points(subdat[, ds],
                           subdat[, value],
                           col = as.numeric(as.factor(subdat$tag)),
                           pch = 19)
                }
            }
            if ("legend" %in% input$chkbox_plotopts){
                legend(
                    "topleft",
                    legend = grp_filtered,
                    col = 1:length(grp_filtered),
                    bg = "white",
                    lwd = 2
                )
                if(length(tag_filtered)>0)
                    legend(
                        "topright",
                        legend = tag_filtered,
                        col = 1:length(tag_filtered),
                        bg = "white",
                        pch = 19,
                        lwd = 0
                    )
            }
        }, message = "Loading graph...")
    })
    
    output$tsplot_zoomed <- renderPlot({
        req(input$user_brush_zoomed)
        withProgress({
            dat <- selectedPoints()
            dat[anomaly==0, tag:=""]
            grp_filtered <- dat[, unique(grp)]
            tag_filtered <- dat[anomaly==1, unique(tag)]
            
            plot(
                dat[grp == grp_filtered[1], ds],
                dat[grp == grp_filtered[1], value],
                type = "l",
                ylim = c(min(dat$value, na.rm = T),
                         max(dat$value, na.rm = T)),
                xlab = "Date",
                ylab = "Value"
            )
            for (i in 1:length(grp_filtered)) {
                lines(
                    x = dat[grp == grp_filtered[i], ds],
                    y = dat[grp == grp_filtered[i], value],
                    type = "l",
                    col = i,
                    lty = 1,
                    lwd = 1.5
                )
                if ("anomaly" %in% input$chkbox_plotopts) {
                    subdat <- dat[grp == grp_filtered[i] &
                                      anomaly == 1]
                    points(subdat[, ds],
                           subdat[, value],
                           col = as.numeric(as.factor(subdat$tag)),
                           pch = 19)
                }
            }
            if ("legend" %in% input$chkbox_plotopts){
                legend(
                    "topleft",
                    legend = grp_filtered,
                    col = 1:length(grp_filtered),
                    bg = "white",
                    lwd = 2
                )
                if(length(tag_filtered)>0)
                    legend(
                        "topright",
                        legend = tag_filtered,
                        col = 1:length(tag_filtered),
                        bg = "white",
                        pch = 19,
                        lwd = 0
                    )
            }
        }, message = "Loading graph...")
    })
    
    output$tsplot_faceted <- renderPlot({
        req(input$filein_rawdata)
        dat <- filtered_data()
        lubridate::tz(dat$ds) <- ""
        xyplot(
            value ~ ds | grp,
            dat,
            type = "l",
            scales = ifelse("freey" %in% input$chkbox_plotopts, "free", "same"),
            xlab = "Date",
            ylab = "Value",
            auto.key = list(columns = 5)
        )
    })
    
    output$outtable <- renderReactable({
        req(input$filein_rawdata)
        dat <- selectedPoints_zoomed()
        if(nrow(dat)==0)
            dat <- selectedPoints()
        reactable(dat, 
                  columns = list(ds = colDef("Date", format = colFormat(datetime = T)), 
                                 grp = colDef("Group"), 
                                 value = colDef("Value", format = colFormat(digits=3)), 
                                 anomaly = colDef("Anomaly"), 
                                 tag = colDef("Tag")
                                 )
                  )
    })
    
    output$metatable <- renderReactable({
        req(input$filein_rawdata)
        dat <- filtered_data()
        
        meta <- data.table(
            Parameter = c(
                "Anomalies in Uploaded File",
                "Groups (Selected/Total)",
                "Points in Selected Groups",
                "Points in Filtered View",
                "Anomalies in Filtered View"
            ),
            Value = c(
                values$count_existing_anomalies,
                paste0(dat[, length(unique(grp))], "/", values$total_grps),
                values$pts_selected_grps, 
                dat[, .N],
                dat[, sum(anomaly)]
            )
        )
        reactable(meta)
    })
    
    selectedPoints <- reactive({
        brushedPoints(
            df = filtered_data(),
            brush = input$user_brush_zoomed,
            xvar = "ds",
            yvar = "value"
        )
    })
    
    selectedPoints_zoomed <- reactive({
        brushedPoints(
            df = selectedPoints(),
            brush = input$user_brush,
            xvar = "ds",
            yvar = "value"
        )
    })
    
    observeEvent(input$mark, {
        seldat <- selectedPoints_zoomed()
        if(nrow(seldat)==0)
            seldat <- selectedPoints()

        seldat[,anomaly := ifelse(input$radio_taglist=="",0,1)]
        seldat[,tag := input$radio_taglist]
        
        unmodified <- values$original[!seldat[,.(ds,grp,anomaly,tag)],on=c("ds","grp")]
        new <- rbindlist(list(unmodified, seldat))
        
        setkeyv(new, c("ds"))
        
        values$original <- new
        reactive_flag(runif(n = 1))
    })
    
    output$download <- downloadHandler(
        filename = function() {
            paste(input$filein_rawdata$name, ".csv", sep = "")
        },
        content = function(file) {
            fwrite(
                x = values$original,
                file = file,
                row.names = FALSE,
                col.names = TRUE
            )
        }
    )
    
    output$plot_anomalybar <- renderPlot({
        req(input$filein_rawdata)
        dat <- filtered_data()
        dat <- dat[anomaly==1, .(total=sum(anomaly)), tag]
        if(nrow(dat)>0)
            barchart(tag~total,dat,xlab="",panel=function(...) { 
                args <- list(...)
                panel.barchart(...)
                panel.text(3, args$y, args$x, offset=0, pos=4)
            }, scales=list(x=list(at=NULL),cex=1),
            col=brewer.pal(6,"Blues"),
            xlim=c(0,max(5,max(dat$total)+5)),
            par.settings = list(axis.line = list(col = "transparent")))
    })
    
    # output$verinfo <- renderMenu({
    #     sidebarMenu(
    #         "\tVer", 0.1
    #     )
    # })
    
}

shinyApp(ui, server, options = list(port = 4686))