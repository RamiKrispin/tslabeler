options(shiny.maxRequestSize = 100 * 1024 ^ 2)
source("ui-code.R")
source("tab-code.R")
# Sidebar -----------------------------------------------------------------

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::menuItem(
      text = "Inputs",
      tabName = "input_data",
      icon = icon("table")
    )#,
    # shinydashboard::menuItem(
    #   text = "Labeler",
    #   tabName = "labeler",
    #   icon = icon("table"),
    #   startExpanded = TRUE,
    #   sidebarMenuOutput("labler_menu")
    # ),
    # shinyWidgets::downloadBttn(
    #   "download",
    #   label = "Download",
    #   style = "minimal",
    #   size = "s"
    # )
  )
)

# Body --------------------------------------------------------------------

body <- shinydashboard::dashboardBody(shinydashboard::tabItems(
  shinydashboard::tabItem(tabName = "input_data",
                          tab_input_data())#,
  # shinydashboard::tabItem(tabName = "labeler",
  #                         tab_labeler())
  # shiny::tabPanel(
  #   "Faceted View",
  #   shiny::plotOutput("tsplot_faceted", height = "850px")
  # )
))


# UI ----------------------------------------------------------------------

ui <- shinydashboard::dashboardPage(
  header = shinydashboardPlus::dashboardHeaderPlus(title = "TS Labeler"),
  sidebar = sidebar,
  body = body,
  skin = "black"
)

# Server ------------------------------------------------------------------

server <- function(input, output) {
  
  # Instantiate values
  
  values <- shiny::reactiveValues()
  reactive_flag <- shiny::reactiveVal(0)

  # Input Data UI
  
  output$input_ui <- shiny::renderUI({
    if (input$data_source == "data_frame"){
        input_data_source_df()
    } else if (input$data_source == "import"){
        input_data_source_import()
    }
  })
  
  # output$df_list <- shiny::renderUI({
  #   if (input$data_source == "data_frame")
  #     selectInput("df_to_load", "Select Data Frame",
  #                 choices = prev_table$inputs_list)
  # })
  
  shiny::observeEvent(input$filein_rawdata, {
    infile <- input$filein_rawdata
    
    if (is.null(infile)) {
      return(NULL)
    }
    
    out <- data.table::fread(file = infile$datapath,
                             header = TRUE)
    
    if (ncol(out) == 3) {
      data.table::setnames(out, c("ds", "grp", "value"))
    } else if (ncol(out) == 5) {
      data.table::setnames(out, c("ds", "grp", "value", "anomaly", "tag"))
    } else {
      stop("Input file non-compliant")
    }
    
    data.table::setkeyv(out, "ds")
    
    out[, ds := lubridate::fast_strptime(ds,
                                         format = "%Y-%m-%d %H:%M:%S",
                                         tz = "UTC",
                                         lt = FALSE)]
    if (all(is.na(out[, ds]))) {
      stop("Could not parse date-time column. Format expected: %Y-%m-%d %H:%M:%S")
    }
    
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
      if (length(custom_tags) > 0) {
        values$tag_list <- c(values$tag_list[values$tag_list != ""],
                             custom_tags,
                             "")
      }
      values$count_existing_anomalies <- out[anomaly == 1, .N]
    }
    
    values$original <- out
  })

  # Labeler UI
  
  output$labler_menu <- renderMenu({
    # shiny::uiOutput("grp_list")
    shiny::req(input$filein_rawdata)
    shinyWidgets::pickerInput(
      inputId = "picker_group",
      label = "Group",
      choices = values$original[, unique(grp)],
      selected = values$original[, unique(grp)][1],
      options = list(`live-search` = TRUE,
                     `actions-box` = TRUE),
      multiple = TRUE
    )
    # shiny::uiOutput("daterange")
    # shiny::dateRangeInput(
    #   inputId = "daterange",
    #   label = "Date range",
    #   start = values$original[, min(ds)],
    #   end = values$original[, max(ds)]
    # )
    # shiny::hr()
    # shinyWidgets::prettyCheckboxGroup(
    #   inputId = "chkbox_plotopts",
    #   label = "Plot Options",
    #   choiceNames = c("Show anomalies",
    #                   "Free Y scale",
    #                   "Show legend"),
    #   choiceValues = c("anomaly", "freey", "legend"),
    #   selected = c("anomaly", "freey", "legend"),
    #   status = "info"
    # )
    # shiny::hr()
    # shinyWidgets::actionBttn(
    #   inputId = "btn_newtag",
    #   label = NULL,
    #   style = "material-circle",
    #   color = "primary",
    #   size = "xs",
    #   icon = shiny::icon("plus")
    # )
    # # shiny::uiOutput("taglist")
    # choice_names <- values$tag_list
    # choice_names[choice_names == ""] <- "remove tag"
    # shinyWidgets::prettyRadioButtons(
    #   inputId = "radio_taglist",
    #   label = "Tags",
    #   choiceNames = choice_names,
    #   choiceValues = values$tag_list,
    #   selected = values$tag_list[1],
    #   inline = TRUE,
    #   status = "danger",
    #   fill = TRUE
    # )
    # shinyBS::bsTooltip(
    #   id = "btn_newtag",
    #   title = "Add your own tag",
    #   placement = "right",
    #   trigger = "hover",
    #   options = NULL
    # )
    # shiny::actionButton("mark", "Mark Anomaly", icon = shiny::icon("thumb-tack"))
    # shiny::hr()
  })
    
  output$grp_list <- shiny::renderUI({
    shiny::req(input$filein_rawdata)
    shinyWidgets::pickerInput(
      inputId = "picker_group",
      label = "Group",
      choices = values$original[, unique(grp)],
      selected = values$original[, unique(grp)][1],
      options = list(`live-search` = TRUE,
                     `actions-box` = TRUE),
      multiple = TRUE
    )
  })
  
  output$daterange <- shiny::renderUI({
    shiny::req(input$filein_rawdata)
    shiny::dateRangeInput(
      inputId = "daterange",
      label = "Date range",
      start = values$original[, min(ds)],
      end = values$original[, max(ds)]
    )
  })
  
  output$taglist <- shiny::renderUI({
    shiny::req(input$filein_rawdata)
    choice_names <- values$tag_list
    choice_names[choice_names == ""] <- "remove tag"
    shinyWidgets::prettyRadioButtons(
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
  
  filtered_data <- shiny::reactive({
    reactive_flag()
    values$pts_selected_grps <-
      values$original[grp %in% input$picker_group, .N]
    values$original[grp %in% input$picker_group &
                      ds >= as.POSIXct(as.character(input$daterange[1]), tz = "UTC") &
                      ds <= as.POSIXct(as.character(input$daterange[2]), tz = "UTC")]
  })
  
  shiny::observeEvent(input$btn_newtag, {
    shiny::showModal(
      shiny::modalDialog(
        shiny::textInput(inputId = "textinput_customtag",
                         label = "What's your custom tag?"),
        footer = shiny::tagList(shiny::actionButton("btn_customtag_ok", "Add")),
        easyClose = TRUE
      )
    )
  })
  
  shiny::observeEvent(input$btn_customtag_ok, {
    if (input$textinput_customtag != "") {
      values$tag_list <- c(values$tag_list,
                           input$textinput_customtag)
    }
  })
  
  output$tsplot <- shiny::renderPlot({
    shiny::req(input$filein_rawdata)
    shiny::withProgress({
      dat <- filtered_data()
      
      grp_filtered <- dat[, unique(grp)]
      tag_filtered <- dat[anomaly == 1, unique(tag)]
      
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
      if ("legend" %in% input$chkbox_plotopts) {
        legend(
          "topleft",
          legend = grp_filtered,
          col = 1:length(grp_filtered),
          bg = "white",
          lwd = 2
        )
        if (length(tag_filtered) > 0) {
          legend(
            "topright",
            legend = tag_filtered,
            col = 1:length(tag_filtered),
            bg = "white",
            pch = 19,
            lwd = 0
          )
        }
      }
    }, message = "Loading graph...")
  })
  
  output$tsplot_zoomed <- shiny::renderPlot({
    shiny::req(input$user_brush_zoomed)
    shiny::withProgress({
      dat <- selectedPoints()
      dat[anomaly == 0, tag := ""]
      grp_filtered <- dat[, unique(grp)]
      tag_filtered <- dat[anomaly == 1, unique(tag)]
      
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
      if ("legend" %in% input$chkbox_plotopts) {
        legend(
          "topleft",
          legend = grp_filtered,
          col = 1:length(grp_filtered),
          bg = "white",
          lwd = 2
        )
        if (length(tag_filtered) > 0) {
          legend(
            "topright",
            legend = tag_filtered,
            col = 1:length(tag_filtered),
            bg = "white",
            pch = 19,
            lwd = 0
          )
        }
      }
    }, message = "Loading graph...")
  })
  
  output$tsplot_faceted <- shiny::renderPlot({
    shiny::req(input$filein_rawdata)
    dat <- filtered_data()
    lubridate::tz(dat$ds) <- ""
    lattice::xyplot(
      value ~ ds | grp,
      dat,
      type = "l",
      scales = ifelse("freey" %in% input$chkbox_plotopts, "free", "same"),
      xlab = "Date",
      ylab = "Value",
      auto.key = list(columns = 5)
    )
  })
  
  output$outtable <- reactable::renderReactable({
    shiny::req(input$filein_rawdata)
    dat <- selectedPoints_zoomed()
    if (nrow(dat) == 0) {
      dat <- selectedPoints()
    }
    reactable::reactable(
      dat,
      columns = list(
        ds = reactable::colDef("Date", format = reactable::colFormat(datetime = T)),
        grp = reactable::colDef("Group"),
        value = reactable::colDef("Value", format = reactable::colFormat(digits = 3)),
        anomaly = reactable::colDef("Anomaly"),
        tag = reactable::colDef("Tag")
      )
    )
  })
  
  output$metatable <- reactable::renderReactable({
    shiny::req(input$filein_rawdata)
    dat <- filtered_data()
    
    meta <- data.table::data.table(
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
    reactable::reactable(meta)
  })
  
  selectedPoints <- shiny::reactive({
    shiny::brushedPoints(
      df = filtered_data(),
      brush = input$user_brush_zoomed,
      xvar = "ds",
      yvar = "value"
    )
  })
  
  selectedPoints_zoomed <- shiny::reactive({
    shiny::brushedPoints(
      df = selectedPoints(),
      brush = input$user_brush,
      xvar = "ds",
      yvar = "value"
    )
  })
  
  shiny::observeEvent(input$mark, {
    seldat <- selectedPoints_zoomed()
    if (nrow(seldat) == 0) {
      seldat <- selectedPoints()
    }
    
    seldat[, anomaly := ifelse(input$radio_taglist == "", 0, 1)]
    seldat[, tag := input$radio_taglist]
    
    unmodified <-
      values$original[!seldat[, .(ds, grp, anomaly, tag)], on = c("ds", "grp")]
    new <- data.table::rbindlist(list(unmodified, seldat))
    
    data.table::setkeyv(new, c("ds"))
    
    values$original <- new
    reactive_flag(runif(n = 1))
  })
  
  output$download <- shiny::downloadHandler(
    filename = function() {
      paste(input$filein_rawdata$name, ".csv", sep = "")
    },
    content = function(file) {
      data.table::fwrite(
        x = values$original,
        file = file,
        row.names = FALSE,
        col.names = TRUE
      )
    }
  )
  
  output$plot_anomalybar <- shiny::renderPlot({
    shiny::req(input$filein_rawdata)
    dat <- filtered_data()
    dat <- dat[anomaly == 1, .(total = sum(anomaly)), tag]
    if (nrow(dat) > 0) {
      lattice::barchart(
        tag ~ total,
        dat,
        xlab = "",
        panel = function(...) {
          args <- list(...)
          lattice::panel.barchart(...)
          lattice::panel.text(3, args$y, args$x, offset = 0, pos = 4)
        },
        scales = list(x = list(at = NULL), cex = 1),
        col = RColorBrewer::brewer.pal(6, "Blues"),
        xlim = c(0, max(5, max(dat$total) + 5)),
        par.settings = list(axis.line = list(col = "transparent"))
      )
    }
  })
  
  # output$verinfo <- renderMenu({
  #     sidebarMenu(
  #         "\tVer", 0.1
  #     )
  # })
}

shiny::shinyApp(ui, server, options = list(port = 4686))