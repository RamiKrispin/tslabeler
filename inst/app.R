options(shiny.maxRequestSize = 100 * 1024 ^ 2)
source("ui-code.R")
source("tab-code.R")
  
# Initial Processessing --------------------------------------------------------------------

dt_list <- tslabeler:::get_dt_from_env()

if(!exists('currentTab')){
  currentTab <- 'input_data'
}

# Sidebar -----------------------------------------------------------------

sidebar <- shinydashboard::dashboardSidebar(
  shinydashboard::sidebarMenu(
    shinydashboard::sidebarMenuOutput(outputId = "labeler_menu"),
    hr(),
    shinyWidgets::actionBttn(
      inputId = "btn_save_to_env",
      label = "Save to Env",
      style = "simple",
      size = "xs",
      color = "success",
      icon = icon("floppy-o")
    ),
    shinyWidgets::downloadBttn(
      outputId = "btn_download",
      label = "Download",
      style = "material-flat",
      color = "primary",
      size = "xs"
    )
  )
)

# Body --------------------------------------------------------------------

body <- shinydashboard::dashboardBody(shinydashboard::tabItems(
  shinydashboard::tabItem(tabName = "input_data",
                          tab_input_data()),
  shinydashboard::tabItem(tabName = "labeler",
                          tab_labeler())
))


# UI ----------------------------------------------------------------------

ui <- shinydashboard::dashboardPage(
  header = shinydashboardPlus::dashboardHeaderPlus(title = "TS Labeler"),
  sidebar = sidebar,
  body = body,
  skin = "blue"
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Instantiate values
  
  values <- shiny::reactiveValues(selected = NULL)

  env_tabs <- shiny::reactiveValues(
    existing_tables = list("Dataframes"=dt_list)
    )

  # Input Data UI
  
  output$input_ui <- shiny::renderUI({
    if (input$data_source == "data_frame"){
      shinydashboard::box(
        shiny::selectInput(
          inputId = "df_to_load",
          label = "Select variable",
          choices = env_tabs$existing_tables,
          multiple = FALSE
        ),
        solidHeader = TRUE
      )
    } else if (input$data_source == "import"){
      input_data_importfile_ui()
    }
  })
  
  shiny::observeEvent(input$filein_rawdata, {
    infile <- input$filein_rawdata

    if (is.null(infile)) {
      return(NULL)
    }
    
    GROUPS <- "groups" %in% input$chkbox_inputfileopts
    ANOMALY_TAGS <- "anomalytag" %in% input$chkbox_inputfileopts
    
    dat <- tslabeler:::process_input_file(
      input_file = infile$datapath,
      sep = input$filein_sep,
      quote = input$filein_quote,
      ANOMALY_TAGS = ANOMALY_TAGS,
      GROUPS = GROUPS,
      date_coltype = input$radio_datetime
    )
    
    values$tag_values <- dat$tag_values
    values$tag_choices <- dat$tag_choices
    values$total_pts <- dat$total_pts
    values$total_grps <- dat$total_grps
    values$count_existing_anomalies <- dat$count_existing_anomalies
    values$original <- dat$original
    values$source <- "FILE"
    values$dfname <- input$filein_rawdata$name
    
  })
  
  shiny::observeEvent(input$df_to_load, {
    shiny::req(input$df_to_load)
    if(all(input$df_to_load!="No dataframes/datatables in memory")){
      out <- eval(parse(text = input$df_to_load))
      values$dfname <- input$df_to_load
      values$original <- out
    }
  })

  output$sample_input <- reactable::renderReactable({
    shiny::req(values$original)
    dat <- head(values$original,10)
    values$source <- "ENV"
    reactable::reactable(dat)
  })
  
  shiny::observeEvent(input$btn_selectdata, {
    if(values$source=="FILE")
        values$selected <- values$original
    if(values$source=="ENV"){
      dat <- tslabeler:::process_dt_from_env(out = values$original)
      values$tag_values <- dat$tag_values
      values$tag_choices <- dat$tag_choices
      values$total_pts <- dat$total_pts
      values$total_grps <- dat$total_grps
      values$count_existing_anomalies <- dat$count_existing_anomalies
      values$selected <- dat$original
    }
  })
  
  # Labeler UI
  
  output$labeler_menu <- shinydashboard::renderMenu({
    if (is.null(values$selected)) {
      p <- shinydashboard::sidebarMenu(shinydashboard::menuItem(
        text = "Inputs",
        tabName = "input_data",
        icon = icon("table")
      ))
    } else {
      p <- shinydashboard::sidebarMenu(
        shinydashboard::menuItem(
          text = "Inputs",
          tabName = "input_data",
          icon = icon("table")
        ),
        shinydashboard::menuItem(
          text = "Labeler",
          tabName = "labeler",
          startExpanded = TRUE,
          selected = TRUE
        ),
        
        shinyWidgets::pickerInput(
          inputId = "picker_group",
          label = "Group",
          choices = values$selected[, unique(grp)],
          selected = values$selected[, unique(grp)][1],
          options = list(`live-search` = TRUE,
                         `actions-box` = TRUE),
          multiple = TRUE
        ),
        shiny::dateRangeInput(
          inputId = "daterange",
          label = "Date range",
          start = values$selected[, min(ds)],
          end = values$selected[, max(ds)]
        ),
        shinyWidgets::prettyCheckboxGroup(
          inputId = "chkbox_plotopts",
          label = "Plot Options",
          choiceNames = c("Show anomalies",
                          "Show legend"),
          choiceValues = c("anomaly", "legend"),
          selected = c("anomaly", "legend"),
          status = "info"
        ),
        shinyWidgets::actionBttn(
          inputId = "btn_newtag",
          label = NULL,
          style = "material-circle",
          color = "primary",
          size = "xs",
          icon = shiny::icon("plus")
        ),
        shinyWidgets::prettyRadioButtons(
          inputId = "radio_taglist",
          label = "Tags",
          choiceNames = values$tag_choices,
          choiceValues = values$tag_values,
          inline = TRUE,
          status = "danger",
          fill = TRUE
        ),
        shiny::actionButton("mark", "Mark Tags", icon = shiny::icon("bullseye"))
      )
      
    }
    p
  })

  filtered_data <- shiny::reactive({
    values$pts_selected_grps <-
      values$selected[grp %in% input$picker_group, .N]
    values$selected[grp %in% input$picker_group &
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
      values$tag_values <- c(values$tag_values,
                             input$textinput_customtag)
    }
  })

  output$tsplot <- shiny::renderPlot({
    shiny::req(values$selected)
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

  output$outtable <- reactable::renderReactable({
    shiny::req(values$selected)
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
    shiny::req(values$selected)
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
      values$selected[!seldat[, .(ds, grp, anomaly, tag)], on = c("ds", "grp")]
    new <- data.table::rbindlist(list(unmodified, seldat))

    data.table::setkeyv(new, c("ds"))

    values$selected <- new
  })

  output$plot_anomalybar <- shiny::renderPlot({
    shiny::req(values$selected)
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

  shiny::observeEvent(input$btn_save_to_env, {
      assign(values$dfname, values$selected, envir = .GlobalEnv)
  })
  
}

shiny::shinyApp(ui, server, options = list(port = 4686))