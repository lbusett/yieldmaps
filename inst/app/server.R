
function(input, output, session) {

  require(shiny)
  require(shinydashboard)
  require(shinyFiles)
  require(shinyWidgets)
  require(shinyjs)
  require(sf)
  require(stats)
  require(graphics)
  require(ggplot2)
  require(foreach)
  require(magrittr)
  require(leaflet)
  require(methods)
  require(RJSONIO)
  require(yieldmaps)

  rv <- reactiveValues()


  #### Tab: Input ####
  ## Box: Input ####

  # change maximum upload file size to 250MB
  options(shiny.maxRequestSize = 250 * 1024^2)

  # fix multiple file inputs (e.g. shapefiles)
  observeEvent(input$rawdata_paths, {
    rv$vectfile_path <- tryCatch({
      yieldmaps:::shiny_input_shapes(input$rawdata_paths, multiple = TRUE)
    }, error = function(e) {
      sendSweetAlert(
        session, title = "Error loading files",
        text = e$message, type = "error", btn_labels = "Ok"
      )
      ""
    })
    if (!is.character(rv$vectfile_path)) {
      enable("rawformat")
      enable("load_button")
    }
  })


  # The user's data, parsed into a data frame
  observeEvent(input$load_button, {

    # if files are not null, import them
    validate(
      need(input$rawdata_paths, message = "Load one or more files")
    )
    loaded_yielddata <- tryCatch({
      rv$pt_yielddata <- read.yielddata(
        rv$vectfile_path,
        format = input$rawformat
      )
    }, error = function(e) {
      rv$pt_yielddata <- NULL
      paste("Errore nel caricamento dei files:", e$message)
    })
    output$pt_yielddata_errormess <- renderText({
      if (!is.character(loaded_yielddata)) {
        enable("filter_button")
        enable("autofilter_button")
        enable("nofilter_button")
        "\u2714"
      }  else {
        loaded_yielddata
      }
    })

  })


  ## Box: field shape ####

  # fix multiple file inputs (e.g. shapefiles)
  observeEvent(input$fields_path, {
    rv$vectfields_path <- tryCatch({
      yieldmaps:::shiny_input_shapes(input$fields_path, multiple = FALSE)
    }, error = function(e) {
      sendSweetAlert(
        session, title = "Error loading files",
        text = e$message, type = "error", btn_labels = "Ok"
      )
      ""
    })
    if (!is.null(rv$vectfields_path)) {
      rv$vectfields <- st_read(rv$vectfields_path$datapath, quiet=TRUE)
    }
  })



  ## Box: output ####

  # get server volumes
  volumes <- c("Home"=path.expand("~"), getVolumes()())

  shinyDirChoose(input, "path_out_sel", roots = volumes)
  # if paths change after using the shinyDirButton, update the values and the textInput
  observe({
    path_out_string <- parseDirPath(volumes, input$path_out_sel)
    updateTextInput(session, "path_out_textin", value = path_out_string)
  })

  # check CRS
  output$outproj_message <- renderUI({

    # if required, take from reference raster
    rv$outproj_validated <- tryCatch(
      st_crs2(input$out_proj),
      error = function(e) {st_crs(NA)}
    )$proj4string
    if (input$out_proj=="") {
      ""
    }  else if (is.na(rv$outproj_validated)) {
      span(style="color:red", "\u2718")
    } else {
      span(style="color:darkgreen", "\u2714")
    }

  })



  ## Box: processing ####

  # Manual filter
  observeEvent(input$filter_button, {
    rv$vectfields <- st_transform(rv$vectfields, st_crs2(input$out_proj))
    filtered_yielddata <- tryCatch({
      rv$pt_yielddata <- filter.yieldpoints.reset(rv$pt_yielddata) %>%
        filter.yieldpoints(
          "rangeq", c(.02,.98), # filter points < 2° and > 98° percentiles
          inlayer = as(rv$vectfields, "Spatial"),
          id_fieldname="id_geom", # TODO add GUI input for it
          byfield = TRUE, samplesize = NA
        ) %>%
        filter.yieldpoints(
          "pos", 5, # filter 5 m from border
          inlayer = as(rv$vectfields, "Spatial"),
          id_fieldname="id_geom", # TODO add GUI input for it
          byfield = TRUE, samplesize = NA
        )
    }, error = function(e) {
      paste("Errore nel filtraggio dei files:", e$message)
    })
    output$autofilter_errormess <- renderText({
      if (!is.character(filtered_yielddata)) {
        "\u2714"
        enable("interp_button")
      }  else {
        filtered_yielddata
        disable("interp_button")
      }
    })
    output$enable_tab_filter <- renderText({TRUE})
    updateTabItems(session, "tabs", "tab_filter")
  })

  # Automatic filter
  observeEvent(input$autofilter_button, {
    rv$vectfields <- st_transform(rv$vectfields, st_crs2(input$out_proj))
    filtered_yielddata <- tryCatch({
      rv$pt_yielddata <- filter.yieldpoints.reset(rv$pt_yielddata) %>%
        filter.yieldpoints(
          "rangeq", c(.02,.98), # filter points < 2° and > 98° percentiles
          inlayer = as(rv$vectfields, "Spatial"),
          id_fieldname="id_geom", # TODO add GUI input for it
          byfield = TRUE, samplesize = NA
        ) %>%
        filter.yieldpoints(
          "pos", 5, # filter 5 m from border
          inlayer = as(rv$vectfields, "Spatial"),
          id_fieldname="id_geom", # TODO add GUI input for it
          byfield = TRUE, samplesize = NA
        )
    }, error = function(e) {
      paste("Errore nel filtraggio dei files:", e$message)
    })
    output$autofilter_errormess <- renderText({
      if (!is.character(filtered_yielddata)) {
        "\u2714"
        enable("interp_button")
      }  else {
        filtered_yielddata
        disable("interp_button")
      }
    })
    output$enable_tab_filter <- renderText({FALSE})
  })

  # No filter
  observeEvent(input$nofilter_button, {
    rv$vectfields <- st_transform(rv$vectfields, st_crs2(input$out_proj))
    rv$pt_yielddata <- filter.yieldpoints.reset(rv$pt_yielddata)
    output$autofilter_errormess <- renderText("")
    enable("interp_button")
    output$enable_tab_filter <- renderText({FALSE})
  })

  # Enable filter tab
  output$enable_tab_filter <- renderText({FALSE})
  outputOptions(output, "enable_tab_filter", suspendWhenHidden = FALSE)
  # observe({
  #   output$enable_tab_filter <- renderText(
  #     if (!is.null(rv$pt_yielddata) & !is.null(rv$vectfields)) {
  #       TRUE
  #     } else {
  #       FALSE
  #     }
  #   )
  # })



  observeEvent(input$interp_button, {

    # Open waiting message
    interp_waiting_modal <- modalDialog(
      title = NULL,
      size = "s",
      div(
        align="center",
        "L'interpolazione dei dati è in corso, attendere prego..."
      ),
      easyClose = FALSE,
      footer = NULL
    )
    showModal(interp_waiting_modal)

    rv$out_raster_path <- yieldmaps_process(
      rv$pt_yielddata, as(rv$vectfields, "Spatial"),
      grid_path = NA, filtered = TRUE,
      id_fieldname="id_geom", # TODO add GUI input for it
      interp_dir = input$path_out_textin,
      out_crs = rv$outproj_validated,
      buffer_radius = 15, # TODO add GUI input for it
      vgm = NA
    )

    removeModal()

    sendSweetAlert(
      session = session, type = "success",
      title = "Dati correttamente interpolati",
      text = shiny::HTML(
        "<p>I dati di resa sono stati correttamente interpolati.</p>",
        "<p>Il raster risultante si trova qui:<br/><tt>",
        rv$out_raster_path,"</tt></p>."
      ),
      btn_labels = c("Ok"),
      closeOnClickOutside = FALSE,
      html = TRUE
    )


  })


  #### Tab: filter ####

  source(
    system.file("app/app_codes/filter_yieldpoints_gui.code.R", package="yieldmaps"),
    local=TRUE
  )$value
  # output$tab_filter_content <- renderUI({
  #   # if (is.null(rv$pt_yielddata)) {
  #   #   "Caricare prima dei dati di resa validi."
  #   # } else {
  #     source("app/app_codes/filter_yieldpoints_gui.tab.R", local=TRUE)$value
  #   # }
  # })



} # end of yieldmaps_gui.server
