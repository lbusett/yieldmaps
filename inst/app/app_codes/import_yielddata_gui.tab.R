shinydashboard::tabItem(
  tabName = "tab_input",

  ### Input selection ###
  shinydashboard::box(
    title = "Dati di resa in input",

    shiny::fileInput(
      'rawdata_paths', 'Scegli uno o più files di resa',
      accept=c(
        'text/plain','application/x-esri-shape'
      ),
      multiple = TRUE
    ),

    shinyjs::disabled(shiny::radioButtons(
      'rawformat', 'Formato',
      c(
        None='',
        'Custom (IBF < 2018)'='custom_bonifiche',
        'Custom (IBF >= 2018)'='custom_ibf'
      ),
      selected = 'custom_ibf'
    )),

    shinyjs::disabled(actionButton("load_button", "Carica i files")),
    shiny::textOutput("pt_yielddata_errormess")

  ), # end of input box


  ### Fields vector ###
  shinydashboard::box(
    title = "Vettoriale dei campi",

    shiny::fileInput(
      'fields_path', 'Scegli il vettoriale dei campi',
      accept=c(
        'application/x-esri-shape'
      ),
      multiple = TRUE
    )


  ), # end of fields box


  ### Output path ###
  shinydashboard::box(
    title = "Impostazioni di output",

    shiny::div(style="display:inline-block;vertical-align:top;",
        shiny::strong("Cartella di output: \u00a0")),
    # shiny::div(style="display:inline-block;vertical-align:top;",
    #     htmlOutput("path_out_errormess")),
    shiny::div(shiny::div(style="display:inline-block;vertical-align:top;width:70pt;",
            shinyFiles::shinyDirButton("path_out_sel", "Seleziona", "Seleziona la cartella di output delle mappe raster")),
        shiny::div(style="display:inline-block;vertical-align:top;width:calc(100% - 70pt - 3px);",
                   shiny::textInput("path_out_textin", NULL, ""))),

    shiny::strong("Sistema di riferimento:"),
    shiny::div(
      shiny::div(
        style="display:inline-block;position:relative;",
        shiny::textInput("out_proj", NULL,
                  value="32632", width="190px")
      ),
      shiny::div(
        style="display:inline-block;position:relative;bottom:0;margin-left:10px;",
        shiny::uiOutput("outproj_message")
      )
    )

  ), # end of output box


  ## Box: processing ####
  shinydashboard::box(
    title = "Processamento",
    width = 12,

    shiny::div(
      style="vertical-align:center;padding-bottom:10px;",
      shiny::strong("Filtraggio dei punti: \u00a0"),
      shinyjs::disabled(actionButton("filter_button", "Manuale")), "\u00a0",
      shinyjs::disabled(actionButton("autofilter_button", "Automatico")), "\u00a0",
      shinyjs::disabled(actionButton("nofilter_button", "Nessuno")), "\u00a0",
      shiny::uiOutput("autofilter_errormess")
    ),

    shiny::div(
      style="vertical-align:center;padding-bottom:10px;",
      shinyjs::disabled(actionButton("interp_button", "Avvia l'interpolazione"))#,
    )
    # textOutput("interp_errormess"),

  )

) # end of tab_rese
