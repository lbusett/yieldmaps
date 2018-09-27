shinydashboard::tabItem(
  tabName = "tab_filter",
  shiny::fluidRow(

    # Left column (check elements, maybe move in sidebar)
    shiny::column(
      width=3,

      # shinydashboard::box(
      #   title="Sampling",
      #   width=12,
      #   radioButtons(
      #     "sampling_data", label = "Sample dataset?",
      #     choices = list("Yes" = TRUE, "No" = FALSE),
      #     selected = TRUE
      #   )
      # ),

      # numericInput("sample_size", label = h4("Sample size"), value = 1E3),
      # sliderInput(
      #   inputId="sample_relsize",
      #   label="Sample dataset:",
      #   min = 0, #trunc(min(outdata@datatable$yield)),
      #   max = 1,
      #   value = 0.25,
      #   step = 0.01),
      # uiOutput("indata_rangey"),

      shinydashboard::box(
        title="Filtri",
        width=12,

        shiny::uiOutput("indata_rangev"),
        shiny::checkboxInput("check_rangev", label = "Attiva il filtro sovrastante", value = FALSE),
        shiny::sliderInput(
          inputId="mins",
          label="Ampiezza relativa minima della barra:",
          min = 0, #trunc(min(outdata@datatable$yield)),
          max = 100,
          value = 25,
          post = "%",
          step = 1),
        shiny::checkboxInput("check_mins", label = "Attiva il filtro sovrastante", value = FALSE),
        shiny::uiOutput("indata_rangey"),
        shiny::checkboxInput("check_rangey", label = "Attiva il filtro sovrastante", value = FALSE),
        shiny::sliderInput(
          inputId="stdy",
          label="Resa, Deviazione standard massima:",
          min = 0, #trunc(min(outdata@datatable$yield)),
          max = 5,
          value = 3,
          step = 0.25),
        shiny::checkboxInput("check_stdy", label = "Attiva il filtro sovrastante", value = FALSE),
        shiny::sliderInput(
          inputId="rangeq",
          label="Resa, intervallo dei quantili:",
          min = 0, #trunc(min(outdata@datatable$yield)),
          max = 100,
          value = c(2,98),
          post = "%",
          step = 1),
        shiny::checkboxInput("check_rangeq", label = "Attiva il filtro sovrastante", value = TRUE),
        shiny::sliderInput(
          inputId="pos",
          label="Distanza minima dal bordo dei campi:",
          min = 0, #trunc(min(outdata@datatable$yield)),
          max = 50,
          value = 5,
          post = " m",
          step = 0.1),
        shiny::checkboxInput("check_pos", label = "Attiva il filtro sovrastante", value = TRUE)
      ),

      # Export filters
      shiny::tags$head(shiny::tags$script(src = "message-handler.js")),
      shiny::strong("Esporta i filtri impostati: "),
      shiny::downloadButton("downloadFilters", "Scarica"),

      # Import filters
      shiny::fileInput(
        "importFilters", 'Importa i filtri da file: ',
        buttonLabel = "Cerca",
        accept=c('text/json', '.json')
      )#,

      # # Save button
      # actionButton("save_filtered", "Apply filters", icon=icon("icon-ok"))


    ),

    # Right (main) column (outputs)
    shiny::column(
      width=9,

      shiny::fluidRow(
        shinydashboard::box(
          title="Istogrammi",
          width=12,
          fluidRow(
            column(7,
                   plotOutput("yield_hist",height="300px")
            ),
            column(5,
                   div(style = "height:70px;",
                       shiny::selectInput("hist_selvariable", "Variabile da mostrare:",
                                   c("Velocità della macchina"="speed","Ampiezza della barra"="rel_width"), selected="speed")),
                   plotOutput("sec_hist", height="230px")
            )
          )
        )
      ),

      shiny::fluidRow(
        shinydashboard::box(
          title="Map",
          width=12,
          div(style = "height:20px;"),
          div(style="display:inline-block; vertical-align:middle;",
              shiny::strong("Variabile da mostrare:")),
          div(style="display:inline-block; vertical-align:middle; width=200px;",
              shiny::selectInput("map_selvariable", NA, c("Resa"="yield","Velocità della macchina"="speed","Ampiezza della barra"="rel_width"), selected="yield")),
          leaflet::leafletOutput("map_map", height="500px")
        )
      )

    )

  )

)








# #' @title GUI for filtering yield data
# #' @description TODO
# #' @param indata TODO
# #' @param inlayer TODO #TODO maske optional
# #' @param samplesize TODO
# #' @import data.table
# #' @importFrom data.table copy
# #' @importFrom graphics hist
# #' @importFrom stats quantile
# #' @importFrom ggplot2 aes geom_histogram geom_vline ggplot xlab ylab ylim
# #' @importFrom leaflet addCircles addLegend addPolygons addProviderTiles
# #'  clearShapes colorBin fitBounds leaflet leafletOutput leafletProxy
# #'  providerTileOptions renderLeaflet
# #' @importFrom RJSONIO fromJSON toJSON
# #' @importFrom shiny actionButton checkboxInput column div downloadButton
# #'  downloadHandler fileInput fluidRow icon observe observeEvent plotOutput
# #'  radioButtons reactive renderPlot renderUI runApp selectInput shinyApp
# #'   sliderInput stopApp strong uiOutput updateCheckboxInput updateSliderInput
# #' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage
# #'  dashboardSidebar menuItem sidebarMenu tabItem tabItems
# #' @importFrom magrittr "%>%"
# #' @export
# #' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
# #' @note License: GPL 3.0
