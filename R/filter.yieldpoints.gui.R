#' @title GUI for filtering yield data
#' @description TODO
#' @param indata TODO
#' @param inlayer TODO #TODO maske optional
#' @param samplesize TODO
#' @import data.table
#' @importFrom data.table copy
#' @importFrom graphics hist
#' @importFrom stats quantile
#' @importFrom ggplot2 aes geom_histogram geom_vline ggplot xlab ylab ylim
#' @importFrom leaflet addCircles addLegend addPolygons addProviderTiles
#'  clearShapes colorBin fitBounds leaflet leafletOutput leafletProxy
#'  providerTileOptions renderLeaflet
#' @importFrom RJSONIO fromJSON toJSON
#' @importFrom shiny actionButton checkboxInput column div downloadButton
#'  downloadHandler fileInput fluidRow icon observe observeEvent plotOutput
#'  radioButtons reactive renderPlot renderUI runApp selectInput shinyApp
#'   sliderInput stopApp strong uiOutput updateCheckboxInput updateSliderInput
#' @importFrom shinydashboard box dashboardBody dashboardHeader dashboardPage
#'  dashboardSidebar menuItem sidebarMenu tabItem tabItems
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

filter.yieldpoints.gui <- function(indata, inlayer, samplesize) {

  # TODO controls on indata format

  # work on a copy
  outdata <- copy(indata)

  # reset eventual previous filtering
  filter.yieldpoints.reset(outdata)

  f_vars <- c("f_rangev", "f_smv", "f_mins", "f_rangey", "f_stdy", "f_rangeq", "f_pos")

  # default filtering values
  filters <- list()

  # shiny
  filter.yieldpoints.shiny <- shinyApp(

    options = list(height = 1000, width = 500),

    ui = dashboardPage(
      dashboardHeader(title="Yield map generator"),

      dashboardSidebar(
        sidebarMenu(
          menuItem("Point filterer", tabName = "tab_filter", icon = icon("search"))
        )
      ),

      dashboardBody(
        tabItems(

          ### Point filterer ###
          tabItem(
            tabName = "tab_filter",
            fluidRow(

              # Left column (check elements, maybe move in sidebar)
              column(
                width=3,

                box(
                  title="Sampling",
                  width=12,
                  radioButtons("sampling", label = "Sample dataset?",
                               choices = list("Yes" = TRUE, "No" = FALSE),selected = 1)
                ),

                # numericInput("sample_size", label = h4("Sample size"), value = 1E3),
                # sliderInput(
                #   inputId="sample_relsize",
                #   label="Sample dataset:",
                #   min = 0, #trunc(min(outdata@data$yield)),
                #   max = 1,
                #   value = 0.25,
                #   step = 0.01),
                # uiOutput("indata_rangey"),

                box(
                  title="Filters",
                  width=12,

                  uiOutput("indata_rangev"),
                  checkboxInput("check_rangev", label = "Activate above filter", value = FALSE),
                  sliderInput(
                    inputId="mins",
                    label="Minimum relative harvest width:",
                    min = 0, #trunc(min(outdata@data$yield)),
                    max = 100,
                    value = 25,
                    post = "%",
                    step = 1),
                  checkboxInput("check_mins", label = "Activate above filter", value = FALSE),
                  uiOutput("indata_rangey"),
                  checkboxInput("check_rangey", label = "Activate above filter", value = FALSE),
                  sliderInput(
                    inputId="stdy",
                    label="Yield maximum standard deviations:",
                    min = 0, #trunc(min(outdata@data$yield)),
                    max = 5,
                    value = 3,
                    step = 0.25),
                  checkboxInput("check_stdy", label = "Activate above filter", value = FALSE),
                  sliderInput(
                    inputId="rangeq",
                    label="Yield range of quantiles:",
                    min = 0, #trunc(min(outdata@data$yield)),
                    max = 100,
                    value = c(2,98),
                    post = "%",
                    step = 1),
                  checkboxInput("check_rangeq", label = "Activate above filter", value = FALSE),
                  sliderInput(
                    inputId="pos",
                    label="Buffer on field boundaries:",
                    min = 0, #trunc(min(outdata@data$yield)),
                    max = 50,
                    value = 0,
                    post = " m",
                    step = 0.1),
                  checkboxInput("check_pos", label = "Activate above filter", value = FALSE)
                ),

                # Export filters
                tags$head(tags$script(src = "message-handler.js")),
                strong("Export filter values: "),
                downloadButton("downloadFilters", "Download"),

                # Import filters
                fileInput("importFilters", 'Import filter values: ',
                          accept=c('text/json',
                                   '.json')),

                # Save button
                actionButton("save_filtered", "Apply filters", icon=icon("icon-ok"))


              ),

              # Right (main) column (outputs)
              column(
                width=9,

                fluidRow(
                  box(
                    title="Histograms",
                    width=12,
                    fluidRow(
                      column(7,
                             plotOutput("yield_hist",height="300px")
                      ),
                      column(5,
                             div(style = "height:70px;",
                                 selectInput("hist_selvariable", "Select variable to plot:",
                                             c("Machine velocity"="speed","Harvesting width"="rel_width"), selected="speed")),
                             plotOutput("sec_hist", height="230px")
                      )
                    )
                  )
                ),

                fluidRow(
                  box(
                    title="Map",
                    width=12,
                    div(style = "height:20px;"),
                    div(style="display:inline-block; vertical-align:middle;",
                        strong("Select variable to map:")),
                    div(style="display:inline-block; vertical-align:middle;",
                        selectInput("map_selvariable", NA, c("Yield"="yield","Machine velocity"="speed","Harvesting width"="rel_width"), selected="yield")),
                    leafletOutput("map_map", height="500px")
                  )
                )

              )

            )

          )

        )

      )
    ),

    server = function(input,inputdata,output,session) {

      # Define UI objects dinamically for filtering
      output$indata_rangev <- renderUI({
        sliderInput(inputId="rangev",
                    label="Machine speed:",
                    min = trunc(min(outdata@data$speed)),
                    max = ceiling(max(outdata@data$speed)),
                    value = quantile(outdata@data$speed,c(0,1)),
                    post = " km/h",
                    step = 0.1)
      })
      output$indata_rangey <- renderUI({
        sliderInput(inputId="rangey",
                    label="Yield range:",
                    min = 0, #trunc(min(outdata@data$yield)),
                    max = ceiling(max(outdata@data$yield)),
                    value = quantile(outdata@data$yield,c(0.02,0.98)),
                    post = " t",
                    step = 0.1)
      })

      # Define data with filters
      # filtdata <- reactive({
      #   filter.yieldpoints(outdata,"rangev",input$rangev,samplesize=NA)
      #   filter.yieldpoints(outdata,"mins",input$mins/100,samplesize=NA)
      #   filter.yieldpoints(outdata,"rangey",input$rangey,samplesize=NA)
      #   filter.yieldpoints(outdata,"stdy",input$stdy,samplesize=NA)
      #   filter.yieldpoints(outdata,"rangeq",input$rangeq/100,samplesize=NA)
      #   filter.yieldpoints(outdata,"pos",input$pos,inlayer=inlayer,samplesize=NA)
      # })
      # observe({
      #   outdata <- filter.yieldpoints(outdata,"rangev",input$rangev,samplesize=NA)
      # })
      # observe({
      #   outdata <- filter.yieldpoints(outdata,"mins",input$mins/100,samplesize=NA)
      # })
      # observe({
      #   outdata <- filter.yieldpoints(outdata,"rangey",input$rangey,samplesize=NA)
      # })
      # observe({
      #   outdata <- filter.yieldpoints(outdata,"stdy",input$stdy,samplesize=NA)
      # })
      # observe({
      #   outdata <- filter.yieldpoints(outdata,"rangeq",input$rangeq/100,samplesize=NA)
      # })
      # observe({
      #   outdata <- filter.yieldpoints(outdata,"pos",input$pos,inlayer=inlayer,samplesize=NA)
      # })

      observe({
        if (input$check_rangev) {
          outdata <- filter.yieldpoints(outdata,"rangev",input$rangev,samplesize=NA)
        } else {
          filter.yieldpoints.reset(outdata,"rangev")
        }
      })
      observe({
        if (input$check_mins) {
          outdata <- filter.yieldpoints(outdata,"mins",input$mins/100,samplesize=NA)
        } else {
          filter.yieldpoints.reset(outdata,"mins")
        }
      })
      observe({
        if (input$check_rangey) {
          outdata <- filter.yieldpoints(outdata,"rangey",input$rangey,samplesize=NA)
        } else {
          filter.yieldpoints.reset(outdata,"rangey")
        }
      })
      observe({
        if (input$check_stdy) {
          outdata <- filter.yieldpoints(outdata,"stdy",input$stdy,samplesize=NA)
        } else {
          filter.yieldpoints.reset(outdata,"stdy")
        }
      })
      observe({
        if (input$check_rangeq) {
          outdata <- filter.yieldpoints(outdata,"rangeq",input$rangeq/100,samplesize=NA)
        } else {
          filter.yieldpoints.reset(outdata,"rangeq")
        }
      })
      observe({
        if (input$check_pos) {
          outdata <- filter.yieldpoints(outdata,"pos",input$pos,inlayer=inlayer,samplesize=NA)
        } else {
          filter.yieldpoints.reset(outdata,"pos")
        }
      })


      # Histogram of yield
      yield_hist_breaks <- reactive({hist(plot = FALSE, outdata@data[sid<=samplesize,]$yield, breaks=50)$breaks})
      yield_hist_ylim <- reactive({c(0,max(hist(plot = FALSE, outdata@data[sid<=samplesize,]$yield, breaks=50)$counts))})
      output$yield_hist <- renderPlot({
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # hist(outdata@data$yield, breaks = bins, col = 'darkgray', border = 'white')
        # ggplot(outdata@data[yield>quantile(yield,input$rangeq[1]/100)&yield<quantile(yield,input$rangeq[2]/100)], aes(x=yield)) +
        #   geom_histogram(colour="white", breaks=yield_hist_breaks)
        ggplot(outdata@data, aes(x=yield)) +
          geom_histogram(data=outdata@data[sid<=samplesize&filter==FALSE,], colour="white", fill="darkgreen", breaks=yield_hist_breaks()) +
          geom_histogram(data=outdata@data[sid<=samplesize&filter==TRUE,], colour="white", fill="red", breaks=yield_hist_breaks()) +
          geom_vline(xintercept=input$rangey, colour="red") +
          geom_vline(xintercept=quantile(outdata@data$yield,input$rangeq/100), colour="red", linetype="dashed") +
          ylim(yield_hist_ylim()) +
          xlab("Yield (t)") + ylab("Count")
      })

      # Secondary histogram
      hist_breaks <- reactive({hist(plot = FALSE, outdata@data[sid<=samplesize,][[input$hist_selvariable]], breaks=30)$breaks})
      hist_ylim <- reactive({c(0,max(hist(plot = FALSE, outdata@data[sid<=samplesize,][[input$hist_selvariable]], breaks=30)$counts))})
      output$sec_hist <- renderPlot({
        ggplot(outdata@data, aes(x=get(input$hist_selvariable))) +
          geom_histogram(data=outdata@data[sid<=samplesize&filter==FALSE,], colour="white", fill="darkcyan", breaks=hist_breaks()) +
          geom_histogram(data=outdata@data[sid<=samplesize&filter==TRUE,], colour="white", fill="red", breaks=hist_breaks()) +
          geom_vline(xintercept = if (input$hist_selvariable=="speed") {input$rangev}
                     else if (input$hist_selvariable=="rel_width") {input$mins/100}
                     else NA, colour="red") +
          ylim(hist_ylim()) +
          xlab(input$hist_selvariable) + ylab("Count")
      })


      # Create the map
      output$map_map <- renderLeaflet({
        leaflet() %>%
          addProviderTiles(providers$Esri.WorldImagery,
                           options = providerTileOptions(noWrap=TRUE, maxZoom=22)
          ) %>%
          addPolygons(data=inlayer, color="blue", fill=NA, weight=2.5 #, dashArray="8,6"
          ) %>%
          # setView(lng = 11.96, lat = 44.86, zoom = 15)
          fitBounds(lng1 = min(outdata@data$lon),
                    lat1 = min(outdata@data$lat),
                    lng2 = max(outdata@data$lon),
                    lat2 = max(outdata@data$lat))
      })

      # Update the map
      observe({
        colorData <- outdata@data[sid<samplesize&filter==FALSE,]
        pal <- colorBin("RdYlGn", range(colorData[[input$map_selvariable]],na.rm=TRUE), bins=5, pretty=TRUE)
        leafletProxy("map_map") %>%
          clearShapes() %>%
          addPolygons(data=inlayer, color="blue", fill=NA, weight=2.5) %>%
          addCircles(~lon, ~lat, data=outdata@data[sid<samplesize&filter==TRUE,],
                     radius=1.5, stroke=FALSE, fillOpacity=0.4, fillColor="gray") %>%
          addCircles(~lon, ~lat, data=colorData,
                     radius=1.5, stroke=FALSE, fillOpacity=0.65, fillColor=~pal(colorData[[input$map_selvariable]])) %>%
          addPolygons(data=aeqd.buffer(inlayer, width=-input$pos), color="red", fill=NA, weight=2, dashArray="8,6") %>%
          # addCircles(~lon, ~lat, data=outdata@data[sid<samplesize&filter==FALSE,],
          #            radius=1.5, stroke=FALSE, fillOpacity=0.4, fillColor=~colorBin("RdYlGn", yield)(yield)) %>%
          addLegend("bottomleft", pal=pal, values=colorData, #title=input$map_selvariable,
                    layerId="colorLegend")
      })

      # Export filtering parameters
      output$downloadFilters <- downloadHandler(
        filename = "filters.json",
        content = function(file) {
          filters <- toJSON(list("rangev"=input$rangev,
                                 "mins"=input$mins/100,
                                 "rangey"=input$rangey,
                                 "stdy"=input$stdy,
                                 "rangeq"=input$rangeq/100,
                                 "pos"=input$pos,
                                 "check_rangev"=input$check_rangev,
                                 "check_mins"=input$check_mins,
                                 "check_rangey"=input$check_rangey,
                                 "check_stdy"=input$check_stdy,
                                 "check_rangeq"=input$check_rangeq,
                                 "check_pos"=input$check_pos))
          write(filters, file)
        }
      )

      # Import filters
      observe({
        if(!is.null(input$importFilters)) {
          imported_filters <- fromJSON(file(input$importFilters$datapath,"r"))
          updateSliderInput(session, "rangev", value=imported_filters[["rangev"]])
          updateSliderInput(session, "mins", value=imported_filters[["mins"]]*100)
          updateSliderInput(session, "rangey", value=imported_filters[["rangey"]])
          updateSliderInput(session, "stdy", value=imported_filters[["stdy"]])
          updateSliderInput(session, "rangeq", value=imported_filters[["rangeq"]]*100)
          updateSliderInput(session, "pos", value=imported_filters[["pos"]])
          updateCheckboxInput(session, "check_rangev", value=imported_filters[["check_rangev"]])
          updateCheckboxInput(session, "check_mins", value=imported_filters[["check_mins"]])
          updateCheckboxInput(session, "check_rangey", value=imported_filters[["check_rangey"]])
          updateCheckboxInput(session, "check_stdy", value=imported_filters[["check_stdy"]])
          updateCheckboxInput(session, "check_rangeq", value=imported_filters[["check_rangeq"]])
          updateCheckboxInput(session, "check_pos", value=imported_filters[["check_pos"]])
        }
      })


      # Save filtered data
      observeEvent(input$save_filtered, {
        stopApp(outdata)
      })

    }
  )

  # run
  if (interactive()) {
    options(device.ask.default = FALSE)
    return(runApp(filter.yieldpoints.shiny))
  } else {
    stop("The function must be run from an interactive R session.")
  }

}
