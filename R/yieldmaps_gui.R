#' @title Main GUI for yield interpolation
#' @description TODO
#' @importFrom shiny conditionalPanel div downloadHandler icon modalDialog need observe
#'  observeEvent reactive reactiveValues removeModal renderPlot renderText renderUI
#'  runApp shinyApp showModal sliderInput span stopApp uiOutput
#'  updateCheckboxInput updateSliderInput updateTextInput validate
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage
#'  dashboardSidebar menuItem sidebarMenu tabItem tabItems updateTabItems
#' @importFrom shinyFiles getVolumes parseDirPath shinyDirButton shinyDirChoose
#' @importFrom shinyWidgets sendSweetAlert
#' @importFrom shinyjs useShinyjs disable disabled enable
#' @importFrom sf st_read st_crs st_transform st_buffer
#' @importFrom stats quantile
#' @importFrom graphics hist
#' @importFrom ggplot2 aes geom_histogram geom_vline ggplot xlab ylab ylim
#' @importFrom foreach foreach "%do%"
#' @importFrom magrittr "%>%"
#' @importFrom leaflet addCircles addLegend addPolygons addProviderTiles
#'  clearShapes colorBin fitBounds leaflet leafletProxy providerTileOptions renderLeaflet
#' @importFrom methods as
#' @importFrom RJSONIO fromJSON toJSON
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0


yieldmaps_gui <- function() {

  # run
  if (interactive()) {
    options(device.ask.default = FALSE)
    return(shiny::runApp(
      system.file("app", package = "yieldmaps"),
      display.mode = "normal"
    ))
  } else {
    stop("The function must be run from an interactive R session.")
  }

}
