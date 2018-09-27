#' @title GUI for filtering yield data
#' @description TODO
#' @param indata TODO
#' @import data.table
#' @importFrom graphics plot
#' @importFrom gstat fit.variogram variogram vgm
#' @importFrom sp CRS
#' @importFrom shiny actionButton column fluidRow HTML icon numericInput
#'  observeEvent plotOutput radioButtons reactive renderPlot renderUI
#'  runApp shinyApp sliderInput stopApp uiOutput updateNumericInput
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage
#'  dashboardSidebar menuItem sidebarMenu tabItem tabItems
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

fit_yieldvgm_gui <- function(indata) {

  indata_sp <- yield.makespdf(indata, outcrs = CRS("+init=epsg:3035"))

  # shiny
  fit.yieldvgm.shiny <- shinyApp(

    ui = dashboardPage(
      dashboardHeader(title="Yield map generator"),

      dashboardSidebar(
        sidebarMenu(
          menuItem("Point interpolation", tabName = "tab_interp", icon = icon("image"))
        )
      ),

      dashboardBody(
        tabItems(

          ### Point interpolation ###
          tabItem(
            tabName = "tab_interp",

            fluidRow(

              # Left column (check elements, maybe move in sidebar)
              column(
                width=4,

                box(
                  width=12,
                  uiOutput("interp_sampling"),
                  uiOutput("vgm_cutoff")
                ),

                box(
                  title="Parameters",
                  width=12,
                  radioButtons("model", label = "Model type",
                               choices = list("Exponential"="Exp", "Spheroidal"="Sph",
                                              "Gaussian"="Gau", "Wavelength"="Wav")),
                  numericInput("sill",
                               label = "Sill",
                               value = 1),
                  numericInput("nugget",
                               label = "Nugget",
                               value = 0),
                  numericInput("range",
                               label = "Range",
                               value = 100),
                  actionButton("fit_vgm", "Try automatic fit"),
                  uiOutput("err_fit_vgm")


                ),

                # Save button
                shiny::tags$head(shiny::tags$script(src = "message-handler.js")),
                actionButton("save_vgm", "Save variogram parameters")

              ),
              column(
                width=8,
                plotOutput("v_plot",height="300px")
              )

            )

          )

        )

      )

    ),

    server = function(input,inputdata,output,session) {

      # Define UI objects dinamically for parameters
      output$interp_sampling <- renderUI({
        sliderInput(inputId="interp_sampling",
                    label="Number of points to use:",
                    min = 0,
                    max = ceiling(length(indata_sp)),
                    value = min(1E4,ceiling(length(indata_sp))),
                    step = 1)
      })
      output$vgm_cutoff <- renderUI({
        sliderInput(inputId="vgm_cutoff",
                    label="Cutoff:",
                    min = 10,
                    max = ceiling(sqrt(sum(apply(indata_sp@bbox,1,diff)^2))),
                    value = round(sqrt(sum(apply(indata_sp@bbox,1,diff)^2))/3),
                    step = 1)
      })

      indata_sel <- reactive({
        if (is.null(input$interp_sampling)) {
          indata_sp[indata_sp$sid<=1E3,]
        } else {
          indata_sp[indata_sp$sid<=input$interp_sampling,]
        }
      })

      v.man <- reactive({
        vgm(psill=input$sill, model=input$model, range=input$range, nugget=input$nugget)
      })

      v <- reactive({
        if (is.null(input$vgm_cutoff)) {
          variogram(yield ~ 1, indata_sel(), cutoff=round(sqrt(sum(apply(indata_sel()@bbox,1,diff)^2))/3))
        } else {
          variogram(yield ~ 1, indata_sel(), cutoff=input$vgm_cutoff)
        }

      })

      #output$err_fit_vgm <- renderUI({" "})

      fitVgm <- observeEvent(input$fit_vgm, {
        fit_vgm <- function() {fit.variogram(v(), v.man())}
        suppressWarnings(v.man <- fit_vgm())
        output$err_fit_vgm <- if (is(tryCatch(fit_vgm(),warning=function(w){"warning"}), "variogramModel")) {
          renderUI({HTML("<font color=\"#FF0000\">Singular fit: values can be unsuitable. Please try after having manually edited values.</font>")})
        } else {
          renderUI({HTML("<font color=\"#008800\">Automatic fit runned without errors.</font>")})
        }
        updateNumericInput(session, "sill", value=v.man[2,2])
        updateNumericInput(session, "nugget", value=v.man[1,2])
        updateNumericInput(session, "range", value=v.man[2,3])
      })

      output$vgm_sill <- renderUI({
        sliderInput(inputId="interp_sampling",
                    label="Number of points to use:",
                    min = 0,
                    max = ceiling(length(indata_sp)),
                    value = min(1E4,ceiling(length(indata_sp))),
                    step = 1)
      })

      # Histogram of yield
      output$v_plot <- renderPlot({
        plot(v(),v.man())
      })

      vgmVals <- observeEvent(input$save_vgm, {
        vgm.fit <- v.man()
        attr(vgm.fit,"cutoff") <- input$vgm_cutoff
        attr(vgm.fit,"n_points") <- input$interop_sampling
        stopApp(vgm.fit)
      })

    })

  # run
  if (interactive()) {
    options(device.ask.default = FALSE)
    return(runApp(fit.yieldvgm.shiny))
  } else {
    # stop("The function must be run from an interactive R session.")
  }

}
