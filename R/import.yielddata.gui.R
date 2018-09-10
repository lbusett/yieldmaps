#' @title GUI for loading raw yield data
#' @description TODO
#' @importFrom shiny callModule fileInput h2 icon need observe radioButtons
#'  reactive runApp shinyApp validate
#' @importFrom shinydashboard dashboardBody dashboardHeader dashboardPage
#'  dashboardSidebar menuItem sidebarMenu tabItem tabItems
#' @export
#' @author Luigi Ranghetti, phD (2018) \email{ranghetti.l@@irea.cnr.it}
#' @note License: GPL 3.0

import.yielddata.gui <- function() {

  # File selection with module
  .yieldFile <- function(input, output, session) {
    # The selected file, if any
    userRawdata <- reactive({
      # If no file is selected, don't do anything
      validate(need(input$rawdata, message = FALSE))
      input$rawdata
    })

    # The user's data, parsed into a data frame
    dataframe <- reactive({
      read.yielddata(userRawdata()$datapath,
                     format = "custom_bonifiche")
    })

    # We can run observers in here if we want to
    observe({
      msg <- sprintf("File %s was uploaded", userRawdata()$name)
      cat(msg, "\n")
    })

    # Return the reactive that yields the data frame
    return(dataframe)
  }

  # shiny
  import.yielddata.shiny <- shinyApp(

    options = list(height = 1000, width = 500),

    ui = dashboardPage(
      dashboardHeader(title="Yield map generator"),

      dashboardSidebar(
        sidebarMenu(
          menuItem("Input selection", tabName = "tab_input", icon = icon("folder-open"))
        )
      ),

      dashboardBody(
        tabItems(

          ### Input selection ###
          tabItem(
            tabName = "tab_input",
            h2("Input selection"),

            fileInput('rawdata', 'Choose raw file',
                      accept=c('text/csv',
                               'text/comma-separated-values,text/plain',
                               '.csv')),

            radioButtons('rawformat', 'Choose format',
                         c(None='',
                           'Custom (Bonifiche Ferraresi)'='custom_bonifiche'),
                         '"')
          )

        )

      )
    ),

    server = function(input,inputdata,output,session) {

      indata <- callModule(.yieldFile, "indata")

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
