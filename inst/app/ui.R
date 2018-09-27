shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title="Interpolatore rese"),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem("Selezione input", tabName = "tab_input", icon = icon("folder-open")),
      shiny::conditionalPanel(
        condition = "output.enable_tab_filter == 'TRUE'",
        shinydashboard::sidebarMenu(
          shinydashboard::menuItem("Filtraggio valori", tabName = "tab_filter", icon = icon("search"))
        )
      )
    ),
    shinyjs::useShinyjs()
  ),

  shinydashboard::dashboardBody(
    shinydashboard::tabItems(
      source(
        system.file("app/app_codes/import_yielddata_gui.tab.R", package="yieldmaps"),
        local=TRUE
      )$value,
      source(
        system.file("app/app_codes/filter_yieldpoints_gui.tab.R", package="yieldmaps"),
        local=TRUE
      )$value
    )
  ) # end of dashboardBody

) # end of dashboardPage
