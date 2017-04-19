library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(skin = "green",
  dashboardHeader(title = "mpbBoreal"),
  dashboardSidebar(width = 300,
    sidebarMenu(id = "wholeThing",
      menuItem("Maps", icon = icon("map-o"),
               menuSubItem("Change polygon layer", tabName = "Polygons", icon = icon("map-marker"))
      ),
      br(),
      h4(HTML("&nbsp;"), "Model details"),
      menuItem("Overview Diagrams", tabName = "simDiagrams", icon = icon("sitemap")),
      menuItem("Module Info", tabName = "moduleInfo", icon = icon("puzzle-piece")),
      br(),
      sidebarFooter() ## CSS rules push the footer to the bottom of the sidebar
    )
  ),
  dashboardBody(
    includeCSS("www/style.css"),

    tabItems(
      tabItem("StudyRegion",
              fluidRow(
                box(
                  width = 12,
                  height = 900,
                  solidHeader = TRUE,
                  status = "success",
                  title = "Fire Return Interval, with sub region currently in Demo (hover mouse over for values)",
                  ggvisOutput("StudyRegion")
                )
              )
      ),
      tabItem("simDiagrams", simInfoUI("simInfoTabs")),
      tabItem("moduleInfo", moduleInfoUI("modInfoBoxes")),
      tabItem("Polygons",
              fluidRow(
                tabBox(width = 12,
                       tabPanel("Current Polygons", tabName = "Polygons1",
                                fluidRow(leafletMapUI("leafletMap"))
                       )
                )
              )
      )
    ),
    copyrightFooter()
  )
)

