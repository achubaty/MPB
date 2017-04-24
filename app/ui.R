library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(skin = "green",
  dashboardHeader(title = "mpbBoreal"),
  dashboardSidebar(width = 300,
    sidebarMenu(id = "wholeThing",
      h4(HTML("&nbsp;"), "Maps"),
      menuItem("Change polygon layer", tabName = "Polygons", icon = icon("map-o")),
      br(),
      h4(HTML("&nbsp;"), "Model details"),
      menuItem("Data Sources", tabName = "dataSources", icon = icon("database")),
      menuItem("Model Overview", tabName = "simDiagrams", icon = icon("sitemap")),
      menuItem("Module Info", tabName = "moduleInfo", icon = icon("puzzle-piece")),
      menuItem("Parameter Values", tabName = "paramVals", icon = icon("wrench")),
      br(),
      sidebarFooter() ## CSS rules push the footer to the bottom of the sidebar
    )
  ),
  dashboardBody(
    includeCSS("www/style.css"),

    tabItems(
      tabItem(
        "dataSources",
        p("NOT YET IMPLEMENTED")
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
      ),
      #tabItem("paramVals", moduleParamsUI("modParams"))
      tabItem("paramVals", p("NOT YET IMPLEMENTED"))
    ),
    copyrightFooter()
  )
)

