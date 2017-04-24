library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(skin = "green",
  dashboardHeader(title = "mpbBoreal"),
  dashboardSidebar(width = 300,
    sidebarMenu(id = "wholeThing",
      h4(HTML("&nbsp;"), "Maps"),
      menuItem("Change polygon layer", tabName = "Polygons", icon = icon("map-o")),
      menuItem("MPB red attack", tabName = "mpbMab", icon = icon("bug")),
      menuItem("Pine distribution", tabName = "pineMap", icon = icon("tree")),
      menuItem("Climate suitability", tabName = "climateMap", icon = icon("thermometer-3")),
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
      tabItem("mpbMap", print("NOT WORKING YET")),
      tabItem("pineMap", print("NOT WORKING YET")),
      tabItem("climateMap", print("NOT WORKING YET")),
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

