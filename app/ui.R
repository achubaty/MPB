library(shiny)
library(shinydashboard)
library(shinyBS)

dashboardPage(skin = "green",
  dashboardHeader(title = "mpbBoreal"),
  dashboardSidebar(width = 300,
    sidebarMenu(id = "wholeThing",
      h4(HTML("&nbsp;"), "Maps"),
      menuItem("Change polygon layer", tabName = "Polygons", icon = icon("map-o")),
      menuItem("MPB red attack", tabName = "mpb_Map", icon = icon("bug")),
      menuItem("Pine distribution", tabName = "pine_Map", icon = icon("tree")),
      menuItem("Climate suitability", tabName = "climate_Map", icon = icon("thermometer-3")),
      br(),
      h4(HTML("&nbsp;"), "Figures"),
      menuItem("Simulation outputs", tabName = "simFigures", icon = icon("bar-chart")),
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
      tabItem("mpb_Map", initialMapUI("mpbMap")),
      tabItem("pine_Map", initialMapUI("pineMap")),
      tabItem("climate_Map", initialMapUI("climateMap")),
      tabItem("simFigures", simOutputsUI("simFigs")),

      tabItem("dataSources", dataInfoUI("modDataInfo")),
      tabItem("simDiagrams", simInfoUI("simInfoTabs")),
      tabItem("moduleInfo", moduleInfoUI("modInfoBoxes")),
      #tabItem("paramVals", moduleParamsUI("modParams")),
      tabItem("paramVals", p("NOT YET IMPLEMENTED")),

      ## do polygons last because it takes the longest
      tabItem("Polygons", fluidRow(
        tabBox(width = 12,
               tabPanel("Current Polygons", tabName = "Polygons1",
                        fluidRow(leafletMapUI("leafletMap")))
               )
        )
      )
    ),

    copyrightFooter(copyrightInfo)
  )
)
