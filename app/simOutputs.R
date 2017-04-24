### simulation outputs (graphs and figures) ------------------------------------
simOutputsUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("simulationOutputs"))
}

simOutputs <- function(input, output, session, sim) {
  output$simulationOutputs <- renderUI({
    fluidRow(
      tabBox(width = 12,
             tabPanel("Growth curve",
                      plot(sim$mpbRedTopGrowthPlotGG)
                      )
      )
    )
  })
}

### initial map ----------------------------------------------------------------
initialMapUI <- function(id) {
  ns <- NS(id)

  uiOutput(ns("map_init"))
}

initialMap <- function(input, output, session, sim, mapID) {
  output$map_init <- renderUI({
    fluidRow(
      h4("Initial MPB outbreak map"),
      Plot(sim[[mapID]])
    )
  })
}
