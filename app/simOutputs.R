### simulation outputs (graphs and figures) ------------------------------------
simOutputsUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    tabBox(width = 12,
      tabPanel("Growth curve", plotOutput(ns("growthCurve")))
    )
  )
}

simOutputs <- function(input, output, session, sim) {
  output$growthCurve <- renderPlot({
    plot(sim$mpbRedTopGrowthPlotGG)
  })
}

### initial map ----------------------------------------------------------------
initialMapUI <- function(id) {
  ns <- NS(id)

  fluidRow(
    plotOutput(ns("map_init"))
  )
}

initialMap <- function(input, output, session, sim, mapID) {
  output$map_init <- renderPlot({
    switch(
      mapID,
      "massAttacksMap" = {
        map <- sim[[mapID]][[paste0("X", start(sim))]]
        setColors(map) <- brewer.pal(9, "YlOrRd")
      },
      "pineMap" = {
        map <- sim[[mapID]][[1]]
        setColors(map) <- brewer.pal(9, "Greens")
      },
      "climateSuitabilityMap" = {
        map <- sim[[mapID]]
        setColors(map) <- rev(brewer.pal(9, "RdBu"))
      }
    )

    clearPlot()
    Plot(map)
  })
}
