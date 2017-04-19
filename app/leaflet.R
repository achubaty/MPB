## leaflet map with ploygon overlays -------------------------------------------
leafletMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        solidHeader = TRUE, collapsible = TRUE,
        title = "Area covered by this demo (in red), within the LandWeb study area (blue)",
        leaflet::leafletOutput(ns("leafletMap1"), height = 600),
        selectInput(ns("leafletMapPolygons"), "Other layers to show summaries with",
                    choices = names(polygons[1:(length(polygons)/4) + (length(polygons)/4)*3]),
                    selected = names(polygons[1:(length(polygons)/4) + (length(polygons)/4)*3])[[1]])
    )
  )
}

leafletMap <- function(input, output, session) {
  output$leafletMap1 <- renderLeaflet({
    withProgress(message = 'Calculation in progress',
                 detail = 'This may take a while...', value = 0, {
                   polyNum <- polygonInput()
                   polyDemo <- polygons[[polyNum + (length(polygons)/4)*3]] # leaflet projection, DEMO scale
                   polyFull <- polygons[[polyNum + (length(polygons)/4)*2]] # leaflet projection, Full scale
                   a <- leaflet() %>% addTiles(group = "OSM (default)") %>%
                     addPolygons(data = polyFull, color = "blue", group = "Full",
                                 fillOpacity = 0.2, weight = 1,
                                 popup = paste(polyFull[[polygonIndivIdsColum[[polyNum]]]])) %>%
                     addPolygons(data = polyDemo, color = "red", group = "Demo",
                                 fillOpacity = 0.6, weight = 3,
                                 popup = paste(polyDemo[[polygonIndivIdsColum[[polyNum]]]]))  %>%
                     setView(mean(c(xmin(polyDemo),xmax(polyDemo))),
                             mean(c(ymin(polyDemo),ymax(polyDemo))),
                             zoom = 5)
                   setProgress(1)
                 })

    a
  })

  polygonInput <- reactive({
    switch(input$leafletMapPolygons,
           "Ecodistricts Demo" = 1)
  })

  return(polygonInput)
}
