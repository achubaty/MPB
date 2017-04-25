## leaflet map with polygon overlays -------------------------------------------
leafletMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12,
        solidHeader = TRUE, collapsible = TRUE,
        title = "Area covered by this demo (outlined in red), within the full study area (blue)",
        leaflet::leafletOutput(ns("mapLFLT"), height = 600),
        selectInput(ns("leafletMapPolygons"), "Other layers to show summaries with",
                    choices = names(polygons[1:4]),
                    selected = names(polygons[1:4])[[4]]) ## default: ecodistricts
    )
  )
}

leafletMap <- function(input, output, session) {
  polygonInput <- reactive({
    switch(input$leafletMapPolygons,
           "Ecozones" = 1,
           "Ecoprovinces" = 2,
           "Ecoregions" = 3,
           "Ecodistricts" = 4)
  })

  output$mapLFLT <- renderLeaflet({
    withProgress(
      message = 'Calculation in progress',
      detail = 'This may take a while...', value = 0, {
        polyNum <- polygonInput()
        polyLFLT <- polygons[[polyNum + length(polygons)/2]] # leaflet projection
        a <- leaflet() %>%
          addTiles(group = "OSM (default)") %>%
          addPolygons(data = polyLFLT, color = "blue", group = "full",
                      fillOpacity = 0.2, weight = 1,
                      popup = paste(polyLFLT[[polygonIndivIdsColum[[polyNum]]]]),
                      highlight = highlightOptions(
                        weight = 5,
                        color = "red",
                        fill = TRUE,
                        fillColor = "blue",
                        fillOpacity = 0.7,
                        bringToFront = TRUE
                      )) %>%
          addPolygons(data = demoArea, color = "red", fillColor = "blue",
                      group = "demo", fillOpacity = 0.2, weight = 1) %>%
          setView(mean(c(xmin(polyLFLT), xmax(polyLFLT))),
                  mean(c(ymin(polyLFLT), ymax(polyLFLT))), zoom = 5)
        setProgress(1)
    })
    a
  })

  selected <- reactiveVal()

  observeEvent(input$mapLFLT_shape_click, {
    click <- input$mapLFLT_shape_click

    if (is.null(click)) return()

    # puts lat and lon for click point into its own data frame
    coords <- as.data.frame(cbind(click$lng, click$lat))

    # converts click point coordinate data frame into SP object, sets CRS
    point <- SpatialPoints(coords)
    proj4string(point) <- crs.lflt

    # retrieves polygon in which the click point resides, set CRS
    selected <- polygons[[polygonInput() + length(polygons)/2]][point,]
    proj4string(selected) <- crs.lflt

    proxy <- leafletProxy("mapLFLT")
    if (is.null(click$id)) {
      proxy %>% addPolygons(data = selected,
                            fillColor = "blue",
                            fillOpacity = 0.7,
                            color = "red",
                            weight = 5,
                            stroke = TRUE,
                            layerId = "Selected")
    } else  {
      if (click$id == "Selected") {
        proxy %>% removeShape(layerId = "Selected")
      } else {
        proxy %>% addPolygons(data = selected,
                              fillColor = "blue",
                              fillOpacity = 0.7,
                              color = "red",
                              weight = 5,
                              stroke = TRUE,
                              layerId = "Selected") ## duplicated above
      }
    }
  })

  return(selected)
}
