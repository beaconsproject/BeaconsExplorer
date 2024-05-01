# Define server logic
server <- function(input, output, session) {
  
  # Initialize map
  output$map <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -130, lat = 64, zoom = 5)%>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
      leafem::addMouseCoordinates()
  })
  
  # create map proxy to make further changes to existing map
  map <- leafletProxy("map")
  
  # change provider tile option
  observe({
    map %>% addProviderTiles(input$bmap)
  })
  
  gpkg <- callModule(geopackage, "geopackage_module")
  
  #### Map-related observers ####
  observe({ # raster layers
    map_bounds1 <- gpkg() %>% st_bbox() %>% as.character()
    map %>% 
      clearGroup("Study area") %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=gpkg(), weight=2, group="Study area")
  })
  
}
