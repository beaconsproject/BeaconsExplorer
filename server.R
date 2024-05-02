# Define server logic
server <- function(input, output, session) {
  # Initialize reactive values
  sa_sf <- reactiveVal(NULL) # Initialize reactiveVal
  up_module <- reactive({input$geoSel})
  
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
  
  # Serve GEO UI according to input format (gpkg, shp)
  observeEvent(input$geoSel, {
     if (up_module() == "gpkg") {
       output$up_module <- renderUI({
         gpkgUI("upload_module")
        })
       sa_sf(callModule(gpkg_upload,"upload_module"))
      
    }else if (up_module() == "shp") {
      output$up_module <- renderUI({
        shpUI("upload_module")
      })
      sa_sf(callModule(shp_upload, "upload_module"))
    }
  })

  #### Map-related observers ####
  observeEvent(input$conf_sa,{ # 
    #browser()
    sa_sf <- sa_sf()() 
    sa_sf <- sa_sf %>% st_transform(4326)
    map_bounds1 <- sa_sf %>% st_bbox() %>% as.character()
    map %>% 
      clearGroup("Study area") %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=sa_sf, weight=2, group="Study area")
  })
  
}
