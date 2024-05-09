# Define server logic
server <- function(input, output, session) {
  # Initialize reactive data
  init.values <- callModule(initializeValues, "init.module")
  
  # Initialize reactive value
  sa_sf <- reactiveVal(NULL) 
  up_module <- reactive({input$geoSel})
  
  # tab and module-level reactives
  module <- reactive({
    input$tabs
  })
  ######################## #
  ### GUIDANCE TEXT ####
  ######################## #
  
  # UI for component guidance text
  output$gtext_module <- renderUI({
    file <- file.path('Rmd', glue("gtext_{module()}.Rmd"))
    if (!file.exists(file)) return()
    includeMarkdown(file)
  })
  
  # Help Component
  help_modules <- c("data", "envs")
  lapply(help_modules, function(module) {
    btn_id <- paste0(module, "Help")
    observeEvent(input[[btn_id]], updateTabsetPanel(session, "main", "Module Guidance"))
  })
  
  
  # Initialize map
  output$myMap <- renderLeaflet({
    leaflet() %>% 
      setView(lng = -130, lat = 64, zoom = 5)%>%
      addProviderTiles("Esri.WorldImagery", group="Esri.WorldImagery") %>%
      addProviderTiles("Esri.WorldTopoMap", group="Esri.WorldTopoMap") %>%
      leafem::addMouseCoordinates()
  })
  # create map proxy to make further changes to existing map
  myMap <- leafletProxy("myMap")

    # change provider tile option
  observe({
    myMap %>% addProviderTiles(input$bmap)
  })
  
  # Provide GEO UI according to input format (gpkg, shp)
  observeEvent(input$geoSel, {
     if (up_module() == "gpkg") {
       output$upload_module <- renderUI({
         gpkgUI("upload_module")
        })
       sa_sf(callModule(gpkg_upload,"upload_module", parent = session))
    }else if (up_module() == "shp") {
      output$upload_module <- renderUI({
        shpUI("upload_module")
      })
      sa_sf(callModule(shp_upload, "upload_module", parent = session))
    }
  })

  #### Map-related observers ####
  observeEvent(input$conf_sa,{ # 
    sa_sf <- sa_sf()() 
    sa_sf <- sa_sf %>% project("EPSG:4326")
    map_bounds1 <- sa_sf %>% ext() %>% as.character()
    myMap %>% 
      clearGroup("Study area") %>%
      fitBounds(map_bounds1[1], map_bounds1[2], map_bounds1[3], map_bounds1[4]) %>%
      addPolygons(data=sa_sf, weight=2, group="Study area")
    #output$editsa_module <- renderUI({
    #  editsaUI("editsa_module")
    #})
    shinyjs::toggle("enable_edit")
    shinyjs::toggle("confirm_edit")
    observeEvent(input$enable_edit, {
      edit_sa(session)
  })
  
  })
}
