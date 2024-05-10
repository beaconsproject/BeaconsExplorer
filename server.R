# Define server logic
server <- function(input, output, session) {
  # Initialize reactive data
  #init.values <- callModule(initializeValues, "init.module")

  # Initialize reactive value
  sa_spat <- reactiveVal(NULL) 
  up_module <- reactive({input$geoSel})
  selected_catchments <- reactiveValues(catchnum = c()) #track selected catchment interactively
  
  pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_total/1000000,1), "<br>Intactness (%):", intact*100 )
  
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
  
  ######################## #
  ### MAPPING LOGIC ####
  ######################## #
  # Initialize map
  output$myMap <- renderLeaflet({
    leaflet() %>% 
      addMapPane(name = "ground", zIndex=380) %>%
      addMapPane(name = "overlay", zIndex=420) %>%
      setView(lng = -130, lat = 64, zoom = 5)%>%
      addProviderTiles("Esri.NatGeoWorldMap", group="Esri.NatGeoWorldMap") %>%
      leafem::addMouseCoordinates()
  })
  # create map proxy to make further changes to existing map
  myMap <- leafletProxy("myMap", session)

  # change provider tile option
  observe({
    myMap %>% addProviderTiles(input$bmap)
  })
  
  # Render selected catchments. None at startup
  observe({
    data_select <- catch_4326[catch_4326$CATCHNUM %in% selected_catchments$catchnum,]
    myMap %>%
      clearGroup("Selected") %>%
      addPolygons(data = data_select,
                  color = 'black', weight = 1, fillColor = "grey", fillOpacity = 0.7,
                  layerId = data_select$CATCHNUM, group = "Selected") 
  })
  
  # Provide GEO UI according to input format (gpkg, shp)
  observeEvent(input$geoSel, {
     if (up_module() == "gpkg") {
       output$upload_module <- renderUI({
         gpkgUI("upload_module")
        })
       sa_spat(callModule(gpkg_upload,"upload_module", parent = session))
    }else if (up_module() == "shp") {
      output$upload_module <- renderUI({
        shpUI("upload_module")
      })
      sa_spat(callModule(shp_upload, "upload_module", parent = session))
    }
  })

  #### Map-related observers ####
  observe({
    req(!is.null(sa_spat()))
    req(!is.null(sa_spat()()))
    sa_spat3578 <- sa_spat()() 
    sa_spat <- sa_spat3578 %>% project("EPSG:4326")
    map_bounds1 <- sa_spat %>% ext() #%>% as.character()
    myMap <- leafletProxy("myMap", session)
    myMap %>% 
      clearGroup("Study area") %>%
      fitBounds(as.character(map_bounds1[1]), as.character(map_bounds1[3]), as.character(map_bounds1[2]), as.character(map_bounds1[4])) %>%
      addPolygons(data=sa_spat, weight=2, group="Study area", options = leafletOptions(pane = "ground"))
    shinyjs::show("mod_sa_button")
    shinyjs::show("view_sa_button")
    shinyjs::show("conf_sa_button")
  })
  
  # Update map when mod_sa_button is pressed
  observeEvent(input$mod_sa_button, {
    # If modify sa is pressed, select all catchnum based on centroid
    sa_catch<-sa.catch(session, sa_spat()(), catch_3578())
    if(input$mod_sa_button[1] ==1){
      selected_catchments$catchnum <- sa_catch$CATCHNUM
    }else{
      selected_catchments$catchnum <- selected_catchments$catchnum
    }
    edit.SA(sa_spat()(), sa_catch, catch_4326, selected_catchments, myMap)
  })
  
  # Track a list of which catchnums have been selected
  observeEvent(input$myMap_shape_click, {
    clickId <- input$myMap_shape_click$id # id is the layerId assigned to the polygon layer in Leaflet
    if (clickId %in% selected_catchments$catchnum) {
      selected_catchments$catchnum <- selected_catchments$catchnum[!selected_catchments$catchnum %in% clickId]
    } else {
      selected_catchments$catchnum <- c(selected_catchments$catchnum, clickId)
    }
  })
  
  # Update map when view_sa_button is pressed
  observeEvent(input$view_sa_button, {
    update.SA(catch_3578, catch_4326, selected_catchments, myMap)
  })
  
  # Confirm map when conf_sa_button is pressed
  observeEvent(input$conf_sa_button, {
    conf.SA(region, myMap)
  })
  
  # If clear selection button is pressed, remove all catchnums from list
  observeEvent(input$clear_button, {
    selected_catchments$catchnum <- c()
  })
  

}
  

