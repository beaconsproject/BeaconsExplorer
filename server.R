# Define server logic
server <- function(input, output, session) {
  # Initialize reactive value
  sa_spat <- reactiveVal(NULL) 
  sa_spat_select <-  reactiveVal(NULL)
  up_module <- reactive({input$geoSel})
  selected_catchments <- reactiveValues(catchnum = c()) #track selected catchment interactively
  layers <- callModule(reactiveLayersModule, id = "reactiveLayersModule", region = NULL)
  render_layers_panel <- reactiveVal(FALSE)
  uploadedFeat <- reactiveVal(NULL)
  
  pop = ~paste("CATCHNUM:", CATCHNUM, "<br>Area (km²):", round(Area_total/1000000,1), "<br>Intactness (%):", intact*100 )
  

  # tab and module-level reactives
  module <- reactive({
    input$tabs
  })
  
  insertedTabs <- reactiveVal(c())
  
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
  help_modules <- c("data", "dist")
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
    catch_4326 <- layers$catch_4326
    data_select <- catch_4326[catch_4326$CATCHNUM %in% selected_catchments$catchnum,]
    myMap %>%
      clearGroup("Selected") %>%
      addPolygons(data = data_select,
                  color = 'black', weight = 1, fillColor = "grey", fillOpacity = 0.7,
                  layerId = data_select$CATCHNUM, group = "Selected") 
  })
  
  output$radio_buttons_ui <- renderUI({
    radioButtons("geoSel", "Choose File Type:", choices = c("shp", "gpkg"), inline = TRUE)
  })
  
  # Provide GEO UI according to input format (gpkg, shp)
  #observeEvent(input$geoSel, {
  observe({
    req(input$geoSel)
     if (up_module() == "gpkg") {
       output$upload_module <- renderUI({
         gpkgUI("upload_module")
        })
       sa_spat(callModule(gpkg_upload,"upload_module"))
    }else if (up_module() == "shp") {
      output$upload_module <- renderUI({
        shpUI("upload_module")
      })
      sa_spat(callModule(shp_upload, "upload_module"))
    }
  })


  #### Map-related observers ####
  observe({
    req(!is.null(sa_spat()))
    req(!is.null(sa_spat()()))
    sa_spat3578 <- sa_spat()() 
    sa_spat <- sa_spat3578 %>% project("EPSG:4326")
    pas_4326 <- layers$pa_2021 %>% project("EPSG:4326")
    caribourange_4326 <- layers$spp1 %>% project("EPSG:4326")
    map_bounds1 <- sa_spat %>% ext() #%>% as.character()
    myMap <- leafletProxy("myMap", session)
    myMap %>% 
      clearGroup("Study area") %>%
      fitBounds(as.character(map_bounds1[1]), as.character(map_bounds1[3]), as.character(map_bounds1[2]), as.character(map_bounds1[4])) %>%
      addPolygons(data=sa_spat, weight=2, group="Study area", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=ecoregions(), color='#996633', fill=F, weight=1, group="Ecoregions", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=fdas(), color='#333333', fill=F, weight=1, group="FDAs", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=caribourange_4326, fillColor = "Brown", fillOpacity=0.7, weight=0.1, group="Caribou ranges", options = leafletOptions(pane = "ground")) %>%
      addPolygons(data=pas_4326, fillColor = "green",  fillOpacity=0.5, weight=0.1, group="Protected areas", options = leafletOptions(pane = "ground")) %>%
      addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Study area","Ecoregions", "FDAs","Protected areas", "Caribou ranges"),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Ecoregions","FDAs","Protected areas", "Caribou ranges"))
    
    output$editSA_module <- renderUI({
      editSAUI("editSA_module")
    })
    sa_spat_select(sa_spat3578)
  })
  
  # Update map when mod_sa_button is pressed
  observeEvent(input[["editSA_module-mod_sa_button"]], {
    # If modify sa is pressed, select all catchnum based on centroid
    sa_catch<-sa.catch(session, sa_spat()(), layers$catch_3578)
    selected_catchments$catchnum <- sa_catch$CATCHNUM
    edit.SA(sa_spat()(), layers$catch_4326, myMap)
    if(input[["editSA_module-mod_sa_button"]][1] ==1){
      myMap %>%
        addControl(actionButton(inputId = "clear_button", label = "Clear selection"), position="topleft", className = "class_clear_button")
    }
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
  observeEvent(input[["editSA_module-view_sa_button"]], {
    update.SA(layers$catch_4326, selected_catchments, myMap)
  })
  
  # Confirm map when conf_sa_button is pressed
  observeEvent(input[["editSA_module-conf_sa_button"]], {
    callModule(conf.SA, id = "confSA", sa_spat()(), layers$catch_4326, selected_catchments, myMap)
    selected_catchments$catchnum <- NULL
    showModal(modalDialog(
      title = "Study area confirmed. Please select spatial layers",
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    removeUI("#radio_buttons_ui", immediate=TRUE)
    removeUI("#upload_module", immediate=TRUE)
    removeUI("#editSA_module", immediate=TRUE)
    render_layers_panel(TRUE)
  })
  
  # Conditionally render selLayerUI only if render_layers_panel is TRUE
  observeEvent(render_layers_panel(), {
    if (render_layers_panel()) {
      output$selLayer_module <- renderUI({
        selLayerUI("selLayer_module")
      })
    layers <- callModule(reactiveLayersModule, id = "reactiveLayersModule", region = sa_spat_select())
    shinyjs::show("modalButtonContainer")
    uploadedFeatures <- callModule(modalDialogServer, "modal")
    callModule(selLayer, "selLayer_module", myMap, layers, uploadedFeatures)
    uploadedFeat(uploadedFeatures)
    output$switch_component <- renderUI({
      switchTabButtonUI("switch_component", "disturbance analysis")
    })
    switchTabButtonServer("switch_component", session, "dist")
    }
  })
  
  observe({
    if(input$tabs == 'dist'){
      output$buffer_module <- renderUI({
        bufferFeatUI("buffer_module")# Pass checkbox state as argument
      })
      callModule(bufferFeatServer, "buffer_module", "main", "Module Guidance", layers, insertedTabs, uploadedFeat, myMap)
    }
    
  }) 
  
  observeEvent(input$`buffer_module-viewpanel`, {
    if (isTRUE(input$`buffer_module-viewpanel`)) {
      updateTabsetPanel(session, "main", selected = "Custom buffers")
    } else {
      updateTabsetPanel(session, "main", selected = "Map")
    }
  })
  
  # If clear selection button is pressed, remove all catchnums from list
  observeEvent(input$clear_button, {
    selected_catchments$catchnum <- c()
  })
  

}
  

