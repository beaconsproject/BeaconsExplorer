############################# #
### Select Spatial layers  ####
############################# #
# Select Layer UI 
selLayerUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    useShinyjs(),
    tags$hr(),
    strong("2. Select optional spatial layers from panel on the right to include."),
    fluidRow(
      column(6,
             ("Projected"),
             div(style = "margin-top:-5px;",
                 checkboxInput(ns("prj1"), label = 'Quartz Claims', value = T)),
             div(style = "margin-top:-15px;",
                 checkboxInput(ns("prj2"), label = 'Placer Claims', value = F)),
             
      ),
      column(6,
             ("Miscellaneous"),
             div(style = "margin-top:-5px;",
                 checkboxInput(ns("spp1"), label = 'Caribou Herds', value = F)),
             div(style = "margin-top:-15px;",
                 checkboxInput(ns("spp2"), label = 'Thinhorn Sheep', value = F)),
             div(style = "margin-top:-15px;",
                 checkboxInput(ns("spp3"), label = 'Key Wetlands 2011', value = F))
      )
    ),
    strong("3. Select range of fires."),
    sliderInput(ns("minmax"), label="Range of fires to include:", min=1920, max=2020, value=c(1960, 2020)),
    tags$hr(),
    #div(style = "margin-top: -40px"),
    strong("4. Preview layers on the map"),
    actionButton(ns("previewLayers"), "Preview", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-primary", style="width:200px"),
    tags$hr(),
    strong("Layers previewed on the map will be saved and use in the disturbance analysis.")
  )
}



# Select Layer server module
selLayer <- function(input, output, session, myMap, layers, uploadedFeatures) {
  
  ns <- session$ns
  
  #set min max outside of the observe scope to allow to update
  observeEvent(input$minmax,{
    req(layers$fires)
    fires <- layers$fires %>%
        tidyterra::filter(YEAR >= input$minmax[1] & YEAR <= input$minmax[2]) %>% 
        project("EPSG:4326")
    
    sd_line <- layers$line %>% project("EPSG:4326")
    sd_poly <- layers$poly %>% project("EPSG:4326")
    ifl_2000 <- layers$ifl_2000 %>% project("EPSG:4326")
    ifl_2020 <- layers$ifl_2020 %>% project("EPSG:4326")
    pa_2021 <- layers$pa_2021 %>% project("EPSG:4326")
    myMap %>%
      clearGroup("Fires") %>%
      clearGroup("Protected areas") %>%
      addPolylines(data=sd_line, color='red', weight=2, group="Linear disturbances") %>%
      addPolygons(data=sd_poly, fill=T, stroke=F, fillColor='red', fillOpacity=0.5, group="Areal disturbances") %>%
      addPolygons(data=fires, fill=T, stroke=F, fillColor='orange', fillOpacity=0.5, group='Fires') %>%
      addPolygons(data=ifl_2000, fill=T, stroke=F, fillColor='#99CC99', fillOpacity=0.5, group="Intactness 2000") %>%
      addPolygons(data=ifl_2020, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness 2020")%>%
      addPolygons(data=pa_2021, fill=T, stroke=F, fillColor='brown', fillOpacity=0.5, group="Protected areas") %>%
      addLayersControl(position = "topright",
                       #baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Study area", "Linear disturbances", "Areal disturbances", "Fires","Intactness 2000", "Intactness 2020", "Protected areas"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Intactness 2000", "Intactness 2020", "Protected areas"))
    myMap
  })
  
  observeEvent(input$previewLayers,{
    req(uploadedFeatures)
    #browser()
    myMap %>%
      clearGroup("Quartz Claims") %>%
      clearGroup("Placer Claims") %>%
      clearGroup("Caribou Herds") %>%
      clearGroup("Thinhorn Sheep") %>%
      clearGroup("Key Wetlands 2011") 
    grps <- NULL 
    toShow <- NULL
    
    #Isolate allow to wait the trigger previewLayers to be pushed before looking into Optionals
    tprj1 <- isolate(input$prj1)
    tprj2 <- isolate(input$prj2)
    tspp1 <- isolate(input$spp1)
    tspp2 <- isolate(input$spp2)
    tspp3 <- isolate(input$spp3)
      
    if (tprj1 & length(layers$prj1)>0) { 
        prj1 <- layers$prj1 %>% project("EPSG:4326")
        myMap <- myMap %>% addPolygons(data=prj1, color='red', fill=T, weight=1, group="Quartz Claims")
        grps <- c(grps,"Quartz Claims")
    }
    if (tprj2 & length(layers$prj2)>0) {
        prj2 <- layers$prj2 %>% project("EPSG:4326")
        myMap <- myMap %>% addPolygons(data=prj2, color='red', fill=T, weight=1, group="Placer Claims")
        grps <- c(grps,"Placer Claims")
    }
    if (tspp1 & length(layers$spp1)>0) {
        spp1 <- layers$spp1 %>% project("EPSG:4326")
        myMap <- myMap %>% addPolygons(data=spp1, color='red', fill=T, weight=1, group="Caribou Herds")
        grps <- c(grps,"Caribou Herds")
    }
    if (tspp2 & length(layers$spp2)>0) {
        spp2 <- layers$spp2 %>% project("EPSG:4326")
        myMap <- myMap %>% addPolygons(data=spp2, color='red', fill=T, weight=1, group="Thinhorn Sheep")
        grps <- c(grps,"Thinhorn Sheep")
    }
    if (tspp3 & length(layers$spp3)>0) {
        spp3 <- layers$spp3 %>% project("EPSG:4326")
        myMap <- myMap %>% addPolygons(data=spp3, color='red', fill=T, weight=1, group="Key Wetlands 2011")
        grps <- c(grps,"Key Wetlands 2011")
     }
    if (!is.null(uploadedFeatures$add_poly())) {
        x_poly <- uploadedFeatures$add_poly() %>% project("EPSG:4326")
        myMap <- myMap %>% addPolygons(data=x_poly, color='#330333', fill=T, weight=1, group="Additonal areal features")
        toShow <- c(toShow,"Additonal areal features")
    }
    if (!is.null(uploadedFeatures$add_line())) {
        x_line <- uploadedFeatures$add_line() %>% project("EPSG:4326")
        myMap <- myMap %>% addPolylines(data=x_line, color='#330333', fill=F, weight=2, group="Additional linear features")
        toShow <- c(toShow,"Additional linear features")
    }
    myMap <- myMap %>% #addLayersControl(position = "topright",
        addLayersControl(position = "topright",
                         baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                         overlayGroups = c("Database limits","Study area", "Linear disturbances", "Areal disturbances", "Fires","Intactness 2000", "Intactness 2020", "Protected areas", toShow, grps),
                         options = layersControlOptions(collapsed = FALSE)) %>%
        hideGroup(c("Database limits", "Intactness 2000", "Intactness 2020", "Protected areas", grps))
    myMap
  })
  
}  



