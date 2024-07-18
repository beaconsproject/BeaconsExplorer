bufferFeatUI2 <- function(id, mainTabId) {
  ns <- NS(id)
  fluidPage(
    checkboxInput(ns('viewpanel'), 'Add custom buffer using the table', value = FALSE),
    sliderInput(ns('buffer1'), label="Linear buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
    sliderInput(ns('buffer2'), label="Areal buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
    sliderInput(ns('area1'), label="Min intact patch size (km2):", min=0, max=2000, value = 0, step=50, ticks=FALSE),
    hr(),
    actionButton(ns("genIntactMap"), "Generate intactness map", style='color: #000')
  )
}


bufferFeatServer2 <- function(input, output, session, mainTabId, target, layers, insertedTabs, extraFeat) {
  #inserted <- reactiveVal(FALSE)
  ns <- session$ns
  
  observeEvent(input$genIntactMap, {
    aoi <- fda()
    if (input$custom_buffers==TRUE) {
      m1sub <- as_tibble(input$linear_buffers) %>% select(TYPE_DISTURBANCE, BUFFER_SIZE) %>%      
        mutate(BUFFER_SIZE=as.integer(BUFFER_SIZE))
      line <- left_join(line(), m1sub) %>% filter(!is.na(BUFFER_SIZE))
      v1 <- st_union(st_buffer(line, line$BUFFER_SIZE))
      m2sub <- as_tibble(input$areal_buffers) %>% select(TYPE_DISTURBANCE, BUFFER_SIZE) %>% 
        mutate(BUFFER_SIZE=as.integer(BUFFER_SIZE))
      poly <- left_join(poly(), m2sub) %>% filter(!is.na(BUFFER_SIZE))
      v2 <- st_union(st_buffer(poly, poly$BUFFER_SIZE))
    } else {
      v1 <- st_union(st_buffer(line(), input$buffer1))
      v2 <- st_union(st_buffer(poly(), input$buffer2))
    }
    v <- st_intersection(st_union(v1, v2), st_buffer(aoi, 100))
  })
  
}


bufferFeatUI <- function(id, mainTabId) {
  ns <- NS(id)
  fluidPage(
    checkboxInput(ns('viewpanel'), 'Add custom buffer using the table', value = FALSE),
    sliderInput(ns('buffer1'), label="Linear buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
    sliderInput(ns('buffer2'), label="Areal buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
    sliderInput(ns('area1'), label="Min intact patch size (km2):", min=0, max=2000, value = 0, step=50, ticks=FALSE),
    hr(),
    actionButton(ns("genIntactMap"), "Generate intactness map", style='color: #000')
  )
}


bufferFeatServer <- function(input, output, session, mainTabId, target, layers, insertedTabs, extraFeat, myMap) {
  #inserted <- reactiveVal(FALSE)
  ns <- session$ns
  check_tab_existence <- function(tab_id) {
    tab_id %in% insertedTabs()
  }
  
  renderLine <- eventReactive(!is.null(layers$line),{
    x <- layers$line %>%
      mutate(length_km=perim(layers$line)) %>%
      as.data.frame() %>%
      group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
      summarize(LENGTH_KM = round(sum(length_km),2)) %>%
      as_tibble()
    y <- left_join(as_tibble(m1), x) %>%
      relocate(TYPE_INDUSTRY, TYPE_DISTURBANCE, LENGTH_KM, BUFFER_SIZE) %>% 
      drop_na()
    y <-as.matrix(y)
  })
  
  renderPoly <- eventReactive(!is.null(layers$poly),{
    x <- layers$poly %>%
      mutate(area_km2=expanse(layers$poly, unit= "km")) %>%      
      as.data.frame() %>%
      group_by(TYPE_INDUSTRY, TYPE_DISTURBANCE) %>%
      summarize(AREA_KM2 = round(as.numeric(sum(area_km2)),2))  %>%
      as_tibble()
    y <- left_join(as_tibble(m1), x) %>%
      relocate(TYPE_INDUSTRY, TYPE_DISTURBANCE, AREA_KM2, BUFFER_SIZE) %>% 
      drop_na()
    y <-as.matrix(y)
  })
  
  #set min max outside of the observe scope to allow to update
  ######################## #
  ### TAB CONTROL     ####
  ######################## #
  m5 <- eventReactive(!is.null(extraFeat()),{
    outBuf <- tibble(TYPE_FEATURE=character(), 
                 BUFFER_SIZE=numeric(),
                 LENGTH_KM=numeric(),
                 AREA_KM2=numeric()
                )
    if(!is.null(extraFeat()$add_line())){
      #browser()
      x_line <- extraFeat()$add_line() %>%
        crop(layers$catch_3578) 
      # Calculate the length of each line
      lengths <- perim(x_line)/1000
      total_length <- sum(lengths)
        
      x <- tibble(TYPE_FEATURE=c("Uploaded lines"), 
                  BUFFER_SIZE= 500,
                  LENGTH_KM = total_length,
                  AREA_KM2=NA
                  )
      outBuf <- rbind(outBuf, x)
    }
    
    if(!is.null(extraFeat()$add_poly())){
      x_poly <- extraFeat()$add_poly() %>%
        crop(layers$catch_3578) 
        # Calculate the length of each line
      x_area <- expanse(x_poly, unit= "km")
      total_area <- sum(x_area)
        
      x <- tibble(TYPE_FEATURE=c("Uploaded polygons"), 
                  BUFFER_SIZE= 500,
                  LENGTH_KM = NA,
                  AREA_KM2=total_area
                  )
      outBuf <- rbind(outBuf, x)
    }
    return(outBuf)
  })
  
  ## do some environment hacking: Get the `session` variabe from the
  ## environment that invoked `callModule`.
  parentSession <- get("session", envir = parent.frame(2))
  observe({
    #if (!inserted()) {
    if (!check_tab_existence("buffer_tab")) {
        insertTab(inputId = mainTabId, tabPanel(
          "Custom buffers",
          id = "buffer_tab",
          tags$h4("Define linear buffer sizes:"),
          matrixInput("linear_buffers", value = renderLine(), rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE)),
          tags$h4("Define areal buffer sizes:"),
          matrixInput("areal_buffers", value = renderPoly(), rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE)),
          if (nrow(m5())>0) {
            m5 <- as.matrix(m5())
            matrixInput("extra_features", value = m5, rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE))
          },
        ),
        target = target,
        position = "after",
        select = FALSE,
        session = parentSession
        )
        print("Tab inserted") # Additional debugging
        #inserted(TRUE)
        # Update the list of inserted tabs
        insertedTabs(c(insertedTabs(), "buffer_tab"))
    }
  })
  
  observeEvent(input$viewpanel, {
    if (input$viewpanel) {
      disable("buffer1")
      disable("buffer2")
    } else {
      enable("buffer1")
      enable("buffer2")
    }
  })
  
  observeEvent(input$genIntactMap, {
    browser()
    #aoi <- fda()
    if (input$viewpanel==TRUE) {
      m1sub <- as_tibble(input$linear_buffers) %>% select(TYPE_DISTURBANCE, BUFFER_SIZE) %>%      
        mutate(BUFFER_SIZE=as.integer(BUFFER_SIZE))
      line <- left_join(line(), m1sub) %>% filter(!is.na(BUFFER_SIZE))
      v1 <- st_union(st_buffer(line, line$BUFFER_SIZE))
      m2sub <- as_tibble(input$areal_buffers) %>% select(TYPE_DISTURBANCE, BUFFER_SIZE) %>% 
        mutate(BUFFER_SIZE=as.integer(BUFFER_SIZE))
      poly <- left_join(poly(), m2sub) %>% filter(!is.na(BUFFER_SIZE))
      v2 <- st_union(st_buffer(poly, poly$BUFFER_SIZE))
    }else{
      v1 <- aggregate(buffer(layers$line, input$buffer1))
      v2 <- aggregate(buffer(layers$poly, input$buffer2))
      if(!is.null(extraFeat()$add_line())){
        v3 <- aggregate(buffer(extraFeat()$add_line(), input$buffer1))
      }else{
        v3 <- NULL
      }
      if(!is.null(extraFeat()$add_poly())){
        v4 <- aggregate(buffer(extraFeat()$add_poly(), input$buffer1))
      }else{
        v4 <- NULL
      }
    }
    # Save in list
    dist_ls <- list(v1, v2, v3, v4)
    # Filter out the NULL objects
    non_null_dist <- Filter(Negate(is.null), dist_ls)
    if (length(non_null_dist) > 0) {
      dist <- do.call(rbind, non_null_dist)
      layers$footprint <- aggregate(dist)
      x <- erase(aoi, layers$footprint)
      y <- mutate(x, area_km2=as.numeric(st_area(x)/1000000))
      intact <- filter(y, area_km2 > input$area1)
      layers$intactness <- intact
    }
    
    intactness <- layers$intactness %>% project("EPSG:4326")
    footprint <- layers$footprint %>% project("EPSG:4326")

    myMap %>%
      clearGroup("Areal disturbances") %>%
      clearGroup("Linear disturbances") %>%
      clearGroup("Intactness 2000") %>%
      clearGroup("Intactness 2020") %>%
      addPolygons(data=intactness, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness")%>%
      addPolygons(data=footprint, fill=T, stroke=F, fillColor='brown', fillOpacity=0.5, group="Footprint") %>%
      addLayersControl(position = "topright",
                       #baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Study area", "Fires","Intactness", "Footprint", "Protected areas"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Footprint", "Protected areas"))
    myMap
  })
 
}
