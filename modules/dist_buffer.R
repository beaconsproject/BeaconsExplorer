bufferFeatUI <- function(id, mainTabId) {
  ns <- NS(id)
  fluidPage(
    checkboxInput(ns('viewpanel'), 'Add custom buffer using the table', value = FALSE),
    sliderInput(ns('buffer1'), label="Linear buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
    sliderInput(ns('buffer2'), label="Areal buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
    sliderInput(ns('area1'), label="Min intact patch size (km2):", min=0, max=2000, value = 0, step=50, ticks=FALSE),
    hr(),
    actionButton(ns("genIntactMap"), "Generate intactness map", icon = icon(name = "map-location-dot", lib = "font-awesome"), class = "btn-primary", style="width:250px")
  )
}

bufferFeatServer <- function(input, output, session, mainTabId, target, layers, insertedTabs, extraFeat, region, myMap) {
  #inserted <- reactiveVal(FALSE)
  ns <- session$ns
  check_tab_existence <- function(tab_id) {
    tab_id %in% insertedTabs()
  }
  # Reactive value to store and updated buffer tab
  bufSizes <- reactiveValues(bufLine = reactiveVal(NULL),
                             bufPoly = reactiveVal(NULL),
                             bufextraLine = reactiveVal(NULL),
                             bufextraPoly = reactiveVal(NULL)
  )

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
    bufSizes$bufLine <- y
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
    bufSizes$bufPoly <- y
  })
  
  extraLine <- eventReactive(!is.null(extraFeat()$add_line()),{
    if(!is.null(extraFeat()$add_line())){
      outBuf_line <- tibble(TYPE_FEATURE=character(), 
                            TYPE_DISTURBANCE=character(), 
                            LENGTH_KM=numeric(),
                            BUFFER_SIZE=numeric()
      )
      x_line <- extraFeat()$add_line() %>%
        crop(region)
      # Calculate the length of each line
     x_line$lengths <- perim(x_line)/1000
      
      # Convert SpatVect  and Summarize the total length per Name_EN category
      col_line <-extraFeat()$col_line
      x_line[[col_line]] <- ifelse(is.na(x_line[[col_line]]), "UNKNOWN", x_line[[col_line]])
      x_line_df <- x_line %>%
        as.data.frame() %>%
        group_by(!!sym(col_line)) %>%
        summarize(Total_Length = sum(lengths, na.rm = TRUE))
      
      # Replace NA with "UNKNOWN" in the summary data frame
      x_line_df[[col_line]] <- if_else(is.na(x_line_df[[col_line]]), "UNKNOWN", x_line_df[[col_line]])
      x <- tibble(TYPE_FEATURE=c("Uploaded lines"), 
                TYPE_DISTURBANCE=x_line_df[[col_line]],
                LENGTH_KM = x_line_df$Total_Length,
                BUFFER_SIZE= 500,
      )
      outBuf_line <- rbind(outBuf_line, x)
      bufSizes$bufextraLine <- as.matrix(outBuf_line)
      return(as.matrix(outBuf_line))
    }else{
      return(NULL)
    }
  })

  extraPoly <- eventReactive(!is.null(extraFeat()$add_poly()),{
    if(!is.null(extraFeat()$add_poly())){
      outBuf <- tibble(TYPE_FEATURE=character(), 
                       TYPE_DISTURBANCE=character(), 
                       AREA_KM2=numeric(),
                       BUFFER_SIZE=numeric()
      )
      x_poly <- extraFeat()$add_poly() %>%
        crop(region)
      # Calculate area
      x_poly$area_km <- expanse(x_poly, unit= "km")
      col_poly <-extraFeat()$col_poly
      
      x_poly_df <- x_poly %>%
        as.data.frame() %>%
        group_by(!!sym(col_poly)) %>%
        summarize(Total_area = sum(area_km, na.rm = TRUE))
      
      # Replace NA with "UNKNOWN" in the summary data frame
      x_poly_df[[col_poly]] <- if_else(is.na(x_poly_df[[col_poly]]), "UNKNOWN", x_poly_df[[col_poly]])
      x <- tibble(TYPE_FEATURE=c("Uploaded polygons"), 
                  TYPE_DISTURBANCE=x_poly_df[[col_poly]],
                  AREA_KM2=x_poly_df$Total_area,
                  BUFFER_SIZE= 500
      )
      outBuf_poly <- rbind(outBuf, x)
      bufSizes$bufextraPoly <- as.matrix(outBuf_poly)
      return(as.matrix(outBuf_poly))
    }else{
      return(NULL)
    }
  })  
  
  ## do some environment hacking: Get the `session` variabe from the
  ## environment that invoked `callModule`.
  parentSession <- get("session", envir = parent.frame(2))
  observe({
    if (!check_tab_existence("buffer_tab")) {
      insertTab(inputId = mainTabId, tabPanel(
        "Custom buffers",
        id = "buffer_tab",
        tags$h4("Define linear buffer sizes:"),
        matrixInput(ns("linear_buffers"), value = renderLine(), rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE)),
        tags$h4("Define areal buffer sizes:"),
        matrixInput(ns("areal_buffers"), value = renderPoly(), rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE)),
        if (!is.null(extraLine())) {
          list(
            tags$h4("Define linear buffer sizes on uploaded lines:"),
            matrixInput(ns("extra_features_line"), value = extraLine(), rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE))
          )
        },
        if (!is.null(extraPoly())) {
          list(
            tags$h4("Define areal buffer sizes on uploaded polygons:"),
            matrixInput(ns("extra_features_poly"), value = extraPoly(), rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE))
          )
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
  
  # Control visibility of the sliders
  observeEvent(input$viewpanel, {
    if (input$viewpanel) {
      disable("buffer1")
      disable("buffer2")
    } else {
      enable("buffer1")
      enable("buffer2")
    }
  })
  
  # Update buffers tab for line 
  observeEvent(input$linear_buffers,{
    buffers <- as_tibble(input$linear_buffers)
    buffers <- buffers %>% mutate(BUFFER_SIZE = as.integer(BUFFER_SIZE))
    bufSizes$bufLine <-buffers
  })
  # Update buffers tab for poly
  observeEvent(input$areal_buffers,{
    buffers <- as_tibble(input$areal_buffers)
    buffers <- buffers %>% mutate(BUFFER_SIZE = as.integer(BUFFER_SIZE))
    bufSizes$bufPoly <-buffers
  })  
  # Update buffers tab for extraline 
  observeEvent(input$extra_features_line,{
    buffers <- as_tibble(input$extra_features_line)
    buffers <- buffers %>% mutate(BUFFER_SIZE = as.integer(BUFFER_SIZE))
    bufSizes$bufextraLine <-buffers
  })
  # Update buffers tab for extraPoly
  observeEvent(input$extra_features_poly,{
    buffers <- as_tibble(input$extra_features_poly)
    buffers <- buffers %>% mutate(BUFFER_SIZE = as.integer(BUFFER_SIZE))
    bufSizes$bufextraPoly <-buffers
  })
  
  observeEvent(input$genIntactMap, {
    if (input$viewpanel==TRUE) {
      if(!is.null(layers$line)){
        line <- left_join(layers$line, bufSizes$bufLine) %>% filter(!is.na(BUFFER_SIZE))
        distLine <- aggregate(buffer(line, line$BUFFER_SIZE))
      }else{distLine<-NULL}
      if(!is.null(layers$poly)){
        poly <- left_join(layers$poly, bufSizes$bufPoly) %>% filter(!is.na(BUFFER_SIZE))
        distPoly <- aggregate(buffer(poly, poly$BUFFER_SIZE))
      }else{distPoly<-NULL}
      if(!is.null(extraFeat()$add_line())){
        linename_x <- extraFeat()$col_line
        extraLine <- extraFeat()$add_line()
        extraLine[[linename_x]] <- replace(extraLine[[linename_x]], is.na(extraLine[[linename_x]]), "UNKNOWN")
        linename_y <- "TYPE_DISTURBANCE"
        extraline <- left_join(extraLine, bufSizes$bufextraLine, by = setNames(linename_y, linename_x)) %>% filter(!is.na(BUFFER_SIZE))
        distextraLine <- aggregate(buffer(extraline, extraline$BUFFER_SIZE))
      }else{distextraLine<-NULL}
      if(!is.null(extraFeat()$add_poly())){
        polyname_x <- extraFeat()$col_poly
        extraPoly <- extraFeat()$add_poly()
        extraPoly[[polyname_x]] <- replace(extraPoly[[polyname_x]], is.na(extraPoly[[polyname_x]]), "UNKNOWN")
        polyname_y <- "TYPE_DISTURBANCE"
        extrapoly <- left_join(extraPoly, bufSizes$bufextraPoly, by = setNames(polyname_y, polyname_x)) %>% filter(!is.na(BUFFER_SIZE))
        distextraPoly <- aggregate(buffer(extrapoly, extrapoly$BUFFER_SIZE))
      }else{distextraPoly <-NULL}
    }else{
      if(!is.null(layers$line)){
        distLine <- aggregate(buffer(layers$line, input$buffer1))
      }else{distLine <-NULL}
      if(!is.null(layers$poly)){
        distPoly <- aggregate(buffer(layers$poly, input$buffer2))
      }else{distPoly<-NULL}
      if(!is.null(extraFeat()$add_line())){
        distextraLine <- aggregate(buffer(extraFeat()$add_line(), input$buffer1))
      }else{distextraLine<-NULL}
      if(!is.null(extraFeat()$add_poly())){
        distextraPoly <- aggregate(buffer(extraFeat()$add_poly(), input$buffer2))
      }else{distextraPoly<-NULL}
    }
    # Save in list
    dist_ls <- list(distLine, distPoly, distextraLine, distextraPoly)
    # Filter out the NULL objects
    non_null_dist <- Filter(Negate(is.null), dist_ls)
    if (length(non_null_dist) > 0) {
      dist <- do.call(rbind, non_null_dist)
      layers$footprint <- aggregate(dist)
      x <- erase(region, layers$footprint) %>%
        disagg()
      y <-mutate(x, area_km2=expanse(x, unit= "km"))
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
      clearGroup("Intactness") %>%
      clearGroup("Footprint") %>%
      addPolygons(data=intactness, fill=T, stroke=F, fillColor='#669966', fillOpacity=0.5, group="Intactness")%>%
      addPolygons(data=footprint, fill=T, stroke=F, fillColor='brown', fillOpacity=0.5, group="Footprint") %>%
      addLayersControl(position = "topright",
                       #baseGroups=c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                       overlayGroups = c("Study area", "Fires","Intactness", "Footprint", "Protected areas"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup(c("Footprint", "Protected areas"))
    myMap
  })
  return(layers)
}


