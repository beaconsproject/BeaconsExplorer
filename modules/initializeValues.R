initializeValues <- function(input, output, session) {
  # set a reactive value for selected catchments
  selected_catchments <- reactiveValues( # this is the list of currently selected catchments
    catchnum = c()
  )
  
  # Section 1 in csr: 3578
  catch_3578 <-reactive({
    st_read(catch3578, 'catchments')
  })
  
  line <- reactive({
    st_read(bp, 'sd_line') %>%
      st_intersection(region()) %>%
      st_cast('MULTILINESTRING')
  })
  
  poly <- reactive({
    vect(bp, 'sd_poly') %>%
      st_as_sf() %>%
      st_cast('MULTIPOLYGON') %>%
      st_intersection(region()) %>%
      st_cast('MULTIPOLYGON')
  })
  
  fires <- reactive({
    req(bnd())
    vect(bp, 'fires') %>%
      st_as_sf() %>%
      st_cast('MULTIPOLYGON') %>%
      filter(YEAR >= input$minmax[1] & YEAR <= input$minmax[2]) %>%
      st_intersection(region()) %>%
      st_cast('MULTIPOLYGON')
  })
  
  ifl_2000 <- reactive({
    st_read(bp, 'ifl_2000') %>%
      st_intersection(region())
  })
  
  ifl_2020 <- reactive({
    st_read(bp, 'ifl_2020') %>%
      st_intersection(region())
  })
  
  pa_2021 <- reactive({
    st_read(bp, 'protected_areas')
  })
  
  caribourange<- reactive({
    st_read(spp, 'Caribou Herds')
  })
  
  prj1 <- eventReactive(input$goButton, {
    if (input$prj1) {
      #aoi <- bnd() %>% st_transform(3578) %>% st_union()
      aoi <- region()
      st_read(prj, 'Quartz Claims') %>%
        st_intersection(aoi) %>%
        filter(TENURE_STATUS=='Active')
    } else {
      return(NULL)
    }
  })
  
  prj2 <- eventReactive(input$goButton, {
    if (input$prj2) {
      #aoi <- bnd() %>% st_transform(3578) %>% st_union()
      aoi <- region()
      st_read(prj, 'Placer Claims') %>%
        st_intersection(aoi) %>%
        filter(TENURE_STATUS=='Active')
    } else {
      return(NULL)
    }
  })
  
  spp1 <- eventReactive(input$goButton, {
    if (input$spp1) {
      #aoi <- bnd() %>% st_transform(3578) %>% st_union()
      aoi <- region()
      x <- st_read(spp, 'Caribou Herds')
      x <- st_intersection(x, aoi)
    } else {
      return(NULL)
    }
  })
  
  spp2 <- eventReactive(input$goButton, {
    if (input$spp2) {
      #aoi <- bnd() %>% st_transform(3578) %>% st_union()
      aoi <- region()
      st_read(spp, 'Thinhorn Sheep') %>%
        st_intersection(aoi)
    } else {
      return(NULL)
    }
  })
  
  spp3 <- eventReactive(input$goButton, {
    if (input$spp3) {
      #aoi <- bnd() %>% st_transform(3578) %>% st_union()
      aoi <- region()
      st_read(spp, 'Key Wetlands 2011') %>%
        st_intersection(aoi)
    } else {
      return(NULL)
    }
  })
  
  # section 2 in crs 4326
  ecoregions <- reactive({
    st_read(placemarks, 'ecoregions')
  })
  
  fdas<- reactive({
    st_read(placemarks, 'fdas')
  })
  
  catch_4326 <-reactive({
    st_read(catch4326, 'catchments')
  })
  
  
  # Return a list of reactive elements
  # Return a list containing all reactive elements
  return(list(
    selected_catchments = selected_catchments,
    catch_3578 = catch_3578,
    line = line,
    poly = poly,
    fires = fires,
    ifl_2000 = ifl_2000,
    ifl_2020 = ifl_2020,
    pa_2021 = pa_2021,
    caribourange = caribourange,
    prj1 = prj1,
    prj2 = prj2,
    spp1 = spp1,
    spp2 = spp2,
    spp3 = spp3,
    ecoregions = ecoregions,
    fdas = fdas,
    catch_4326 = catch_4326
  ))
}