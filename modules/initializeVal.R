# set a reactive value for selected catchments
selected_catchments <- reactiveValues( # this is the list of currently selected catchments
  catchnum = c()
)
  
# Section 1 in csr: 3578
catch_3578 <-reactive({
  vect(catch3578, 'catchments')
})
  
line <- reactive({
  vect(bp, 'sd_line') %>%
     intersect(region()) #%>%
    #st_cast('MULTILINESTRING')
})
  
poly <- reactive({
  vect(bp, 'sd_poly') %>%
    #st_as_sf() %>%
    #st_cast('MULTIPOLYGON') %>%
    intersect(region()) #%>%
    #st_cast('MULTIPOLYGON')
})
  
fires <- reactive({
  req(bnd())
  vect(bp, 'fires') %>%
    #st_as_sf() %>%
    #st_cast('MULTIPOLYGON') %>%
    tidyterra::filter(YEAR >= input$minmax[1] & YEAR <= input$minmax[2]) %>%
    intersect(region()) #%>%
    #st_cast('MULTIPOLYGON')
})
  
ifl_2000 <- reactive({
  vect(bp, 'ifl_2000') %>%
    intersect(region())
})
  
ifl_2020 <- reactive({
  vect(bp, 'ifl_2020') %>%
    intersect(region())
})
  
pa_2021 <- reactive({
  vect(bp, 'protected_areas')
})
  
caribourange<- reactive({
  vect(spp, 'Caribou Herds')
})
  
prj1 <- eventReactive(input$goButton, {
  if (input$prj1) {
      #aoi <- bnd() %>% st_transform(3578) %>% st_union()
    aoi <- region()
    vect(prj, 'Quartz Claims') %>%
      intersect(aoi) %>%
      tidyterra::filter(TENURE_STATUS=='Active')
  } else {
    return(NULL)
  }
})
  
prj2 <- eventReactive(input$goButton, {
  if (input$prj2) {
    #aoi <- bnd() %>% st_transform(3578) %>% st_union()
    aoi <- region()
    vect(prj, 'Placer Claims') %>%
      intersect(aoi) %>%
      tidyterra::filter(TENURE_STATUS=='Active')
  } else {
    return(NULL)
  }
})
  
spp1 <- eventReactive(input$goButton, {
  if (input$spp1) {
    #aoi <- bnd() %>% st_transform(3578) %>% st_union()
    aoi <- region()
    vect(spp, 'Caribou Herds') %>%
      intersect(x, aoi)
  } else {
    return(NULL)
  }
})
  
spp2 <- eventReactive(input$goButton, {
  if (input$spp2) {
    #aoi <- bnd() %>% st_transform(3578) %>% st_union()
    aoi <- region()
    vect(spp, 'Thinhorn Sheep') %>%
      intersect(aoi)
  } else {
    return(NULL)
  }
})
  
spp3 <- eventReactive(input$goButton, {
  if (input$spp3) {
    #aoi <- bnd() %>% st_transform(3578) %>% st_union()
    aoi <- region()
    vect(spp, 'Key Wetlands 2011') %>%
      intersect(aoi)
  } else {
    return(NULL)
  }
  })
  
# section 2 in crs 4326
ecoregions <- reactive({
  vect(placemarks, 'ecoregions')
})
  
fdas<- reactive({
  vect(placemarks, 'fdas')
})
  
catch_4326 <- vect(catch4326, 'catchments')