# Define a module to manage the reactive values
reactiveLayersModule <- function(input, output, session, layers, region) {
  layers <- reactiveValues(catch_3578 = reactiveVal(NULL),
                           catch_4326 = reactiveVal(NULL),
                           line = reactiveVal(NULL),
                           poly = reactiveVal(NULL),
                           fires = reactiveVal(NULL),
                           ifl_2000 = reactiveVal(NULL),
                           ifl_2020 = reactiveVal(NULL),
                           pa_2021 = reactiveVal(NULL),
                           prj1 = reactiveVal(NULL),
                           prj2 = reactiveVal(NULL),
                           spp1 = reactiveVal(NULL),
                           spp2 = reactiveVal(NULL),
                           spp3 = reactiveVal(NULL),
                           footprint = reactiveVal(NULL),
                           intactness = reactiveVal(NULL)
  )
  
  observe({
    layers$catch_3578 <- vect(catch3578, 'catchments')
    layers$catch_4326 <- vect(catch4326, 'catchments')
    layers$line <- vect(bp, 'sd_line')
    layers$poly <- vect(bp, 'sd_poly')
    layers$fires <- vect(bp, 'fires')
    layers$ifl_2000 <- vect(bp, 'ifl_2000')
    layers$ifl_2020 <- vect(bp, 'ifl_2020')
    layers$pa_2021 <- vect(bp, 'protected_areas')
    layers$prj1 <- vect(prj, 'Quartz Claims')
    layers$prj2 <- vect(prj, 'Placer Claims')
    layers$spp1 <- vect(spp, 'Caribou Herds')
    layers$spp2 <- vect(spp, 'Thinhorn Sheep')
    layers$spp3 <- vect(spp, 'Key Wetlands 2011')
  })
  
  observe({
    req(region)
    # Clip spatial data based on the region
    layers$line <- isolate(layers$line) %>%
      terra::intersect(region)
    layers$poly <- isolate(layers$poly) %>%
      terra::intersect(region)
    layers$fires <- isolate(layers$fires) %>%
      terra::intersect(region)
    layers$ifl_2000 <- isolate(layers$ifl_2000) %>%
      terra::intersect(region)
    layers$ifl_2020 <- isolate(layers$ifl_2020) %>%
      terra::intersect(region)
    layers$pa_2021 <- isolate(layers$pa_2021) %>%
      terra::intersect(region)
    layers$prj1 <- isolate(layers$prj1) %>%
      terra::intersect(region)
    layers$prj2 <- isolate(layers$prj2) %>%
      terra::intersect(region)
    layers$spp1 <- isolate(layers$spp1) %>%
      terra::intersect(region)
    layers$spp2 <- isolate(layers$spp2) %>%
      terra::intersect(region)
    layers$spp3 <- isolate(layers$spp3) %>%
      terra::intersect(region)
  })
  return(layers)
}

  
