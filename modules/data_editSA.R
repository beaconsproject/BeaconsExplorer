# gpkg UI 
editSAUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    actionButton(ns("mod_sa_button"), "Enable boundary editing", icon = icon(name = "pen-to-square", lib = "font-awesome"), class = "btn-success", style="width:250px"),
    br(),
    actionButton(ns("view_sa_button"), "Preview modified study area", icon = icon(name = "rotate", lib = "font-awesome"), class = "btn-success", style="width:250px"),
    br(),
    br(),
    actionButton(ns("conf_sa_button"), "Confirm study area", icon = icon(name = "check", lib = "font-awesome"), class = "btn-warning", style="width:200px"),  )
}


sa.catch <- function(session, sa_sf, catch) {
  req(sa_sf)
  sa.catch <- terra::intersect(centroids(catch), sa_sf)
  return(sa.catch)
}  

edit.SA <- function(sa_spat, catch_4326, myMap) {
  region <- project(sa_spat, "EPSG:4326")
  myMap %>% 
    clearGroup("Study area") %>%
    clearGroup("Catchments") %>%
    addPolygons(data = region, fill = FALSE, color = "blue", weight = 4, group = "Study area", options = leafletOptions(pane = "ground")) %>%
    addPolygons(data = catch_4326, color = 'black', fillOpacity = 0, weight = 1, layerId = catch_4326$CATCHNUM, group = "Catchments", options = leafletOptions(pane = "overlay")) %>%
    addLayersControl(overlayGroups = c("Study area", "Catchments","Ecoregions", "FDAs","Protected areas", "Caribou ranges"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(c("Ecoregions","FDAs","Protected areas", "Caribou ranges"))
}

update.SA <- function(catch_4326, selected_catchments, myMap) {
  data_selected <- catch_4326[catch_4326$CATCHNUM %in% selected_catchments$catchnum,]
  data_select <- terra::aggregate(data_selected) 
  region <- data_select %>%
    buffer(width = 20) %>% 
    buffer(width = -20)  
  myMap %>% 
    clearGroup("Study area") %>%
    addPolygons(data = region, fill = FALSE, color = "blue", weight = 4, group = "Study area", options = leafletOptions(pane = "ground")) #%>%
}

conf.SA <- function(input, output, session, SA, catch_4326, selected_catchments, myMap) {
  region <- reactiveVal(NULL)
  if(is.null(selected_catchments$catchnum)){
    region(project(SA, "EPSG:4326"))
  }else{
    data_selected <- catch_4326[catch_4326$CATCHNUM %in% selected_catchments$catchnum,]
    data_select <- terra::aggregate(data_selected)  %>%
      buffer(width = 20) %>% 
      buffer(width = -20) 
    region(data_select)
  }
  myMap %>%
    clearGroup("Study area") %>%
    clearGroup("Catchments") %>% 
    clearControls() %>%
    addPolygons(data = region(), fill = FALSE, color = "blue", weight = 4, group = "Study area", options = leafletOptions(pane = "ground")) %>%
    addLayersControl(overlayGroups = c("Study area","Ecoregions", "FDAs","Protected areas", "Caribou ranges"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    hideGroup(c("Ecoregions","FDAs","Protected areas", "Caribou ranges"))
  region <- region() %>% project("EPSG:3578")
  return(region)
}