sa.catch <- function(session, sa_sf, catch) {
    req(sa_sf)
    catch.3578 <-catch %>% project("EPSG:3578")
    catch_int <- intersect(centroids(catch.3578), sa_sf)
    sa.catch <- catch.3578[catch_int,]
    return(sa.catch)
}  

edit.SA <- function(sa_spat, sa_catch, catch_4326, selected_catchments, myMap) {
  region <- project(sa_spat, "EPSG:4326")
  #sa_catch <- project(sa_catch, "EPSG:4326")
  myMap %>% 
    clearGroup("Study area") %>%
    clearGroup("Catchments") %>%
    addPolygons(data = region, fill = FALSE, color = "blue", weight = 4, group = "Study area", options = leafletOptions(pane = "ground")) %>%
    addPolygons(data = catch_4326, color = 'black', fillOpacity = 0, weight = 1, layerId = catch_4326$CATCHNUM, group = "Catchments", options = leafletOptions(pane = "overlay")) %>%
    addControl(actionButton(inputId = "clear_button", label = "Clear selection"), position="topleft", className = "class_clear_button") %>%
    addLayersControl(overlayGroups = c("Study area", "Catchments"),
                     options = layersControlOptions(collapsed = FALSE))
}

update.SA <- function(catch_3578, catch_4326, selected_catchments, myMap) {
  data_selected <- catch_3578()[catch_3578()$CATCHNUM %in% selected_catchments$catchnum,]
  data_select <- st_union(data_selected) 
  region <- data_select %>%
    st_buffer(dist = 20) %>% 
    st_buffer(dist = -20) %>%
    st_transform(4326) 
  myMap %>% 
    clearGroup("Study area") %>%
    addPolygons(data = catch_4326, color = 'black', fillColor = "grey", fillOpacity = 0, weight = 1, layerId = catch_4326$CATCHNUM, group = "Catchments", options = leafletOptions(pane = "overlay")) %>%
    addPolygons(data = region, fill = FALSE, color = "blue", weight = 4, group = "Study area", options = leafletOptions(pane = "ground")) %>%
    addLayersControl(overlayGroups = c("Study area", "Catchments"),
                     options = layersControlOptions(collapsed = FALSE))
}

conf.SA <- function(region, myMap) {
  region <- st_transform(region(), 4326)
  myMap %>%
    clearGroup("Study area") %>%
    clearGroup("Catchments") %>%
    addPolygons(data = region, fill = FALSE, color = "blue", weight = 4, group = "Study area", options = leafletOptions(pane = "ground")) %>%
    addLayersControl(baseGroups = c("Esri.WorldTopoMap", "Esri.WorldImagery"),
                     overlayGroups = c("Study area"),
                     options = layersControlOptions(collapsed = FALSE)) %>%
  showModal(modalDialog(
    title = "Study area confirmed. Please select spatial layers",
    easyClose = TRUE,
    footer = modalButton("OK")
  ))
}