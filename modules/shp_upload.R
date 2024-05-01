#----SHP-------------------------------
#shp UI
shpUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    fileInput(ns("shp_input"), "Upload a Shapefile", multiple = TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg')),
    actionButton(ns("conf_sa"), "Confirm", style="width:200px"),
    leafletOutput(ns("map"))
  )
}

shp_upload <- function(input, output, session) {
  ns <- session$ns
  
  # Reactive value to store uploaded shapefile
  shp_data <- reactiveVal(NULL)
  
  observeEvent(input$shp_input, {
    req(input$shp_input)
    if(length(shp_input>1)){
      dir <- unique(dirname(path$datapath)) # get the directory
      outfiles <- file.path(dir, path$name) # create new path name
      name <- strsplit(path$name[1], "\\.")[[1]][1] # strip name 
      purrr::walk2(path$datapath, outfiles, ~file.rename(.x, .y)) # rename files
      shp <-read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
      shp_data(shp)
    }
  })
  
  # Return the selected layer as an sf object
  observeEvent(input$conf_sa, {
    req(shp_data())
    shplayer <- shp_data() %>% st_transform(4326)
    
    leafletProxy("map")%>%
      addPolygons(data = shplayer)
  })
}  
## Create a Shiny module
#shp_upload_module_server <- function(input, output, session) {
#  shp_upload(input, output, session)
#}
shp_upload_module_server <- function(id, map_id) {
  moduleServer(id, function(input, output, session) {
    shp_upload(input, output, session)
  })
}
#shp_upload_module_server <- function(id, callback) {
#  moduleServer(
#    id,
#    function(input, output, session) {
#      observeEvent(input$shp_input, {
        #observeEvent(input$conf_sa, {
#        req(input$shp_input)
#        if(length(shp_input>1)){
#          browser()
#          dir <- unique(dirname(path$datapath)) # get the directory
#          outfiles <- file.path(dir, path$name) # create new path name
#          name <- strsplit(path$name[1], "\\.")[[1]][1] # strip name 
#          purrr::walk2(path$datapath, outfiles, ~file.rename(.x, .y)) # rename files
#          read_sf(file.path(dir, paste0(name, ".shp"))) # read-in shapefile
#        }
        
#      })
      
      # Return the selected layer as an sf object
 #     observe({
 #       req(input$conf_sa>0) 
 #       shplayer <- shp %>% st_transform(4326)
        
 #       leafletProxy("map")%>%
 #         addPolygons(data = shplayer)
 #     })
 #   }
 # )
#}
