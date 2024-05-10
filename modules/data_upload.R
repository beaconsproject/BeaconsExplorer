######################## #
### GPKG            ####
######################## #
# gpkg UI 
gpkgUI <- function(id) {
  ns <- NS(id)
  fluidPage(
   fileInput(ns("file"), "Choose a GeoPackage file"),
    selectInput(ns("layer"), "Select a layer", choices = NULL),
  )
}
# GPKG server module
gpkg_upload <- function(input, output, session, parent) {
  sa_data <- reactiveVal(NULL) # Initialize reactiveVal
 
  observeEvent(input$file, {
    req(input$file)
    layers <- vector_layers(input$file$datapath)
    updateSelectInput(session, "layer", choices = layers)
    updateActionButton(parent, 'conf_sa', disabled = FALSE)
  })
  observeEvent(input$layer, {
    req(input$layer)
    sa_data(vect(input$file$datapath, layer = input$layer))
  })
  return(sa_data)
}  

######################## #
### SHP             ####
######################## #
#shp UI
shpUI <- function(id) {
  ns <- NS(id)
  ui <-fluidRow(
    fileInput(ns("shp_input"), "Upload a Shapefile", multiple = TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg')),
  )
}
# shp server
shp_upload <- function(input, output, session, parent) {
  sa_data <- reactiveVal(NULL) # Initialize reactiveVal
  
  observeEvent(input$shp_input, {
    req(input$shp_input)
    infile <- input$shp_input
    if(length(input$shp_input>1)){
      dir <- unique(dirname(infile$datapath)) # get the directory
      outfiles <- file.path(dir, infile$name) # create new path name
      name <- strsplit(infile$name[1], "\\.")[[1]][1] # strip name 
      purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y)) # rename files
      sa_data(vect(file.path(dir, paste0(name, ".shp")))) # read-in shapefile  
    }
    updateActionButton(parent, 'conf_sa', disabled = FALSE)
  })
  return(sa_data)  
}