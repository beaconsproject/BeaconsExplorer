# Define UI for the module
gpkgUI <- function(id) {
  ns <- NS(id)
  fluidPage(
   fileInput(ns("file"), "Choose a GeoPackage file"),
    selectInput(ns("layer"), "Select a layer", choices = NULL),
  )
}

gpkg_upload <- function(input, output, session) {
  sa_data <- reactiveVal(NULL) # Initialize reactiveVal
 
  observeEvent(input$file, {
    req(input$file)
    layers <- vector_layers(input$file$datapath)
    updateSelectInput(session, "layer", choices = layers)
  })
  observeEvent(input$layer, {
    req(input$layer)
    browser()
    sa_data(vect(input$file$datapath, layer = input$layer))
  })
  return(sa_data)
}  

