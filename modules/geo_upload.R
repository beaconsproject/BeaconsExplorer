# Define UI for the module
geopackageUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Choose a GeoPackage file"),
    selectInput(ns("layer"), "Select a layer", choices = NULL),
    #actionButton(ns("conf_sa"), "Confirm", style="width:200px"),
  )
}

# Define server logic for the module
geopackage <- function(input, output, session) {
  ns <- session$ns
  
  userFile <- reactive({
    validate(need(input$file, message=FALSE))
    input$file
  })
  
  observeEvent(input$file, {
    req(input$file)
    layers <- st_layers(input$file$datapath)$name
    updateSelectInput(session, "layer", choices = layers)
  })
  gpkg <- reactive({
    req(input$layer)
    layer <- st_read(input$file$datapath, layer = input$layer, quiet = TRUE ) %>% 
      st_transform(4326)
  })

}
