editUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    tags$hr(),
    actionButton(ns("enable_edit"), "Enable boundary editing", icon = icon(name = "pen-to-square", lib = "font-awesome"), class = "btn-warning", style="width:250px"),
    tags$hr(),
    actionButton(ns("confirm_edit"), "Confirm study area boundary", icon = icon(name = "rotate", lib = "font-awesome"), class = "btn-success", style="width:250px"),
  )
}

edit_sa <- function(input, output, session, sa_sf) {
  
  observeEvent(input$enable_edit, {
    browser()
    req(sa_sf)
    layers <- vector_layers(input$file$datapath)
    updateSelectInput(session, "layer", choices = layers)
  })
  observeEvent(input$confirm_edit, {
    req(input$layer)
    sa_data(vect(input$file$datapath, layer = input$layer))
  })
  return(sa_data)
}  
