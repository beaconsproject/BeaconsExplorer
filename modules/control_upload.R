#------- UI-------------------------
# Define UI for the module
insert_uploadUI <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    tags$div(title = "text",
             radioButtons(ns("geoSel"), 
                          label = "Choose data format:",
                          choices = c("Geopackage" = 'gpkg', "Shapefile" = 'shp'),
                          selected = 'shp',
                          inline = TRUE)),
    conditionalPanel(
      sprintf("input['%s'] == 'gpkg'", ns("geoSel")),
      #condition="input.geoSel == 'gpkg'",
      gpkgUI("geopackage_module")
    ),
    # Check default
    conditionalPanel(
      sprintf("input['%s'] == 'shp'", ns("geoSel")),
      #condition="input.geoSel == 'shp'",
      shpUI("shp_module")
    ),
    actionButton(ns("conf_sa"), "Confirm")
  )
}

#----SERVER LOGIC-------------------------------
# Create a Shiny module
geopackageModule <- function(input, output, session) {
  observeEvent(input$geoSel,{
    #req(input$geoSel)
    if (input$geoSel == "gpkg") {
      callModule(gpkg_upload_module_server, "geopackage_module", "map")
      #gpkg_upload_module_server(input, output, session)
    } else if (input$geoSel == "shp") {
      callModule(shp_upload_module_server, "shp_module", "map")
      #shp_upload_module_server(input, output, session)
    }
  })
}
