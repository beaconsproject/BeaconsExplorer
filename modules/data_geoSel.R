saDialogUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("geoSel"), "Select File Type for linear features:",
                 choices = list("Shapefile" = "shp", "GeoPackage" = "gpkg"),
                 selected = "shp", 
                 inline = TRUE),
    #uiOutput(ns("dynamicUploadSAUI")),
    uiOutput(ns("upload_sa")),
  )
}

saDialogServer <- function(input, output, session, sa_spat) {
  ns <- session$ns
  
  observeEvent(input$geoSel, {
    output$upload_sa <- renderUI({
         if (input$geoSel == "gpkg") {
           output$upload_sa <- renderUI({
             gpkgUI("upload_sa")
            })
           sa_spat(callModule(gpkg_upload,"upload_sa"))
        }else if (input$geoSel == "shp") {
          output$upload_sa <- renderUI({
            shpUI("upload_sa")
          })
          sa_spat(callModule(shp_upload, "upload_sa"))
        }
      })
    })
#  observeEvent(input$geoSel, {
#    if (input$geoSel == "gpkg") {
#      sa_spat(callModule(gpkg_upload, "upload_sa"))
#    } else if (input$geoSel == "shp") {
#      sa_spat(callModule(shp_upload, "upload_sa"))
#    }
# })
  if(!is.null(sa_spat)){
    return(sa_spat)
  }
}