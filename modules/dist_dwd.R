# downloadModuleUI.R
downloadModuleUI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("downloadData"), "Download Data", class = ("btn-info bottom-fixed"))
  )
}

##############################################################################
# Save features to a geopackage
##############################################################################
downloadModuleServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      output$downloadData <- downloadHandler(
        filename = function() { paste("disturbance_explorer-", Sys.Date(), ".gpkg", sep="") },
        content = function(file) {
        if(is.null(input$upload_poly)) { #upload polygon
          st_write(fda(), dsn=file, layer='studyarea')
          st_write(line(), dsn=file, layer='linear_disturbance', append=TRUE)
          st_write(poly(), dsn=file, layer='areal_disturbance', append=TRUE)
          if (input$goButton) {
            st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
            st_write(intactness_sf(), dsn=file, layer='intactness', append=TRUE)
          }
        } else {
          poly_clip <- st_intersection(poly(), aoi_bnd())
          line_clip <- st_intersection(line(), aoi_bnd())
          st_write(aoi_bnd(), dsn=file, layer='studyarea')
          st_write(line_clip, dsn=file, layer='linear_disturbance', append=TRUE)
          st_write(poly_clip, dsn=file, layer='areal_disturbance', append=TRUE)
          if (input$goButton) {
            st_write(footprint_sf(), dsn=file, layer='footprint', append=TRUE)
            st_write(intactness_sf(), dsn=file, layer='intactness', append=TRUE)
          }
        }
      }
    )
   }
  )
}
