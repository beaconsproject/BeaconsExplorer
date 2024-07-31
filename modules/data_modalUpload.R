
modalDialogUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("showModal"), "Upload additional features"),
    uiOutput(ns("upload_line")),
    uiOutput(ns("upload_poly"))
  )
}

modalDialogServer <- function(input, output, session) {
  ns <- session$ns
  
  observeEvent(input$showModal, {
    showModal(modalDialog(
      title = "Upload a File",
      radioButtons(ns("lineType"), "Select File Type for linear features:",
                   choices = list("Shapefile" = "shp", "GeoPackage" = "gpkg"),
                   selected = "shp"),
      uiOutput(ns("dynamicUploadLineUI")),
      uiOutput(ns("colLine_select")), # New UI output for column selection
      radioButtons(ns("polyType"), "Select File Type for polygon features:",
                   choices = list("Shapefile" = "shp", "GeoPackage" = "gpkg"),
                   selected = "shp"),
      uiOutput(ns("dynamicUploadPolyUI")),
      uiOutput(ns("colPoly_select")), # New UI output for column selection
      footer = tagList(
        modalButton("Cancel"),
        actionButton(ns("confirmUpload"), "Upload")
      )
    ))
  })
  
  observeEvent(input$lineType, {
    output$dynamicUploadLineUI <- renderUI({
      if (input$lineType == "gpkg") {
        gpkgUI(ns("upload_line"))
      } else if (input$lineType == "shp") {
        shpUI(ns("upload_line"))
      }
    })
  })
  
  observeEvent(input$polyType, {
    output$dynamicUploadPolyUI <- renderUI({
      if (input$polyType == "gpkg") {
        gpkgUI(ns("upload_poly"))
      } else if (input$polyType == "shp") {
        shpUI(ns("upload_poly"))
      }
    })
  })
  
  upload_features <- reactiveValues(add_line =reactiveVal(NULL),
                                    add_poly =reactiveVal(NULL),
                                    col_line =reactiveVal(NULL),
                                    col_poly =reactiveVal(NULL))

  # Separate observers for file uploads to update the reactive value
  observeEvent(callModule(gpkg_upload, "upload_line"), {
    upload_features$add_line <- callModule(gpkg_upload, "upload_line")
  })
  
  observeEvent(callModule(shp_upload, "upload_line"), {
    upload_features$add_line <- callModule(shp_upload, "upload_line")
  })
  
  # Separate observers for file uploads to update the reactive value
  observeEvent(callModule(gpkg_upload, "upload_poly"), {
    upload_features$add_poly <- callModule(gpkg_upload, "upload_poly")
  })
  
  observeEvent(callModule(shp_upload, "upload_poly"), {
    upload_features$add_poly <- callModule(shp_upload, "upload_poly")
  })
  
  # Observe changes in the uploaded file and update the column select input
  observeEvent(upload_features$add_line(), {
    sf_obj <- upload_features$add_line()
    if (!is.null(sf_obj)) {
      col_names <- names(sf_obj)
      output$colLine_select <- renderUI({
        selectInput(ns("line_column"), "Select Column:", choices = col_names)
      })
    }
  })
  
  observeEvent(upload_features$add_poly(), {
    sf_obj <- upload_features$add_poly()
    if (!is.null(sf_obj)) {
      col_names <- names(sf_obj)
      output$colPoly_select <- renderUI({
        selectInput(ns("poly_column"), "Select Column:", choices = col_names)
      })
    }
  })
  
  ################################################################################################
  # Set layers choices
  ################################################################################################
  observeEvent(!is.null(input$line_column), {
    req(input$line_column)
    upload_features$col_line <- input$line_column
  })
  
  observeEvent(!is.null(input$poly_column), {
    req(input$poly_column)
    upload_features$col_poly <- input$poly_column
  })

  observeEvent(input$confirmUpload, {
    # Handle the file upload confirmation
    removeModal()
  })
  
  output$upload_line <- renderUI({
    req(upload_features$add_line())
    tagList(
      verbatimTextOutput(ns("lineData"))
    )
  })
  
  output$upload_poly <- renderUI({
    req(upload_features$add_poly())
    tagList(
      verbatimTextOutput(ns("polyData"))
    )
  })
  
  output$lineData <- renderPrint({
    basename(sources(upload_features$add_line()))
  })
  
  output$polyData <- renderPrint({
    basename(sources(upload_features$add_poly()))
  })
  return(upload_features)
}
