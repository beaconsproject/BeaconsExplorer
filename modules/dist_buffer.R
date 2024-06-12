#bufferFeatUI <- function(id) {
#  ns <- NS(id)
#  fluidPage(
#    checkboxInput(ns('addpanel'), 'Add custom buffer using the table'),
#    sliderInput(ns("buffer1"), label="Linear buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
#    sliderInput(ns("buffer2"), label="Areal buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
#    sliderInput(ns("area1"), label="Min intact patch size (km2):", min=0, max=2000, value = 0, step=50, ticks=FALSE),
#    hr(),
#    actionButton("goButton", "Generate intactness map", style='color: #000')
#  )
#}

bufferFeatUI <- function(id, mainTabId) {
  ns <- NS(id)
  fluidPage(
    checkboxInput(ns('viewpanel'), 'Add custom buffer using the table', value = FALSE),
    sliderInput(ns('buffer1'), label="Linear buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
    sliderInput(ns('buffer2'), label="Areal buffer size (m):", min=0, max=2000, value = 500, step=50, ticks=FALSE),
    sliderInput(ns('area1'), label="Min intact patch size (km2):", min=0, max=2000, value = 0, step=50, ticks=FALSE),
    hr(),
    actionButton("goButton", "Generate intactness map", style='color: #000')
  )
}


bufferFeatServer <- function(input, output, session, mainTabId, target) {
  inserted <- reactiveVal(FALSE)
  #set min max outside of the observe scope to allow to update
  ######################## #
  ### TAB CONTROL     ####
  ######################## #
  ## do some environment hacking: Get the `session` variabe from the
  ## environment that invoked `callModule`.
  parentSession <- get("session", envir = parent.frame(2))
  observe({
    if (!inserted()) {
        insertTab(inputId = mainTabId, tabPanel(
          "Custom buffers",
          id = "buffer_tab",
          tags$h4("Define linear buffer sizes:"),
          matrixInput("linear_buffers", value = m1, rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE)),
          tags$h4("Define areal buffer sizes:"),
          matrixInput("areal_buffers", value = m2, rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE))
          
        ),
        target = target,
        position = "after",
        select = FALSE,
        session = parentSession
        )
        print("Tab inserted") # Additional debugging
        inserted(TRUE)
    }
  })
    
  observeEvent(input$viewpanel, {
    if (input$viewpanel) {
      disable("buffer1")
      disable("buffer2")
    } else {
      enable("buffer1")
      enable("buffer2")
    }
  })
  
}

bufferFeatServer_backup <- function(input, output, session, mainTabId, target) {
  #set min max outside of the observe scope to allow to update
  ######################## #
  ### TAB CONTROL     ####
  ######################## #
  inserted <- reactiveVal(FALSE)
  ## do some environment hacking: Get the `session` variabe from the
  ## environment that invoked `callModule`.
  parentSession <- get("session", envir = parent.frame(2))
  observeEvent(input$addpanel, {
    if (input$addpanel && !inserted()) {
      browser()
      insertTab(inputId = mainTabId, tabPanel(
        "Custom buffers",
        id = "buffer_tab",
        tags$h4("Define linear buffer sizes:"),
        matrixInput("linear_buffers", value = m1, rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE)),
        tags$h4("Define areal buffer sizes:"),
        matrixInput("areal_buffers", value = m2, rows = list(names = FALSE, extend = TRUE), cols = list(names = TRUE))
        
      ),
      target = target,
      position = "after",
      select = TRUE,
      session = parentSession
      #session = session
      )
      print("Tab inserted") # Additional debugging
      inserted(TRUE)
    }#else{
    # if ("buffer_tab" %in% isolate(input[[mainTabId]])) {
    #   print("Tab unchecked") # Additional debugging
    #   removeTab(inputId = mainTabId, target = "buffer_tab", session = parentSession)
    # }
    #}
  })
}
