#----SHP-------------------------------
#shp UI
shpUI <- function(id) {
  ns <- NS(id)
  ui <-fluidRow(
    fileInput(ns("shp_input"), "Upload a Shapefile", multiple = TRUE, accept=c('.shp','.dbf','.sbn','.sbx','.shx','.prj','.cpg')),
  )
}

shp_upload <- function(input, output, session) {
  sa_data <- reactiveVal(NULL) # Initialize reactiveVal
  
  observeEvent(input$shp_input, {
    req(input$shp_input)
    infile <- input$shp_input
    if(length(input$shp_input>1)){
      dir <- unique(dirname(infile$datapath)) # get the directory
      outfiles <- file.path(dir, infile$name) # create new path name
      name <- strsplit(infile$name[1], "\\.")[[1]][1] # strip name 
      purrr::walk2(infile$datapath, outfiles, ~file.rename(.x, .y)) # rename files
      sa_data(st_read(file.path(dir, paste0(name, ".shp")))) # read-in shapefile  
    }
  })
  return(sa_data)  
}