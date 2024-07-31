library(glue)
library(leaflet)
library(shiny)
library(gpkg)
library(purrr)
library(markdown)
library(terra)
library(shinyjs) # toggle
library(tidyverse)
library(dplyr)
library(shinyBS)
library(shinyMatrix)
library(bslib)
# data component related
bp <- 'www/data/demo_datasets.gpkg'
spp <- 'www/data/species.gpkg'
prj <- 'www/data/projected.gpkg'
placemarks <- 'www/data/placemarks_4326.gpkg'
catch4326 <- 'www/data/catchments1M_4326.gpkg'
catch3578 <- 'www/data/catchments1M_3578.gpkg'
limits <- vect(bp, 'fda') %>% terra::union() %>% project("EPSG:4326")
## dist component related
m1 <- as.matrix(read_csv('docs/cas.csv')[42:66,2:4]) #%>% filter(TYPE_DISTURBANCE %in% x1))
m2 <- as.matrix(read_csv('docs/cas.csv')[1:41,2:4]) #%>% filter(TYPE_DISTURBANCE %in% x2))



# set a reactive value for selected catchments
selected_catchments <- reactiveValues( # this is the list of currently selected catchments
  catchnum = c()
)

# section 2 in crs 4326
ecoregions <- reactive({
  vect(placemarks, 'ecoregions')
})

fdas<- reactive({
  vect(placemarks, 'fdas')
})


MB <- 1024^2

UPLOAD_SIZE_MB <- 5000
options(shiny.maxRequestSize = UPLOAD_SIZE_MB*MB)

SAVE_SESSION_SIZE_MB_WARNING <- 100

source("helpers.R")

# Load all base modules (old format)
# TODO this should not exist after moving all modules to the new format
base_module_files <- list.files('modules', pattern = "\\.R$", full.names = TRUE)
for (file in base_module_files) source(file, local = TRUE)

# The components that have modules. These names must match the values of the
# tabs of the components in the UI.
COMPONENTS <- c("data", "envs")

# Information about modules that various parts of the app need access to
COMPONENT_MODULES <- list()

# Load all Wallace base modules
all_module_configs <- c(
  "modules/data_upload.yml"
)

for (module_config_file in all_module_configs) {
  # Read each user-defined module config file
  module_config <- yaml::read_yaml(module_config_file)
  config_dir <- dirname(module_config_file)
  id <- tools::file_path_sans_ext(basename(module_config_file))
  module_config$id <- id

  # Perform lots of error checking to ensure the module was written properly
  required_fields <- c("component", "short_name", "long_name")

  missing <- required_fields[!required_fields %in% names(module_config)]
  if (length(missing) > 0) {
    stop(glue("Module {id}: Some required fields are missing: {join(missing)}"),
         call. = FALSE)
  }

  if (!module_config$component %in% COMPONENTS) {
    stop(glue("Module {id}: Invalid component `{module_config$component}` ",
              "(options are: {join(COMPONENTS)})"), call. = FALSE)
  }

  module_config$instructions <- suppressWarnings(
    normalizePath(file.path(config_dir, glue("{id}.md")))
  )
  if (!file.exists(module_config$instructions)) {
    stop(glue("Module {id}: Instructions file `{module_config$instructions}` was expected but not found"), call. = FALSE)
  }

  rmd_file <- suppressWarnings(
    normalizePath(file.path(config_dir, glue("{id}.Rmd")))
  )
  if (file.exists(rmd_file)) {
    module_config$rmd_file <- rmd_file
  }

  module_config$file <- suppressWarnings(
    normalizePath(file.path(config_dir, glue("{id}.R")))
  )
  if (!file.exists(module_config$file)) {
    stop(glue("Module {id}: Source file `{module_config$file}` was expected but not found"), call. = FALSE)
  }
  temp_env <- new.env()
  source(module_config$file, local = temp_env)

  # Save the module information
  COMPONENT_MODULES[[module_config$component]][[id]] <- module_config

  # Load the module's code
  source(module_config$file, local = TRUE)
}


