#library(wallace)
library(glue)
library(leaflet)
library(shiny)
library(sf)

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
COMPONENTS <- c("geo", "envs")

# Information about modules that various parts of the app need access to
COMPONENT_MODULES <- list()

# Load all Wallace base modules
all_module_configs <- c(
  "modules/geo_upload.yml"#,
  #"modules/geo_edit.yml",
  #"modules/envs_worldclim.yml",
  #"modules/envs_ecoclimate.yml",
  #"modules/envs_userEnvs.yml"
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

  ui_function <- glue("{id}_module_ui")
  if (!exists(ui_function, envir = temp_env) || !is.function(get(ui_function, envir = temp_env))) {
    stop(glue("Module {id}: Could not find a UI function named `{ui_function}`"),
         call. = FALSE)
  }
  server_function <- glue("{id}_module_server")
  if (!exists(server_function, envir = temp_env) || !is.function(get(server_function, envir = temp_env))) {
    stop(glue("Module {id}: Could not find a server function named `{server_function}`"),
         call. = FALSE)
  }

  # Save the module's UI and server
  module_config$ui_function <- ui_function
  module_config$server_function <- server_function

  # Save the module's result and map code and Rmd variables if they exist
  result_function <- glue("{id}_module_result")
  if (exists(result_function, envir = temp_env) && is.function(get(result_function, envir = temp_env))) {
    module_config$result_function <- result_function
  }
  map_function <- glue("{id}_module_map")
  if (exists(map_function, envir = temp_env) && is.function(get(map_function, envir = temp_env))) {
    module_config$map_function <- map_function
  }
  rmd_function <- glue("{id}_module_rmd")
  if (exists(rmd_function, envir = temp_env) && is.function(get(rmd_function, envir = temp_env))) {
    module_config$rmd_function <- rmd_function
  }

  # Save the module information
  COMPONENT_MODULES[[module_config$component]][[id]] <- module_config

  # Load the module's code
  source(module_config$file, local = TRUE)
}