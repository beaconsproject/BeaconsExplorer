# Join a string vector into a single string separated by commas
join <- function(v) paste(v, collapse = ", ")

# Add radio buttons for all modules in a component
insert_modules_options <- function(component) {
  unlist(setNames(
    lapply(COMPONENT_MODULES[[component]], `[[`, "id"),
    lapply(COMPONENT_MODULES[[component]], `[[`, "short_name")
  ))
}

# Add the UI for a module
insert_modules_ui <- function(component) {
  lapply(COMPONENT_MODULES[[component]], function(module) {
    conditionalPanel(
      glue("input.{component}Sel == '{module$id}'"),
      do.call(module$ui_function, list(module$id)),
    )
  })
}

# Add the results section UI of all modules in a component
insert_modules_results <- function(component) {
  lapply(COMPONENT_MODULES[[component]], function(module) {
    if (is.null(module$result_function)) return()
    conditionalPanel(
      glue("input.{component}Sel == '{module$id}'"),
      do.call(module$result_function, list(module$id))
    )
  })
}

# Add helper button for component
help_comp_ui <- function(name) {
  actionLink(name, label = "", icon = icon("circle-question"),
             class = "compHelpButton")
}
