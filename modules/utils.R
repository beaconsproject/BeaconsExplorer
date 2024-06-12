# Module UI
switchTabButtonUI <- function(id, tab) {
  ns <- NS(id)
  actionButton(ns("switch_btn"), paste0("Run ", tab), icon = icon(name = "rotate", lib = "font-awesome"), class = "btn-warning", style = "width:250px")
}

# Module server
switchTabButtonServer <- function(id, parent_session, tab) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$switch_btn, {
      updateTabsetPanel(session = parent_session, inputId = "tabs", selected = tab)
    })
  })
}