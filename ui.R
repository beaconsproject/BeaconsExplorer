tagList(
  navbarPage(
    theme = bslib::bs_theme(version = 3, bootswatch = "yeti"),
    id = 'tabs',
    collapsible = TRUE,
    header = tagList(
      tags$head(tags$link(href = "css/styles.css", rel = "stylesheet"))
    ),
    title = img(src = "beacons.png", height = '50', width = '43.21',
                style = "margin-top: -15px"),
    windowTitle = "BEACONs Conservation Explorer",
    tabPanel("Intro", value = 'intro'),
    tabPanel("Data package", value = 'geo'),
    tabPanel("Footprint map", value = 'envs'),
    navbarMenu("Support", icon = icon("life-ring"),
               HTML('<a href="https://beaconsproject.ualberta.ca/" target="_blank">BEACONs Homepage</a>'),
               HTML('<a href="https://github.com/beaconsproject/geopackage_creator" target="_blank">Github Page</a>'),
               HTML('<a href="https://github.com/beaconsproject/wallace/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: pierre.vernier@gmail.com" target="_blank">Send Email</a>')),
    tabPanel(NULL, icon = icon("power-off"), value = "_stopapp")
  ),
  tags$div(
    class = "container-fluid",
    fluidRow(
      column(4,
        wellPanel(
          conditionalPanel(
            condition="input.tabs == 'intro'",
            includeMarkdown("Rmd/text_intro_tab.Rmd")
          ),
          # OBTAIN OCCS ####
          conditionalPanel(
             condition="input.tabs == 'geo'",
             div("Create Data Package", class = "componentName"),
             geopackageUI("geopackage_module")
          )
        )
      ),
      # --- RESULTS WINDOW ---
      column(8,
             conditionalPanel(
               "input.tabs != 'intro' & input.tabs != 'rep'",
               #"input.tabs == 'geo'",
               tabsetPanel(
                 id = 'main',
                 tabPanel(
                   'Map',
                   leaflet::leafletOutput("map", height = 700),
                   absolutePanel(
                     top = 50, right = 20, width = 150, draggable = TRUE,
                     selectInput("bmap", "",
                                 choices = c('ESRI Imagery' = "Esri.WorldImagery",
                                             'ESRI Nat Geo' = 'Esri.NatGeoWorldMap'),
                                 selected = "Esri.WorldTopoMap"
                     )
                   )
                 )
               )
             )
      )
    )
  )
)