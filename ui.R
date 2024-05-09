tagList(
  navbarPage(
    theme = bslib::bs_theme(version = 3, bootswatch = "yeti"),
    id = 'tabs',
    collapsible = TRUE,
    header = tagList(
      tags$head(tags$link(href = "css/styles.css", rel = "stylesheet"))
    ),
    title = HTML('<div style="margin-top: -10px;"><a href="https://beaconsproject.ualberta.ca/" target="_blank"><img src="beacons.png" height="50"></a></div>'),
    windowTitle = "BEACONs Explorer",
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
             radioButtons(inputId ="geoSel", 
                          label = "Choose data format:",
                          choices = c("Geopackage" = 'gpkg', "Shapefile" = 'shp'),
                          selected = character(0),
                          inline = TRUE),
             uiOutput("upload_module"),
             actionButton("conf_sa", "Visualize study area", disabled= TRUE),
             uiOutput("editsa_module")

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
                   leaflet::leafletOutput("myMap", height = 700),
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