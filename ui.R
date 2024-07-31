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
    tabPanel("Data Initialization", value = 'data'),
    tabPanel("Disturbance Analysis", value = 'dist'),
    navbarMenu("Support", icon = icon("life-ring"),
               HTML('<a href="https://beaconsproject.ualberta.ca/" target="_blank">BEACONs Homepage</a>'),
               HTML('<a href="https://github.com/beaconsproject/geopackage_creator" target="_blank">Github Page</a>'),
               HTML('<a href="https://github.com/beaconsproject/wallace/issues" target="_blank">GitHub Issues</a>'),
               HTML('<a href="mailto: pierre.vernier@gmail.com" target="_blank">Send Email</a>')),
    tabPanel(NULL, icon = icon("power-off"), value = "_stopapp"),
  ),
  tags$div(
    useShinyjs(),
    class = "container-fluid",
    fluidRow(
      column(4,
             wellPanel(
               conditionalPanel(
                 condition="input.tabs == 'intro'",
                 includeMarkdown("Rmd/text_intro_tab.Rmd")
               ),
               # OBTAIN Data ####
               conditionalPanel(
                 condition="input.tabs == 'data'",
                 tabsetPanel(
                   tabPanel("Component",
                            div("Create Data Package", class = "moduleName"),
                            help_comp_ui("dataHelp"),
                            uiOutput("radio_buttons_ui"),
                            uiOutput("upload_module"),
                            uiOutput("editSA_module"),
                            hidden(div(id = "modalButtonContainer", 
                                       strong("1. Upload additional Features"),
                                       modalDialogUI("modal"))),
                            uiOutput("selLayer_module"),
                            uiOutput("switch_component")
                   ) 
                 )  
               ),
               conditionalPanel(
                 condition="input.tabs == 'dist'",
                 tabsetPanel(
                   tabPanel("Component",
                            div("Create Footprint Map", class = "moduleName"),
                            help_comp_ui("distHelp"),
                            uiOutput("buffer_module"),
                            uiOutput("distdwd_module")
                   ), 
                   tabPanel("Statistics table", tableOutput("buffStats"))
                 )  
               )
             )
      ),
      # --- RESULTS WINDOW ---
      column(8,
             conditionalPanel(
               "input.tabs != 'intro' & input.tabs != 'rep'",
               tabsetPanel(
                 id = 'main',
                 tabPanel(
                   'Map',
                   leaflet::leafletOutput("myMap", height = 700),
                   absolutePanel(
                     top = 32, left = 60, width = 150, draggable = TRUE,
                     selectInput("bmap", "",
                                 choices = c("ESRI Imagery" = 'Esri.WorldImagery',
                                             "ESRI Nat Geo" = 'Esri.NatGeoWorldMap',
                                             "Blank Background" = "Blank.map"),
                                 selected = 'Esri.NatGeoWorldMap'
                     )
                   ),
                 ),
                 tabPanel(
                   'Module Guidance', icon = icon("circle-info"),
                   uiOutput('gtext_module')
                 )
               )
             )
      )
    )
  )
)