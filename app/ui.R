
# Define UI
# -----------------------------

# 1950 - 2017
choices_years <- format(seq.Date(from = as.Date('1-1-1950', "%m-%d-%Y"), to = as.Date('12-31-2016', "%m-%d-%Y"), by="year"), "%Y")

# month, day, year ("%m-%d-%Y")
dateToShow <- "6-9-1992"

# states, took out "(PR) Puerto Rico" and "(VI) Virgin Islands" because not in geoJSON
choices_states <- 
  c("(AK)	Alaska", "(AL)	Alabama", "(AZ)	Arizona", "(AR)	Arkansas", "(CA)	California", "(CO)	Colorado",
    "(CT)	Connecticut", "(DE)	Delaware", "(FL)	Florida", "(GA)	Georgia", "(HI)	Hawaii", "(ID)	Idaho",
    "(IL)	Illinois", "(IN)	Indiana", "(IA)	Iowa", "(KS)	Kansas", "(KY)	Kentucky", "(LA)	Louisiana",
    "(ME)	Maine", "(MD)	Maryland", "(MA)	Massachusetts", "(MI)	Michigan", "(MN)	Minnesota", "(MS)	Mississippi",
    "(MO)	Missouri", "(MT)	Montana", "(NE)	Nebraska", "(NV)	Nevada", "(NH)	New Hampshire", "(NJ)	New Jersey",
    "(NM)	New Mexico", "(NY)	New York", "(NC)	North Carolina", "(ND)	North Dakota", "(OH)	Ohio", 
    "(OK)	Oklahoma", "(OR)	Oregon", "(PA)	Pennsylvania", "(RI)	Rhode Island",
    "(SC)	South Carolina", "(SD)	South Dakota", "(TN)	Tennessee", "(TX)	Texas", "(UT)	Utah", "(VT)	Vermont",
    "(VA)	Virginia", "(WA)	Washington", "(WV)	West Virginia", "(WI)	Wisconsin",
    "(WY)	Wyoming")

# -----------------------------
ui <- fluidPage(
  
  # -----------------------------
  useShinyjs(),  # Include shinyjs
  
  # -----------------------------
  # Styling
  
  theme = shinytheme("darkly"),
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "ui.css")),
  
  # -----------------------------
  # App title & modal buttons
  
  br(),
  fluidRow(column(8, offset = 2, align = 'justify',
                  sliderTextInput(inputId = "slider_years", 
                    label = NULL, width = '100%', grid = TRUE, force_edges = FALSE, hide_min_max = TRUE,
                    choices = choices_years, selected = '1992',
                    animate=animationOptions(interval = 2000, loop = FALSE, playButton = NULL, pauseButton= NULL)
                  )),
      
           column(2, offset = 0, align = 'right',
                  h1("You Spin Me Round"),
                  actionButton("action_Settings", label = "", icon = icon("cog", "fa-3x")),
                  actionButton("action_About", label = "", icon = icon("info", "fa-3x")))),
  br(),
  
  # -----------------------------
  # Main Navigation
  
  column(10,
  
  tabsetPanel(id = "mainNav",
              
  # -----------------------------
  # Overview
              
              tabPanel("Overview", br(),
                       column(3, 
                              
                              # wellPanel(
                              #   fluidRow(column(12, align = "left", "placeholder"))),
                              # 
                              
                              # wellPanel(
                              #   fluidRow(column(12, align = "left", "placeholder")))
                              
                              wellPanel(
                                fluidRow(column(12, align = "left",
                                                tags$style(type = "text/css", "#c1_state1_table {color: black; min-height:30vh !important;}"),
                                                DTOutput("b4_state1_table"))
                                )),
                              
                              wellPanel(
                                fluidRow(column(12, align = "left",
                                                tags$style(type = "text/css", "#c1_state1_table {color: black;min-height:30vh !important;}"),
                                                DTOutput("b4_state2_table"))
                                ))
                              
                              ),
                       column(9,
                              wellPanel(
                                fluidRow(column(12, offset = 0, align = 'justify',
                                                tags$style(type = "text/css", "#overview_map {min-height:74vh !important;z-index:5 !important;}"),
                                                leafletOutput("overview_map")
                                )))
                              )
                       ),
              
  # -----------------------------
  # Explore Further
                       
              tabPanel("Explore Further", br(),

                       column(4, offset = 0, align = 'justify',
                              wellPanel(
                              fluidRow(
                                tabBox(id = "hideTabs", title = "Tornadoes", side = "left", width = 12, 
                                       
  # -----------------------------
  # C1
                                       tabPanel("", br(), #C1
                                                tabsetPanel(id = "chartAndTable_tabs",
                                                            tabPanel("Chart", 
                                                                     fluidRow(
                                                                       column(6,
                                                                              tags$style(type = "text/css", "#c1_state1 {min-height:30vh !important;}"),
                                                                              plotlyOutput("c1_state1")),
                                                                       column(6,
                                                                              tags$style(type = "text/css", "#c1_state2 {min-height:30vh !important;}"),
                                                                              plotlyOutput("c1_state2"))
                                                                       
                                                                     )
                                                            ),
                                                            tabPanel("Table", 
                                                                     fluidRow(
                                                                       column(6,
                                                                              tags$style(type = "text/css", "#c1_state1_table {min-height:30vh !important;}"),
                                                                              DTOutput("c1_state1_table")),
                                                                       column(6,
                                                                              tags$style(type = "text/css", "#c1_state2_table {min-height:30vh !important;}"),
                                                                              DTOutput("c1_state2_table"))
                                                                     )
                                                            )
                                                )
                                                
                                       )
                                       
                                )
                                ),
                              br(),
  
                              fluidRow(
                                
                                  tabBox(id = "hideTabs", title = "Injuries, Fatalities, Loss", side = "left", width = 12,
                                         
                                         # -----------------------------
                                         # C5
                                         tabPanel("", br(), #C5
                                                  tabsetPanel(id = "chartAndTable_tabs",
                                                              tabPanel("Chart", 
                                                                       fluidRow(
                                                                         column(6,
                                                                                tags$style(type = "text/css", "#c5_state1 {min-height:30vh !important;}"),
                                                                                plotlyOutput("c5_state1")),
                                                                         column(6,
                                                                                tags$style(type = "text/css", "#c5_state2 {min-height:30vh !important;}"),
                                                                                plotlyOutput("c5_state2"))
                                                                       )
                                                              ),
                                                              tabPanel("Table",
                                                                       fluidRow(
                                                                         column(6,
                                                                                tags$style(type = "text/css", "#c5_state1_table {min-height:30vh !important;}"),
                                                                                DTOutput("c5_state1_table")),
                                                                         column(6,
                                                                                tags$style(type = "text/css", "#c5_state2_table {min-height:30vh !important;}"),
                                                                                DTOutput("c5_state2_table"))
                                                                       )
                                                              )
                                                  )
                                         )    
                                  )
                            
                                )),
  
                                fluidRow(column(2, offset = 10, align = 'right',
                                                actionButton("action_AddCharts", label = "Additional Charts"),
                                                
                                                tags$div(id = "extraCharts",
                                                         fluidRow(
                                                           tabBox(title = "Tornadoes (summed over all years)", side = "left", width = 12,
                                                                  
                                                                  # -----------------------------
                                                                  # C2
                                                                  tabPanel("Per Month", #C2
                                                                           tabsetPanel(id = "chartAndTable_tabs",
                                                                                       tabPanel("Chart", 
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c2_state1 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c2_state1")), #, height = "80vh"
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c2_state2 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c2_state2"))
                                                                                                )
                                                                                       ),
                                                                                       tabPanel("Table", 
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c2_state1_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c2_state1_table")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c2_state1_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c2_state2_table"))
                                                                                                )
                                                                                       )
                                                                           )
                                                                           
                                                                  ),
                                                                  
                                                                  # -----------------------------
                                                                  # C3
                                                                  tabPanel("Per Hour", #C3
                                                                           tabsetPanel(id = "chartAndTable_tabs",
                                                                                       tabPanel("Chart", 
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c3_state1 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c3_state1")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c3_state2 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c3_state2"))
                                                                                                )
                                                                                       ),
                                                                                       tabPanel("Table",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c3_state1_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c3_state1_table")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c3_state2_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c3_state2_table"))
                                                                                                )
                                                                                       )
                                                                           )
                                                                  ),
                                                                  
                                                                  # -----------------------------
                                                                  # C4
                                                                  tabPanel("Distance From City", #C4
                                                                           tabsetPanel(id = "chartAndTable_tabs",
                                                                                       tabPanel("Chart", 
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c4_state1 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c4_state1")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c4_state2 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c4_state2"))
                                                                                                )
                                                                                       ),
                                                                                       tabPanel("Table",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c4_state1_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c4_state1_table")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c4_state2_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c4_state2_table"))
                                                                                                )
                                                                                       )
                                                                           ),
                                                                           fluidRow(
                                                                             column(1, offset = 0, h3("Distance"), align='left'),
                                                                             column(10, offset = 0, sliderInput("distanceSlider", width="100%", label = NULL, min = 0, max = 600, value = 300))
                                                                           ), br()
                                                                  ) ,
                                                                  # -----------------------------
                                                                  # C8
                                                                  tabPanel("Counties Most Hit", #C8
                                                                           tabsetPanel(id = "chartAndTable_tabs",
                                                                                       tabPanel("Chart", 
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c8_state1 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c8_state1")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c8_state2 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c8_state2"))
                                                                                                )
                                                                                       ),
                                                                                       tabPanel("Table",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c8_state1_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c8_state1_table")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c8_state2_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c8_state2_table"))
                                                                                                )
                                                                                       )
                                                                           )
                                                                  )
                                                           )),
                                                         br(),
                                                         fluidRow(
                                                           tabBox(title = "Injuries, Fatalities, Loss (summed over all years)", side = "left", width = 12,
                                                                  
                                                                  # -----------------------------
                                                                  # C6
                                                                  tabPanel("Per Month", #C6
                                                                           tabsetPanel(id = "chartAndTable_tabs",
                                                                                       tabPanel("Chart", 
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c6_state1 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c6_state1")), #, height = "80vh"
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c6_state2 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c6_state2"))
                                                                                                )
                                                                                       ),
                                                                                       tabPanel("Table", 
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c6_state1_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c6_state1_table")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c6_state2_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c6_state2_table"))
                                                                                                )
                                                                                       )
                                                                           )
                                                                           
                                                                  ),
                                                                  
                                                                  # -----------------------------
                                                                  # C7
                                                                  tabPanel("Per Hour", #C7
                                                                           tabsetPanel(id = "chartAndTable_tabs",
                                                                                       tabPanel("Chart", 
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c7_state1 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c7_state1")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c7_state2 {min-height:30vh !important;}"),
                                                                                                         plotlyOutput("c7_state2"))
                                                                                                )
                                                                                       ),
                                                                                       tabPanel("Table",
                                                                                                fluidRow(
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c7_state1_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c7_state1_table")),
                                                                                                  column(6,
                                                                                                         tags$style(type = "text/css", "#c7_state2_table {min-height:30vh !important;}"),
                                                                                                         DTOutput("c7_state2_table"))
                                                                                                )
                                                                                       )
                                                                           )
                                                                  )    
                                                           )
                                                         )
                                                         #tags$p("Ready to take the Shiny tutorial? If so"),
                                                         #tags$a(href = "shiny.rstudio.com/tutorial", "Click Here!")
                                                )))
  
                       ),
  
                       column(8, offset = 0, align = 'justify',
  # -----------------------------
  # C9
                              wellPanel(
                              fluidRow(column(6, offset = 0, align = 'justify',
                                              tags$style(type = "text/css", "#c9_state1_map {min-height:74vh !important;z-index:5 !important;}"),
                                              leafletOutput("c9_state1_map")
                                       ),
                                       column(6, offset = 0, align = 'justify',
                                              tags$style(type = "text/css", "#c9_state2_map {min-height:74vh !important;z-index:5 !important;}"),
                                              leafletOutput("c9_state2_map")
                                       )
                              ))
  
                             
  
        
                                                                   
                       )
              )
              
  ),
  fluidRow(
    column(1, offset = 10, align = 'right', h2("Map Provider: ")),
    column(1, offset = 0, align = 'justify',
                  selectInput("mapProvider_Input", label = NULL, providers, width = "100%")
  )
  )
  ),
  
  # -----------------------------
  # Inputs
  
  column(2,
         tags$div(class = "padding1"),
         wellPanel(
           fluidRow(column(6, align = "left", h2("State 1 (left)")), column(6, align = "left", h2("State 2 (right)"))),
           br(),
           fluidRow(
             column(6, offset = 0, align = 'justify',
                    selectInput("state1_select", label = NULL, width = "100%",
                                choices = choices_states, selected = "(IL)	Illinois")
             ),
             column(6, offset = 0, align = 'justify',
                    selectInput("state2_select", label = NULL, width = "100%",
                                choices = choices_states, selected = "(KS)	Kansas")
             ))
         ),
         wellPanel(
           fluidRow(column(7, align = "right", h3("Ranges over:    ")), column(5, align = "left",  radioButtons("yearsRange_radio", label = NULL, inline = TRUE,
                                                                                                             choices = list("CURRENT year" = 0, "All years" = 1), 
                                                                                                             selected = 0) )),
           fluidRow(column(12, align = "justify", h2("Magnitudes"))),
           br(),
           fluidRow(column(10, offset = 1, align = "center", checkboxGroupInput("magnitudes_Input", label = NULL, inline = TRUE,
                              choices = list("UNKNOWN" = -9, "0" = 0, "1" = 1, "2" = 2, "3" = 3, "4" = 4, "5" = 5),
                              selected = c(0, 1, 2, 3, 4, 5)))),
           
           br(),
           
           fluidRow(column(12, align = "left", h2("Width"))),
           fluidRow(column(10, offset = 1, align = 'justify',
                           uiOutput("width_dSlider"))),
           
           br(), br(),
           
           fluidRow(column(12, align = "left", h2("Length"))),
           fluidRow(column(10, offset = 1, align = 'justify',
                           uiOutput("length_dSlider"))),
           
           br(), br(),
           
           fluidRow(column(12, align = "left", h2("Injuries"))),
           fluidRow(column(10, offset = 1, align = 'justify',
                           uiOutput("injuries_dSlider"))),
           
           br(), br(),
           
           fluidRow(column(12, align = "left", h2("Fatalities"))),
           fluidRow(column(10, offset = 1, align = 'justify',
                           uiOutput("fatalities_dSlider"))),
           
           br(), br(),
           
           fluidRow(column(1, align = "left", h2("Loss")), column(2, align = "left", h3("(in millions)"))),
           fluidRow(column(10, offset = 1, align = 'justify',
                           uiOutput("loss_dSlider"))),
           br(),
           fluidRow(column(12, align = "right", h5("Note: Entry of 0 does not mean $0")))

        ),
        
        fluidRow(
          
          column(12, align = "left",
                 
                 tags$div(id = "hide2",
                   wellPanel(id = "mapLayers_Panel",
                             br(),
                             fluidRow(column(3, align = "left", h2("Map Layers")),
                                      column(8, offset = 1, align = "left", checkboxGroupInput("mapLayers_Input", label = NULL, inline = TRUE,
                                                                                                  c("Tracks", "Counties", "SafeZone"),
                                                                                                  selected = c("Tracks")))
                             ), br()
                             
                             
                 ))
                 
          )
        ), 
        
        fluidRow(
          
          column(7, align = "left",
                 
            wellPanel(
              #fluidRow(column(4, offset = 0, align = "left", h2("Mapping")), column(6, align = "left", h2("Based On"))),
              fluidRow(column(12, offset = 0, align = "left", h2("Tracks  (mapping based on):"))),
              
              br(),
              fluidRow(column(3, offset = 1, align = "justify", checkboxGroupInput("mapping_Input", label = NULL, inline = FALSE,
                                                                choices = list("Color", "Size"),
                                                                selected = c())),
                       column(8, align = "justify", selectInput("basedOn1_Select", label = NULL, 
                                                                choices = list("Magnitude", "Width", "Length", "Injuries", "Fatalities", "Loss"), 
                                                                selected = "Magnitude"),
                                                    selectInput("basedOn2_Select", label = NULL, 
                                                                choices = list("Magnitude", "Width", "Length", "Injuries", "Fatalities", "Loss"), 
                                                                selected = "Magnitude")

                              )

                       )
            )),
          
          column(5, align = "left",
                 
                 tags$div(id = "hide",
                          wellPanel(id = "countiesSelect_Panel",
                                    fluidRow(column(12, align = "left", h2("Counties  (show):"))),
                                    br(),
                                    fluidRow(column(11, offset = 1, align = "justify",selectInput("counties_Select", label = NULL,
                                                                                                  c("Tornadoes (magnitude)", "Fatalities", "Injuries", "Loss"), 
                                                                                                  selected = 1)    )
                                    ), br(), br(), br()
                                    
                          ))
                 
                 
          )
        
        
        
        
        )
        

         
         
         
         
         
         
  ),
  
  # -----------------------------
  # log
  
  column(1, offset = 11, align = 'right',
         dropdownButton(
           verbatimTextOutput("logText"),
           circle = TRUE, label = "Log", status = "default", icon = icon("list", "fa-1x"), width = "1000px",
           tooltip = FALSE
         )
  ),
  
  
  # -----------------------------
  # C10, C11
  
  bsModal(id = "modal_Settings", h2("Settings"), trigger = "action_Settings", size = "large",
          h2("Time"), 
          fluidRow(column(11, offset = 1, checkboxInput("hourFormat_checkBox", label = "24h format", value = FALSE))),
          h2("Measurement System"), 
          fluidRow(column(11, offset = 1, radioButtons("mSystem_radio", label = NULL,
                                                       choices = list("Metric" = 1, "Imperial" = 2), 
                                                       selected = 2)))),
  bsModal(id = "modal_About", h2("About"), trigger = "action_About", size = "large",
          h2("Project 3 - CS 424", align = "center"),
          h2("You Spin Me Round", align = "center"), br(),
          h3("By: Peter Hanula (phanul2), Dimitar Kirilov (dkiril4), Konrad Biegaj (kbiega2)", align = "center"), br(),
          h2("DATA INFORMATION", align = "right"),
          h3("The data for this project comes from ...", align = "justify"),
          h3("Direct link: ..."), br(),
          h2("APPLICATION INFORMATION", align = "right"),
          h3("put stuff here...", align ="justify"), br(),
          h2("LIBRARIES USED", align = "right"),
          
          fluidRow(column(4, align="center", h3("shiny")),column(4, align="center", h3("lubridate")),column(4, align="center", h3("xts"))),
          fluidRow(column(4, align="center", h3("shinyBS")),column(4, align="center", h3("ggplot2")),column(4, align="center", h3("geojsonio"))),
          fluidRow(column(4, align="center", h3("shinythemes")),column(4, align="center", h3("reshape2")),column(4, align="center", h3("tidyr"))),
          fluidRow(column(4, align="center", h3("shinydashboard")),column(4, align="center", h3("DT")),column(4, align="center", h3("streamgraph"))),
          fluidRow(column(4, align="center", h3("shinyWidgets")),column(4, align="center", h3("leaflet")),column(4, align="center", h3("plotly"))),
          fluidRow(column(4, align="center", h3("plyr")),column(4, align="center", h3("leaflet.minicharts")),column(4, align="center", h3("dygraphs"))),
          fluidRow(column(4, align="center", h3("dplyr")),column(4, align="center", h3("zoo")),column(4, align="center", h3("")))
  )
)
