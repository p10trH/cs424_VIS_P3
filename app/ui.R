
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
  
  fluidRow(column(8, offset = 2, align = 'justify',
                  sliderTextInput(inputId = "slider_years", 
                    label = NULL, width = '100%', grid = TRUE, force_edges = TRUE, hide_min_max = TRUE,
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
                       column(3),
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
                                                                  tabPanel("Distance From Chicago", #C4
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
              
  )),
  
  column(2,
         tags$div(class = "padding1"),
         wellPanel(
         fluidRow(column(6,h2("State 1 (left)")), column(6,h2("State 2 (right)"))),
         fluidRow(
           column(6, offset = 0, align = 'justify',
                  selectInput("state1_select", label = NULL, width = "100%",
                              choices = choices_states, selected = "(IL)	Illinois")
           ),
           column(6, offset = 0, align = 'justify',
                  selectInput("state2_select", label = NULL, width = "100%",
                              choices = choices_states, selected = "(KS)	Kansas")
           )
         ))),
  
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
