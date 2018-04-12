
# Define UI
# -----------------------------

# 1950 - 2017
choices_years <- format(seq.Date(from = as.Date('1-1-1950', "%m-%d-%Y"), to = as.Date('12-31-2017', "%m-%d-%Y"), by="year"), "%Y")

# month, day, year ("%m-%d-%Y")
dateToShow <- "6-9-1992"

# states
choices_states <- c(
  "(AL)	Alabama",
  "(AK)	Alaska",
  "(AZ)	Arizona",
  "(AR)	Arkansas",
  "(CA)	California",
  "(CO)	Colorado",
  "(CT)	Connecticut",
  "(DE)	Delaware",
  "(FL)	Florida",
  "(GA)	Georgia",
  "(HI)	Hawaii",
  "(ID)	Idaho",
  "(IL)	Illinois",
  "(IN)	Indiana",
  "(IA)	Iowa",
  "(KS)	Kansas",
  "(KY)	Kentucky",
  "(LA)	Louisiana",
  "(ME)	Maine",
  "(MD)	Maryland",
  "(MA)	Massachusetts",
  "(MI)	Michigan",
  "(MN)	Minnesota",
  "(MS)	Mississippi",
  "(MO)	Missouri",
  "(MT)	Montana",
  "(NE)	Nebraska",
  "(NV)	Nevada",
  "(NH)	New Hampshire",
  "(NJ)	New Jersey",
  "(NM)	New Mexico",
  "(NY)	New York",
  "(NC)	North Carolina",
  "(ND)	North Dakota",
  "(OH)	Ohio",
  "(OK)	Oklahoma",
  "(OR)	Oregon",
  "(PA)	Pennsylvania",
  "(RI)	Rhode Island",
  "(SC)	South Carolina",
  "(SD)	South Dakota",
  "(TN)	Tennessee",
  "(TX)	Texas",
  "(UT)	Utah",
  "(VT)	Vermont",
  "(VA)	Virginia",
  "(WA)	Washington",
  "(WV)	West Virginia",
  "(WI)	Wisconsin",
  "(WY)	Wyoming"
)

# -----------------------------
ui <- fluidPage(
  
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
                    animate=animationOptions(interval = 5000, loop = FALSE, playButton = NULL, pauseButton= NULL)
                  )),
      
           column(2, offset = 0, align = 'right',
                  h1("You Spin Me Round"),
                  actionButton("action_Settings", label = "", icon = icon("cog", "fa-3x")),
                  actionButton("action_About", label = "", icon = icon("info", "fa-3x")))),
  br(),
  
  # -----------------------------
  # Main Navigation
  
  tabsetPanel(id = "mainNav",
              
  # -----------------------------
  # Overview
              
              tabPanel("Overview", br()),
              
  # -----------------------------
  # Explore Further
                       
              tabPanel("Explore Further", br(),
                       fluidRow(
                         column(3, offset = 6, align = 'justify',
                                selectInput("state1_select", label = NULL, width = "100%",
                                            choices = choices_states, selected = "(IL)	Illinois")
                         ),
                         column(3, offset = 0, align = 'justify',
                                selectInput("state2_select", label = NULL, width = "100%",
                                            choices = choices_states, selected = "(KS)	Kansas")
                         )
                       ),
                       column(6, offset = 0, align = 'justify',
                              fluidRow(
                                tabBox(title = "", side = "left", width = 6,
                                       
  # -----------------------------
  # C1
                                       tabPanel("Tornadoes", #C1
                                                tabsetPanel(id = "chartAndTable_tabs",
                                                            tabPanel("Chart", 
                                                                     fluidRow(
                                                                       column(6,
                                                                              tags$style(type = "text/css", "#c1_state1 {min-height:30vh !important;}"),
                                                                              plotlyOutput("c1_state1")), #, height = "80vh"
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
                                       
                                ),
                                
                                tabBox(title = "", side = "left", width = 6,
  
  # -----------------------------
  # C5
                                       tabPanel("Injuries, Fatalities, Loss", #C5
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
                                  )),
  
                            fluidRow(
                                tabBox(title = "Tornadoes (summed over all years)", side = "left", width = 6,
                                       
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
                                                )
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
                                ),
                             
                           
                                tabBox(title = "Injuries, Fatalities, Loss (summed over all years)", side = "left", width = 6,
                                       
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
                                ))
  
                       ),
                       column(6, offset = 0, align = 'justify')
              )
              
  )
  
  
  

)
