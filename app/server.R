
# Define Server
# -----------------------------


# -----------------------------
server <- function(input, output) {
  
  # Get year
  getYearAsNum <- reactive({as.numeric(input$slider_years)})
  getYearAsStr <- reactive({toString(input$slider_years)})
  
  # Get state abbreviation from first (left) dropdown
  getState1 <- reactive({substr(input$state1_select, 2, 3)})
  
  # Get state abbreviation from second (right) dropdown
  getState2 <- reactive({substr(input$state2_select, 2, 3)})
  
  # Get hour format; FALSE = 12 hour AM/PM, TRUE = 24 hour
  getHourFormat <- reactive({input$hourFormat_checkBox})
  
  # Get System of Measurement; 1 = Metric, 2 = Imperial
  getMSystem <- reactive({input$mSystem_radio})
  
  output$logText <- renderPrint({
    print("Year as Num:")
    print(getYearAsNum())
    print("Year as Str:")
    print(getYearAsStr())
    print("State 1:")
    print(getState1())
    print("State 2:")
    print(getState2())
    print("Hour Format:")
    print(getHourFormat())
    print("Meas System:")
    print(getMSystem())
  })
  
  # -----------------------------
  
  states <- geojsonio::geojson_read("data/states.geojson", what = "sp")
  
  # c9 state 1 leaflet
  output$c9_state1_map <- renderLeaflet({ 
    
    m <- leaflet(states, options = leafletOptions(minZoom = 7, maxZoom = 16)) %>%
      setView(lng = (-87.4 + -91.6) / 2.0, lat = (36.7 + 42.6) / 2.0, zoom = 8) %>%
      #fitBounds(-87.4, 36.7, -91.6, 42.6) %>%
      setMaxBounds(-87.4, 36.7, -91.6, 42.6) %>%
      addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
                       options = providerTileOptions(noWrap = TRUE))
    
    m %>% addPolygons(weight = 5, opacity = 1, color = "black", fillOpacity = .1)
  })
  
  # c9 state 2 leaflet
  output$c9_state2_map <- renderLeaflet({ 
    
    m <- leaflet(states, options = leafletOptions(minZoom = 7, maxZoom = 16)) %>%
      setView(lng = (-87.4 + -91.6) / 2.0, lat = (36.7 + 42.6) / 2.0, zoom = 8) %>%
      #fitBounds(-87.4, 36.7, -91.6, 42.6) %>%
      setMaxBounds(-87.4, 36.7, -91.6, 42.6) %>%
      addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
                       options = providerTileOptions(noWrap = TRUE))
    
    m %>% addPolygons(weight = 5, opacity = 1, color = "black", fillOpacity = .1)
  })
  
  # -----------------------------
  
  
  
  
  
  
  
  
 
}
