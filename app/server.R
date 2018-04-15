
# Define Server
# -----------------------------

allTornadoes <- read.csv(file = "data/all_tornadoes.csv", header = TRUE)

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
  
  # use input$MAPID_bounds and input$MAPID_zoom
  # set up observers and proxyleaflet
  
  state1_map_data <- reactive({
    mapData <- filter(allTornadoes, st == getState1(), yr == getYearAsNum())
    colnames(mapData)[8] <- "STUSPS"
    states <- subset(states, STUSPS == getState1())
    mapData <- sp::merge(states, mapData, by = "STUSPS", duplicateGeoms = TRUE)
    #mapData <- subset(mapData, STUSPS == getState1())
  })
  
  # c9 state 1 leaflet
  output$c9_state1_map <- renderLeaflet({ 
    
    #m <- leaflet(states, options = leafletOptions(minZoom = 7, maxZoom = 16)) %>%
    leaflet(options = leafletOptions(minZoom = 7, maxZoom = 16)) %>%
      setView(lng = (-87.4 + -91.6) / 2.0, lat = (36.7 + 42.6) / 2.0, zoom = 8) %>%
      #fitBounds(-87.4, 36.7, -91.6, 42.6) %>%
      setMaxBounds(-87.4, 36.7, -91.6, 42.6) %>%
      addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
                       options = providerTileOptions(noWrap = TRUE))
  })
  
  # Incremental changes to the map should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  observe({
    
    mapData = state1_map_data()

    lat_start <- mapData@data$slat
    lat_end <- mapData@data$elat
    
    lon_start <- mapData@data$slon
    lon_end <- mapData@data$elon
    
    state <- toString(mapData@data$STUSPS[1])
    
    proxy <- leafletProxy("c9_state1_map", data = mapData) %>% clearShapes() %>%
      addPolygons(weight = 3, opacity = 1, color = "black", fillOpacity = 0.0) # %>% this is redundant and draws the state multiple times
      
    for(i in 1:length(lat_start)){
      
      if (!(lat_start[i] == 0.0 | lat_end[i] == 0.0 | lon_start[i] == 0.0 | lon_end[i] == 0.0)) {
        
          proxy <- addPolylines(proxy, lat = c(lat_start[i],lat_end[i]),
                                       lng = c(lon_start[i],lon_end[i]),
                                weight = 20, opacity = 0.35, color = "#FF5600")
      }
    }
    
    proxy %>%
      # start
      addCircles(
        lng = ~mapData@data$slon,
        lat = ~mapData@data$slat,
        weight = 5, fillOpacity = 0.0, radius = 2000,
        color = "#FF5600",
        label = paste("Start"),
        labelOptions = labelOptions(style = list(
          "padding" = "10px",
          "font-size" = "35px"
      ))) %>%
      # end
      addCircles(
      lng = ~mapData@data$elon,
      lat = ~mapData@data$elat,
      weight = 5, fillOpacity = 0.7, radius = 3500,
      color = "#FF5600",
      label = paste("End"),
      labelOptions = labelOptions(style = list(
        "padding" = "10px",
        "font-size" = "35px"
      )))
    
  })
  
  #outputOptions(output,"c9_state1_map",suspendWhenHidden=FALSE)
  
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
