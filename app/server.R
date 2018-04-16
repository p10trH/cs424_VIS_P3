
# Define Server
# -----------------------------

allTornadoes <- read.csv(file = "data/all_tornadoes.csv", header = TRUE)
allTornadoes$timestamp <- as.POSIXct(paste(allTornadoes$date, allTornadoes$time), format="%Y-%m-%d %H:%M:%S")

allStatesLatLng <- read.csv(file = "data/all_states_lat_lng.csv", header = TRUE)

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
  
  # Dimitar----
  textSize <- 15
  
  plotTheme <- theme(plot.margin= unit(c(1,1,1.5,1), "cm"),
                     plot.background = element_rect(fill = "#AAAAAA"),
                     legend.background = element_rect(fill = "#AAAAAA"),
                     axis.text = element_text(size = textSize),
                     axis.title = element_text(size = textSize),
                     legend.text = element_text(size = textSize))
  
  getStateLat <- function(state)
  {
    allStatesLatLng %>% filter(Code == state) %>% dplyr::select(Latitude) 
  }
  
  getStateLng <- function(state)
  {
    allStatesLatLng %>% filter(Code == state) %>% dplyr::select(Longitude) 
  }
  
  # Data
  c1Data <- function(state)
  {
    c1 <- allTornadoes %>% dplyr::filter(st == state) %>%
      group_by(Year = yr, Magnitude = mag) %>% 
      summarise(Count = n()) %>% 
      mutate(Percent = (Count / sum(Count) * 100))
    
    c1$Percent <- format(round(c1$Percent, 2), nsmall = 2)
    c1$Percent <- paste0(c1$Percent, "%")
    
    
    c1 <- dplyr::arrange(c1, Year, -Magnitude)
    
    c1$Magnitude <- factor(c1$Magnitude)
    
    return(c1)
  }
  
  c2Data <- function(state)
  {
    c2 <- allTornadoes %>% dplyr::filter(st == state) %>%
      group_by(Month = mo, Magnitude = mag) %>% 
      summarise(Count = n()) %>% 
      mutate(Percent = (Count / sum(Count) * 100))
    
    c2$Percent <- format(round(c2$Percent, 2), nsmall = 2)
    c2$Percent <- paste0(c2$Percent, "%")
    
    c2 <- dplyr::arrange(c2, Month, -Magnitude)
    
    c2$Magnitude <- factor(c2$Magnitude)
    
    return(c2)
  }
  
  c3Data <- function(state)
  {
    c3 <- allTornadoes %>% dplyr::filter(st == state) %>%
      group_by(Hour = hour(timestamp), Magnitude = mag) %>% 
      summarise(Count = n()) %>% 
      mutate(Percent = (Count / sum(Count) * 100))
    
    c3$Percent <- format(round(c3$Percent, 2), nsmall = 2)
    c3$Percent <- paste0(c3$Percent, "%")
    
    c3 <- dplyr::arrange(c3, Hour, -Magnitude)
    
    c3$Magnitude <- factor(c3$Magnitude)
    
    return(c3)
  }
  
  c4Data <- function(state)
  {
    tornadoes <- allTornadoes
    tornadoes <- tornadoes %>% mutate(distanceFromState = distHaversine(cbind(getStateLng(state), getStateLat(state)), cbind(slon, slat)) / 1000)
    
    # Metric
    if (input$mSystem_radio == 1)
    {
      c4 <- tornadoes %>% filter(tornadoes$distanceFromState < input$distanceSlider) %>%
        group_by(Year = yr, Magnitude = mag) %>%
        summarise(Count = n()) %>%
        mutate(Percent = (Count / sum(Count) * 100))
    }
    # Imperial
    else
    {
      c4 <- tornadoes %>% filter((tornadoes$distanceFromState * 0.621371) < input$distanceSlider) %>%
        group_by(Year = yr, Magnitude = mag) %>%
        summarise(Count = n()) %>%
        mutate(Percent = (Count / sum(Count) * 100))
    }
    
    c4$Percent <- format(round(c4$Percent, 2), nsmall = 2)
    c4$Percent <- paste0(c4$Percent, "%")
    
    c4 <- dplyr::arrange(c4, Year, -Magnitude)
    
    c4$Magnitude <- factor(c4$Magnitude)
    
    return(c4)
  }
  
  #State 1
  
  output$c1_state1 <- renderPlotly({
    
    c1 <- c1Data(getState1())
    
    ggplotly(ggplot(c1, aes(x = Year, 
                            y = Count, 
                            group = "Magnitude", 
                            fill = Magnitude,
                            text = paste0("Tornadoes: ", Count, " (", Percent, ")"))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_fill_brewer(type = "seq"), tooltip = c("x", "text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
  })
  
  output$c2_state1 <- renderPlotly({
    
    c2 <- c2Data(getState1())
    
    ggplotly(ggplot(c2, aes(x = Month, 
                            y = Count, 
                            group = "Magnitude", 
                            fill = Magnitude,
                            text = paste0("Tornadoes: ", Count, " (", Percent, ")"))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_x_continuous(breaks = round(seq(1, 12, by = 1),1)) +
               scale_fill_brewer(type = "seq"), tooltip = c("text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
  })
  
  output$c3_state1 <- renderPlotly({
    
    c3 <- c3Data(getState1())
    
    ggplotly(ggplot(c3, aes(x = Hour, 
                            y = Count, 
                            group = "Magnitude", 
                            fill = Magnitude,
                            text = paste0("Tornadoes: ", Count, " (", Percent, ")"))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_x_continuous(breaks = round(seq(0, 23, by = 1),1)) +
               scale_fill_brewer(type = "seq"), tooltip = c("text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
  })
  
  output$c4_state1 <- renderPlotly({
    
    c4 <- c4Data(getState1())
    
    ggplotly(ggplot(c4, aes(x = Year,
                            y = Count,
                            group = "Magnitude",
                            fill = Magnitude,
                            text = paste0("Tornadoes: ", Count, " (", Percent, ")"))) +
               geom_bar(stat = "identity") +
               plotTheme + 
               scale_fill_brewer(type = "seq"), tooltip = c("x", "text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
  })
  
  output$c1_state1_table <- renderDT({
    c1 <- c1Data(getState1())
    
    datatable(c1, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  output$c2_state1_table <- renderDT({
    c2 <- c2Data(getState1())
    
    datatable(c2, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  output$c3_state1_table <- renderDT({
    c3 <- c3Data(getState1())
    
    datatable(c3, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  output$c4_state1_table <- renderDT({
    c4 <- c4Data(getState1())
    
    datatable(c4, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  #State 2
  output$c1_state2 <- renderPlotly({
    
    c1 <- c1Data(getState2())
    
    ggplotly(ggplot(c1, aes(x = Year, 
                            y = Count, 
                            group = "Magnitude", 
                            fill = Magnitude,
                            text = paste0("Tornadoes: ", Count, " (", Percent, ")"))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_fill_brewer(type = "seq"), tooltip = c("x", "text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
  })
  
  output$c2_state2 <- renderPlotly({
    
    c2 <- c2Data(getState2())
    
    ggplotly(ggplot(c2, aes(x = Month, 
                            y = Count, 
                            group = "Magnitude", 
                            fill = Magnitude,
                            text = paste0("Tornadoes: ", Count, " (", Percent, ")"))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_x_continuous(breaks = round(seq(1, 12, by = 1),1)) +
               scale_fill_brewer(type = "seq"), tooltip = c("text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
  })
  
  output$c3_state2 <- renderPlotly({
    
    c3 <- c3Data(getState2())
    
    ggplotly(ggplot(c3, aes(x = Hour, 
                            y = Count, 
                            group = "Magnitude", 
                            fill = Magnitude,
                            text = paste0("Tornadoes: ", Count, " (", Percent, ")"))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_x_continuous(breaks = round(seq(0, 23, by = 1),1)) +
               scale_fill_brewer(type = "seq"), tooltip = c("text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
  })
  
  output$c4_state2 <- renderPlotly({
    
    c4 <- c4Data(getState2())
    
    ggplotly(ggplot(c4, aes(x = Year,
                            y = Count,
                            group = "Magnitude",
                            fill = Magnitude,
                            text = paste0("Tornadoes: ", Count, " (", Percent, ")"))) +
               geom_bar(stat = "identity") +
               plotTheme + 
               scale_fill_brewer(type = "seq"), tooltip = c("x", "text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))
  })
  
  output$c1_state2_table <- renderDT({
    c1 <- c1Data(getState2())
    
    datatable(c1, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  output$c2_state2_table <- renderDT({
    c2 <- c2Data(getState2())
    
    datatable(c2, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  output$c3_state2_table <- renderDT({
    c3 <- c3Data(getState2())
    
    datatable(c3, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  output$c4_state2_table <- renderDT({
    c4 <- c4Data(getState2())
    
    datatable(c4, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  #------------
  
  
  
  
  
  
 
}
