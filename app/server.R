
# Define Server
# -----------------------------

allTornadoes <- read.csv(file = "data/all_tornadoes.csv", header = TRUE)
allTornadoes$timestamp <- as.POSIXct(paste(allTornadoes$date, allTornadoes$time), format="%Y-%m-%d %H:%M:%S")

allStatesLatLng <- read.csv(file = "data/all_states_lat_lng.csv", header = TRUE)

stateBounds <- read.csv(file = "data/state_Bounds.csv", header = TRUE)

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
  
  getWidthLower <- reactive({input$width_dSlider[1]})
  getWidthUpper <- reactive({input$width_dSlider[2]})
  
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
  
  observeEvent(input$action_AddCharts, {
    #toggle("extraCharts", anim = TRUE, time = 1, animType = "fade")  # toggle is a shinyjs function
    toggle("extraCharts")  # toggle is a shinyjs function
  })
  
  # -----------------------------
  # Leaflet
  
  states <- geojsonio::geojson_read("data/states.geojson", what = "sp")
  states <- sp::merge(states, stateBounds, by = "STUSPS", duplicateGeoms = TRUE)
  
  # -----------------------------
  # Input Controls
  
  output$width_dSlider <- renderUI({
    
    #mapData1 <- filter(allTornadoes, st == getState1(), yr == getYearAsNum())
    #mapData2 <- filter(allTornadoes, st == getState2(), yr == getYearAsNum())
    
    mapData1 <- filter(allTornadoes, st == getState1())
    mapData2 <- filter(allTornadoes, st == getState2())

    maxState1 <- max(mapData1$wid, na.rm = TRUE)
    minState1 <- min(mapData1$wid, na.rm = TRUE)
    
    maxState2 <- max(mapData2$wid, na.rm = TRUE)
    minState2 <- min(mapData2$wid, na.rm = TRUE)
    
    sliderInput(inputId = "width_dSlider", label = NULL, width = "100%", post = NULL, step = 1,
                min = min(minState1, minState2, na.rm = TRUE), max = max(maxState1, maxState2, na.rm = TRUE), value = c(min, max))
    
    # startDate <- as.Date(paste(substr(input$slider_month, 2, 3), '01-2017', sep = "-"), "%m-%d-%Y")
    # 
    # choices_day <- format(seq.Date(from = startDate, length.out = as.numeric(days_in_month(startDate)), by="day"), "(%d)  %a")
    # 
    # if(!is.null(input$slider_day)) {
    #   
    #   dateToShow <- getDate()
    # }
    # 
    # message(dateToShow)
    # 
    # sliderTextInput(
    #   inputId = "slider_day",
    #   label = NULL, width = '100%', grid = TRUE, force_edges = TRUE, hide_min_max = TRUE,
    #   choices = choices_day, selected = choices_day[day(mdy(dateToShow))]
    # )
  })
  
  # -----------------------------
  
  # use input$MAPID_bounds and input$MAPID_zoom
  # set up observers and proxyleaflet
  
  state1_map_track_data <- reactive({
    
    mapData <- filter(allTornadoes, st == getState1(), yr == getYearAsNum())
    #mapData <- filter(mapData, wid >= getWidthLower() & wid <= getWidthUpper())
    colnames(mapData)[8] <- "STUSPS"
    stateSelected <- subset(states, STUSPS == getState1())
    mapData <- sp::merge(stateSelected, mapData, by = "STUSPS", duplicateGeoms = TRUE)
    #mapData <- subset(mapData, STUSPS == getState1())
    
    return(mapData)
  })
  
  state2_map_track_data <- reactive({
    mapData <- filter(allTornadoes, st == getState2(), yr == getYearAsNum())
    #mapData <- filter(mapData, wid >= input$width_dSlider[1] & wid <= input$width_dSlider[2])
    colnames(mapData)[8] <- "STUSPS"
    stateSelected <- subset(states, STUSPS == getState2())
    mapData <- sp::merge(stateSelected, mapData, by = "STUSPS", duplicateGeoms = TRUE)
    #mapData <- subset(mapData, STUSPS == getState1())
    
    return(mapData)
  })
  
  
  
  # c9 state 1 leaflet
  output$c9_state1_map <- renderLeaflet({ # initialize map
    
    # initialize map to default dropdown state value
    initState <- filter(stateBounds, STUSPS == "IL")
    
    rightB <- initState$right_bound
    bottomB <- initState$bottom_bound
    leftB <- initState$left_bound
    topB <- initState$top_bound
    
    leaflet(options = leafletOptions(minZoom = 7, maxZoom = 16)) %>%
      setView(lng = (rightB + leftB) / 2.0, lat = (bottomB + topB) / 2.0, zoom = 8) %>%
      setMaxBounds(rightB, bottomB, leftB, topB) %>%
      addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
                       options = providerTileOptions(noWrap = TRUE))
  })
  
  # Incremental changes to the map should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  observe({ # set view and max bounds
    
    activeState <- subset(states, STUSPS == getState1())
    
    rightB <- activeState@data$right_bound
    bottomB <- activeState@data$bottom_bound
    leftB <- activeState@data$left_bound
    topB <- activeState@data$top_bound
    
    proxy <- leafletProxy("c9_state1_map")
    
    # set view and max Bounds
    proxy %>% setView(lng = (rightB + leftB) / 2.0, lat = (bottomB + topB) / 2.0, zoom = 8) %>% 
              setMaxBounds(rightB, bottomB, leftB, topB)
  })
  
  observe({ # track data,
    
    mapData = state1_map_track_data()
    mapData <- subset(mapData, wid >= input$width_dSlider[1] & wid <= input$width_dSlider[2])
    
    #mapData <- subset(mapData, wid >= getWidthLower() & wid <= getWidthUpper())
    
    activeState <- subset(states, STUSPS == getState1())

    
    lat_start <- mapData@data$slat
    lat_end <- mapData@data$elat
    
    lon_start <- mapData@data$slon
    lon_end <- mapData@data$elon
    
    #state <- toString(mapData@data$STUSPS[1])
    
    # get map and clear shapes
    proxy <- leafletProxy("c9_state1_map", data = mapData) %>% clearShapes()
    
    # outline of all states
    proxy %>% addPolygons(data = states, weight = 3, opacity = .5, color = "black", fillOpacity = 0.0, fillColor = "black")
    
    # active state outline and fill
    proxy %>% addPolygons(data = activeState, weight = 6, opacity = 1, color = "black", fillOpacity = 0.1, fillColor = "black")
    
    if (length(lat_start) > 0) {
    for(i in 1:length(lat_start)){
      
      if ((!(lat_start[i] == 0.0 | lat_end[i] == 0.0 | lon_start[i] == 0.0 | lon_end[i] == 0.0))) {
        
          proxy <- addPolylines(proxy, lat = c(lat_start[i],lat_end[i]),
                                       lng = c(lon_start[i],lon_end[i]),
                                weight = 20, opacity = 0.35, color = "#FF5600")
      }
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
  
  #outputOptions(output,"c9_state1_map",suspendWhenHidden=FALSE) # causes errors in console? don't use?
  
  # c9 state 2 leaflet
  output$c9_state2_map <- renderLeaflet({ # initialize map to default dropdown state value
    
    m <- leaflet(states, options = leafletOptions(minZoom = 7, maxZoom = 16)) %>%
      setView(lng = (-87.4 + -91.6) / 2.0, lat = (36.7 + 42.6) / 2.0, zoom = 8) %>%
      #fitBounds(-87.4, 36.7, -91.6, 42.6) %>%
      setMaxBounds(-87.4, 36.7, -91.6, 42.6) %>%
      addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
                       options = providerTileOptions(noWrap = TRUE))
    
    m %>% addPolygons(weight = 5, opacity = 1, color = "black", fillOpacity = .1)
  })
  
  # overview leaflet
  output$overview_map <- renderLeaflet({ 
    
    m <- leaflet(states, options = leafletOptions(minZoom = 6, maxZoom = 16)) %>%
      setView(lng = -96, lat = 37.8, zoom = 6) %>%
      #fitBounds(-87.4, 36.7, -91.6, 42.6) %>%
      #setMaxBounds(-87.4, 36.7, -91.6, 42.6) %>%
      addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
                       options = providerTileOptions(noWrap = TRUE))
    
    m %>% addPolygons(weight = 5, opacity = .5, color = "black", fillOpacity = .1)
  })
  
  #-----------------------
  
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
  
  getMaxDistanceBetweenStates <- function(dist, state2)
  {
    
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
    #tornadoes <- allTornadoes
    
    if (getHourFormat())
    {
      hours24 <- as.data.frame(c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"))
      
      c3 <- allTornadoes %>% dplyr::filter(st == state) %>%
        group_by(Hour = format(strptime(time, "%H:%M:%S"), format="%H:%00"), Magnitude = mag) %>% 
        summarise(Count = n()) %>% 
        mutate(Percent = (Count / sum(Count) * 100))
      
      c3$Hour <- ordered(c3$Hour, levels = hours24[,])
    }
    else
    {
      hours12 <- as.data.frame(c("12:00 AM", "01:00 AM", "02:00 AM", "03:00 AM", "04:00 AM", "05:00 AM", "06:00 AM", "07:00 AM", "08:00 AM", "09:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "01:00 PM", "02:00 PM", "03:00 PM", "04:00 PM", "05:00 PM", "06:00 PM", "07:00 PM", "08:00 PM", "09:00 PM", "10:00 PM", "11:00 PM"))
      
      c3 <- allTornadoes %>% dplyr::filter(st == state) %>%
        group_by(Hour = format(strptime(time, "%H:%M:%S"), format="%I:%00 %p"), Magnitude = mag) %>% 
        summarise(Count = n()) %>% 
        mutate(Percent = (Count / sum(Count) * 100))
      
      c3$Hour <- ordered(c3$Hour, levels = hours12[,])
    }

    
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
  
  # -----------------------------
  
  
  
  
  
  
 
}
