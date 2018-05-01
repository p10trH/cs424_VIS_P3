
# Define Server
# -----------------------------

load("data/allTornadoes_Final.RData")
stateFips <- read.csv(file = "data/US_FIPS_Codes.csv", header = TRUE)

allStatesLatLng <- read.csv(file = "data/all_states_lat_lng.csv", header = TRUE)

stateBounds <- read.csv(file = "data/state_Bounds.csv", header = TRUE)
counties <- readOGR("data/shpf/cb_2017_us_county_20m.shp", layer = "cb_2017_us_county_20m", stringsAsFactors = FALSE)


# -----------------------------
server <- function(input, output, session) {
  
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
  
  # Get magnitudes for maps
  getMagnitudes <- reactive({input$magnitudes_Input})
  
  # Get zoom State 1
  getZoomState1 <- reactive({input$c9_state1_map_zoom})
  
  # Get zoom State 2
  getZoomState2 <- reactive({input$c9_state2_map_zoom})
  
  # Get width slider for maps
  getWidthLower <- reactive({input$width_dSlider[1]})
  getWidthUpper <- reactive({input$width_dSlider[2]})
  
  # Get length slider for maps
  getLengthLower <- reactive({input$length_dSlider[1]})
  getLengthUpper <- reactive({input$length_dSlider[2]})
  
  # Get injuries slider for maps
  getInjuriesLower <- reactive({input$injuries_dSlider[1]})
  getInjuriesUpper <- reactive({input$injuries_dSlider[2]})
  
  # Get fatalities slider for maps
  getFatalitiesLower <- reactive({input$fatalities_dSlider[1]})
  getFatalitiesUpper <- reactive({input$fatalities_dSlider[2]})
  
  # Get loss slider for maps
  getLossLower <- reactive({(input$loss_dSlider[1] * 1000000)})
  getLossUpper <- reactive({(input$loss_dSlider[2] * 1000000)})
  
  # Get county option for maps
  getCountyOption <- reactive({input$counties_Select})
  
  # Get map layers
  getMapLayers <- reactive({input$mapLayers_Input})
  
  # Current or all years in sliders
  getYearCurrOrAll <- reactive({input$yearsRange_radio})
  
  # Get Map Provider
  getMapProvider <- reactive({input$mapProvider_Input})
  
  # Get Track Mappings
  getTrackMappings <- reactive({input$mapping_Input})
  
  
  
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
  
  observeEvent(input$mainNav, {
    #toggle("extraCharts", anim = TRUE, time = 1, animType = "fade")  # toggle is a shinyjs function
    #toggle("mapLayers_Panel")  # toggle is a shinyjs function
    toggle("countiesSelect_Panel")
    
    if (input$mainNav == "Overview") {
      runjs("var ele = document.getElementById(\"hide2\"); ele.style.opacity = \"0.2\";")
    } else
      runjs("var ele = document.getElementById(\"hide2\"); ele.style.opacity = \"1.0\";")
    
    #val <- getYearAsStr()
    
    #updateSliderInput(session, inputId = "slider_years", min = 1950, max = 2016, value = 2011, step = 1)
  })
  
  # -----------------------------
  
  observeEvent(input$mapLayers_Input, {
    #toggle("extraCharts", anim = TRUE, time = 1, animType = "fade")  # toggle is a shinyjs function
    if (!("Counties" %in% input$mapLayers_Input)) {
      runjs("var ele = document.getElementById(\"hide\"); ele.style.opacity = \"0.2\";")
    } else
      runjs("var ele = document.getElementById(\"hide\"); ele.style.opacity = \"1.0\";")
  })
  
  # -----------------------------
  # Input Controls
  
  output$width_dSlider <- renderUI({ # width
    
    selYear <- getYearAsNum()
    
    if (getYearCurrOrAll() == 0) {
      mapData1 <- filter(allTornadoes, st == getState1(), yr == selYear)
      mapData2 <- filter(allTornadoes, st == getState2(), yr == selYear)
    } else {
      mapData1 <- filter(allTornadoes, st == getState1())
      mapData2 <- filter(allTornadoes, st == getState2())
    }
    
    maxState1 <- max(mapData1$wid, na.rm = TRUE)
    minState1 <- min(mapData1$wid, na.rm = TRUE)
    
    maxState2 <- max(mapData2$wid, na.rm = TRUE)
    minState2 <- min(mapData2$wid, na.rm = TRUE)
    
    #sliderInput(inputId = "width_dSlider", label = NULL, width = "100%", post = " yds", step = 1,
    #            min = min(minState1, minState2, na.rm = TRUE), max = max(maxState1, maxState2, na.rm = TRUE), value = c(getWidthLower(), getWidthUpper()))
    
    sliderInput(inputId = "width_dSlider", label = NULL, width = "100%", post = " yds", step = 1, round = TRUE,
                min = min(minState1, minState2, na.rm = TRUE), max = max(maxState1, maxState2, na.rm = TRUE), value = c(min, max))
  })
  
  output$length_dSlider <- renderUI({
    
    selYear <- getYearAsNum()
    
    if (getYearCurrOrAll() == 0) {
      mapData1 <- filter(allTornadoes, st == getState1(), yr == selYear)
      mapData2 <- filter(allTornadoes, st == getState2(), yr == selYear)
    } else {
      mapData1 <- filter(allTornadoes, st == getState1())
      mapData2 <- filter(allTornadoes, st == getState2())
    }
    
    maxState1 <- max(mapData1$len, na.rm = TRUE)
    minState1 <- min(mapData1$len, na.rm = TRUE)
    
    maxState2 <- max(mapData2$len, na.rm = TRUE)
    minState2 <- min(mapData2$len, na.rm = TRUE)
    
    sliderInput(inputId = "length_dSlider", label = NULL, width = "100%", post = " mi", step = 1, round = TRUE,
                min = min(minState1, minState2, na.rm = TRUE), max = max(maxState1, maxState2, na.rm = TRUE), value = c(min, max))
  })
  
  output$injuries_dSlider <- renderUI({
    
    selYear <- getYearAsNum()
    
    if (getYearCurrOrAll() == 0) {
      mapData1 <- filter(allTornadoes, st == getState1(), yr == selYear)
      mapData2 <- filter(allTornadoes, st == getState2(), yr == selYear)
    } else {
      mapData1 <- filter(allTornadoes, st == getState1())
      mapData2 <- filter(allTornadoes, st == getState2())
    }
    
    maxState1 <- max(mapData1$inj, na.rm = TRUE)
    minState1 <- min(mapData1$inj, na.rm = TRUE)
    
    maxState2 <- max(mapData2$inj, na.rm = TRUE)
    minState2 <- min(mapData2$inj, na.rm = TRUE)
    
    sliderInput(inputId = "injuries_dSlider", label = NULL, width = "100%", post = NULL, step = 1,
                min = min(minState1, minState2, na.rm = TRUE), max = max(maxState1, maxState2, na.rm = TRUE), value = c(min, max))
  })
  
  output$fatalities_dSlider <- renderUI({
    
    selYear <- getYearAsNum()
    
    if (getYearCurrOrAll() == 0) {
      mapData1 <- filter(allTornadoes, st == getState1(), yr == selYear)
      mapData2 <- filter(allTornadoes, st == getState2(), yr == selYear)
    } else {
      mapData1 <- filter(allTornadoes, st == getState1())
      mapData2 <- filter(allTornadoes, st == getState2())
    }
    
    maxState1 <- max(mapData1$fat, na.rm = TRUE)
    minState1 <- min(mapData1$fat, na.rm = TRUE)
    
    maxState2 <- max(mapData2$fat, na.rm = TRUE)
    minState2 <- min(mapData2$fat, na.rm = TRUE)
    
    sliderInput(inputId = "fatalities_dSlider", label = NULL, width = "100%", post = NULL, step = 1,
                min = min(minState1, minState2, na.rm = TRUE), max = max(maxState1, maxState2, na.rm = TRUE), value = c(min, max))
  })
  
  output$loss_dSlider <- renderUI({
    
    selYear <- getYearAsNum()
    
    if (getYearCurrOrAll() == 0) {
      mapData1 <- filter(allTornadoes, st == getState1(), yr == selYear)
      mapData2 <- filter(allTornadoes, st == getState2(), yr == selYear)
    } else {
      mapData1 <- filter(allTornadoes, st == getState1())
      mapData2 <- filter(allTornadoes, st == getState2())
    }
    
    maxState1 <- max(mapData1$loss_updated, na.rm = TRUE)
    minState1 <- min(mapData1$loss_updated, na.rm = TRUE)
    
    maxState2 <- max(mapData2$loss_updated, na.rm = TRUE)
    minState2 <- min(mapData2$loss_updated, na.rm = TRUE)
    
    sliderInput(inputId = "loss_dSlider", label = NULL, width = "100%", post = NULL, pre = "$ ", step = 0.5,
                min = (min(minState1, minState2, na.rm = TRUE)/1000000), max = (max(maxState1, maxState2, na.rm = TRUE)/1000000), value = c(min, max))
  })
  
  # -----------------------------
  # Leaflet
  
  states <- geojsonio::geojson_read("data/states.geojson", what = "sp")
  states <- sp::merge(states, stateBounds, by = "STUSPS", duplicateGeoms = TRUE)
  
  # -----------------------------
  
  # try to filter data elsewhere so no double rendering?
  # state1_map_track_data <- reactive({
  #   
  #   #selYear <- getYearAsNum()
  #   
  #   mapData <- filter(allTornadoes, st == getState1(), yr == getYearAsNum())
  #   mapData <- filter(mapData, mag %in% getMagnitudes())
  #   #mapData <- filter(mapData, wid >= getWidthLower() & wid <= getWidthUpper())
  #   colnames(mapData)[8] <- "STUSPS"
  #   stateSelected <- subset(states, STUSPS == getState1())
  #   mapData <- sp::merge(stateSelected, mapData, by = "STUSPS", duplicateGeoms = TRUE)
  #   #mapData <- subset(mapData, STUSPS == getState1())
  #   
  #   return(mapData)
  # })
  
  # state2_map_track_data <- reactive({
  #   
  #   #selYear <- getYearAsNum()
  #   
  #   mapData <- filter(allTornadoes, st == getState2(), yr == getYearAsNum())
  #   mapData <- filter(mapData, mag %in% getMagnitudes())
  #   #mapData <- filter(mapData, wid >= input$width_dSlider[1] & wid <= input$width_dSlider[2])
  #   colnames(mapData)[8] <- "STUSPS"
  #   stateSelected <- subset(states, STUSPS == getState2())
  #   mapData <- sp::merge(stateSelected, mapData, by = "STUSPS", duplicateGeoms = TRUE)
  #   #mapData <- subset(mapData, STUSPS == getState1())
  #   
  #   return(mapData)
  # })
  
  # -----------------------------
  # c9 state 1 leaflet
  output$c9_state1_map <- renderLeaflet({ # initialize map
    
    # initialize map to default dropdown state value
    initState <- filter(stateBounds, STUSPS == "IL")
    
    rightB <- initState$right_bound
    bottomB <- initState$bottom_bound
    leftB <- initState$left_bound
    topB <- initState$top_bound
    
    leaflet(options = leafletOptions(minZoom = 7, maxZoom = 15)) %>%
      setView(lng = (rightB + leftB) / 2.0, lat = (bottomB + topB) / 2.0, zoom = 8) %>%
      setMaxBounds(rightB, bottomB, leftB, topB) %>%
      addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
                       options = providerTileOptions(noWrap = TRUE)) %>% hideGroup("Counties")%>%
      addMiniMap(
        tiles = providers$Stamen.TonerLite,
        toggleDisplay = TRUE,
        width = 400, height = 400,
        collapsedWidth = 50, collapsedHeight = 50,
        aimingRectOptions = list(color = "#333333", weight = 1, clickable = FALSE)) %>% 
      addMeasure(    position = "bottomleft",
                     primaryLengthUnit = "meters",
                     primaryAreaUnit = "sqmeters",
                     activeColor = "#3D535D",
                     completedColor = "#3D535D")
  })
  
  # c9 state 2 leaflet
  output$c9_state2_map <- renderLeaflet({ # initialize map
    
    # initialize map to default dropdown state value
    initState <- filter(stateBounds, STUSPS == "KS")
    
    rightB <- initState$right_bound
    bottomB <- initState$bottom_bound
    leftB <- initState$left_bound
    topB <- initState$top_bound
    
    leaflet(options = leafletOptions(minZoom = 7, maxZoom = 15)) %>%
      setView(lng = (rightB + leftB) / 2.0, lat = (bottomB + topB) / 2.0, zoom = 8) %>%
      setMaxBounds(rightB, bottomB, leftB, topB) %>%
      addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
                       options = providerTileOptions(noWrap = TRUE)) %>% hideGroup("Counties")%>%
      addMiniMap(
        tiles = providers$Stamen.TonerLite,
        toggleDisplay = TRUE,
        width = 400, height = 400,
        collapsedWidth = 50, collapsedHeight = 50,
        aimingRectOptions = list(color = "#333333", weight = 1, clickable = FALSE)) %>% 
      addMeasure(    position = "bottomleft",
                     primaryLengthUnit = "meters",
                     primaryAreaUnit = "sqmeters",
                     activeColor = "#3D535D",
                     completedColor = "#3D535D")
  })
  
  # Incremental changes to the map should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  
  # c9 state 1 leaflet
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
  
  # c9 state 2 leaflet
  observe({ # set view and max bounds
    
    activeState <- subset(states, STUSPS == getState2())
    
    rightB <- activeState@data$right_bound
    bottomB <- activeState@data$bottom_bound
    leftB <- activeState@data$left_bound
    topB <- activeState@data$top_bound
    
    proxy <- leafletProxy("c9_state2_map")
    
    # set view and max Bounds
    proxy %>% setView(lng = (rightB + leftB) / 2.0, lat = (bottomB + topB) / 2.0, zoom = 8) %>% 
      setMaxBounds(rightB, bottomB, leftB, topB)
  })
  
  # c9 state 1 leaflet
  observe({ # sync zoom
    
    zLevel <- getZoomState2()
    
    runjs(paste("var el = document.getElementById(\"c9_state1_map\"); var map = $(el).data(\"leaflet-map\"); map.setZoom(", zLevel,")"))
  })
  
  # c9 state 2 leaflet
  observe({ # sync zoom
    
    zLevel <- getZoomState1()
    
    runjs(paste("var el = document.getElementById(\"c9_state2_map\"); var map = $(el).data(\"leaflet-map\"); map.setZoom(", zLevel,")"))
  })
  
  # c9 state 1 & 2 leaflet
  observe({ # map provider
    
    proxy <- leafletProxy("c9_state1_map")
    proxy2 <- leafletProxy("c9_state2_map")
    
    proxy %>% clearTiles() %>% addProviderTiles(getMapProvider(), options = providerTileOptions(noWrap = TRUE))
    proxy2 %>% clearTiles() %>% addProviderTiles(getMapProvider(), options = providerTileOptions(noWrap = TRUE))
  })
  
  # c9 state 1 & 2 leaflet
  observe({ # handle map layers
    
    #message("-------OBSERVE 2 START-------")
    
    layers <- getMapLayers()
    
    proxy <- leafletProxy("c9_state1_map")
    proxy2 <- leafletProxy("c9_state2_map")
    
    if ("Counties" %in% layers) {
      proxy %>% showGroup("Counties")
      proxy2 %>% showGroup("Counties")
    }
    else {
      proxy %>% hideGroup("Counties")
      proxy2 %>% hideGroup("Counties")
    }
    
    if ("Tracks" %in% layers) {
      proxy %>% showGroup("Tracks")
      proxy2 %>% showGroup("Tracks")
    }
    else {
      proxy %>% hideGroup("Tracks")
      proxy2 %>% hideGroup("Tracks")
    }
    
    
    if ("Counties" %in% layers && "Tracks" %in% layers){
      
      proxy %>% hideGroup("Counties")
      proxy %>% hideGroup("Tracks")
      
      proxy %>% showGroup("Counties")
      proxy %>% showGroup("Tracks")
      
      proxy2 %>% hideGroup("Counties")
      proxy2 %>% hideGroup("Tracks")
      
      proxy2 %>% showGroup("Counties")
      proxy2 %>% showGroup("Tracks")
    }
    
    #message("-------OBSERVE 2 END-------")
  })
  
  
  
  # c9 state 1 leaflet
  observe({ # render track data, counties
    
    # 
    # req(input$width_dSlider)
    # req(input$length_dSlider)
    # req(input$injuries_dSlider)
    # req(input$fatalities_dSlider)
    # req(input$loss_dSlider)
    #
    
    # -----------
    # counties data
    #countyData <- getCountyData()
    
    dataType <- getCountyOption()
    magnitudes <- getMagnitudes()
    
    stateCode <- filter(stateFips, stateFips$State == substr(input$state1_select, 6, stop = 1000))
    
    stateCounties <- subset(counties, counties$STATEFP == stateCode$FIPS.State[1])
    
    stateCountyData <- allTornadoes %>% filter(yr == getYearAsNum())
    
    #maybe just calculate from data so it doesn't double render
    stateCountyData <- subset(stateCountyData, wid >= getWidthLower() & wid <= getWidthUpper())
    stateCountyData <- subset(stateCountyData, len >= getLengthLower() & len <= getLengthUpper())
    stateCountyData <- subset(stateCountyData, inj >= getInjuriesLower() & inj <= getInjuriesUpper())
    stateCountyData <- subset(stateCountyData, fat >= getFatalitiesLower() & fat <= getFatalitiesUpper())
    stateCountyData <- subset(stateCountyData, loss_updated >= getLossLower() & loss_updated <= getLossUpper())
    
    stateCountyData <- stateCountyData %>% filter(f1 %in% stateCounties$COUNTYFP | f2 %in% stateCounties$COUNTYFP | f3 %in% stateCounties$COUNTYFP | f4 %in% stateCounties$COUNTYFP)
    
    magStateCountyData <- stateCountyData %>% dplyr::filter(mag %in% magnitudes)
    
    if (dataType == "Tornadoes (magnitude)") {
      countyData <- magStateCountyData %>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = n())
    }
    else if (dataType ==  "Fatalities") {
      countyData <- magStateCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(fat))
    }
    else if (dataType == "Injuries") {
      countyData <- magStateCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(inj))
    }
    else if (dataType == "Loss") {
      countyData <- magStateCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(loss_updated))
    }
    
    # ----
    mapData <- filter(allTornadoes, st == getState1(), yr == getYearAsNum(), mag %in% getMagnitudes())
    #mapData <- filter(mapData, mag %in% getMagnitudes())
    #mapData <- filter(mapData, wid >= getWidthLower() & wid <= getWidthUpper())
    colnames(mapData)[8] <- "STUSPS"
    stateSelected <- subset(states, STUSPS == getState1())
    mapData <- sp::merge(stateSelected, mapData, by = "STUSPS", duplicateGeoms = TRUE)
    #mapData <- subset(mapData, STUSPS == getState1())
    
    mapData <- subset(mapData, wid >= getWidthLower() & wid <= getWidthUpper())
    mapData <- subset(mapData, len >= getLengthLower() & len <= getLengthUpper())
    mapData <- subset(mapData, inj >= getInjuriesLower() & inj <= getInjuriesUpper())
    mapData <- subset(mapData, fat >= getFatalitiesLower() & fat <= getFatalitiesUpper())
    mapData <- subset(mapData, loss_updated >= getLossLower() & loss_updated <= getLossUpper())
    
    # get map and clear shapes
    proxy <- leafletProxy("c9_state1_map", data = mapData) %>% clearShapes()
    
    activeState <- subset(states, STUSPS == getState1())
    
    # outline of all states
    proxy %>% addPolygons(data = states, weight = 3, opacity = .5, color = "black", fillOpacity = 0.2, fillColor = "black")
    
    # active state outline and fill
    proxy %>% addPolygons(data = activeState, weight = 6, opacity = 1, color = "black", fillOpacity = 0.3, fillColor = "black")
    
    # ----
    
    #message("-----OBSERVE 1 BP 1-----")
    if (nrow(countyData) != 0)
    {
      #return()
      
      countyData <- sp::merge(stateCounties, countyData, by = c("COUNTYFP"))
      
      #message("-----OBSERVE 1 BP 2-----")
      
      
      maxCount <- max(countyData$Count, na.rm = TRUE)
      
      if (maxCount <= 100)
        bins <- c(0, 1, 2, 5, 10, 20, 50, 100, Inf)
      else if (maxCount <= 1000)
        bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
      else
        bins <- c(0, maxCount * 0.01, maxCount * 0.02, maxCount * 0.05, maxCount * 0.1, maxCount * 0.2, maxCount * 0.5, maxCount * 0.7, maxCount)
      
      pal <- colorBin("OrRd", domain = countyData$Count, bins = bins, na.color = "#99999955")
      
      #message("-----OBSERVE 1 BP 3-----")
      
      if (maxCount > 1000000)
      {
        popupData <- paste0("<strong>County: </strong>", 
                            countyData$NAME, 
                            "<br><strong>Count: </strong>", 
                            countyData$Count)
      }
      else
      {
        popupData <- paste0("<strong>County: </strong>", 
                            countyData$NAME, 
                            "<br><strong>Count: </strong>", 
                            countyData$Count)
      }
      
      #message("-----OBSERVE 1 BP LAST-----")
      
      
      
      # -----------
      # counties
      
      proxy %>% addPolygons(data = countyData,
                            fillColor = ~pal(Count), 
                            fillOpacity = 0.7, 
                            color = "#333333", 
                            weight = 2,
                            popup = popupData,
                            popupOptions = popupOptions(style = list(
                              "width" = "500px",
                              "padding" = "10px",
                              "font-size" = "36px"
                            )),
                            highlightOptions = highlightOptions(color = "black", weight = 4, bringToFront = FALSE),
                            group = "Counties")
      
      #proxy %>% hideGroup("Counties")
      
      # -----------
      
      
      #message("-----OBSERVE 1 END-----")
    }
    
    #message("-----OBSERVE 1 START-----")
    # track data
    #mapData = state1_map_track_data()
    
    
    
    
    
    lat_start <- mapData@data$slat
    lat_end <- mapData@data$elat
    
    lon_start <- mapData@data$slon
    lon_end <- mapData@data$elon
    
    #state <- toString(mapData@data$STUSPS[1])
    
    # ---- color mapping
    mappings <- getTrackMappings()
    
    basedOnColor <- input$basedOn1_Select
    
    if ("Color" %in% mappings) {
      
      if (basedOnColor == "Magnitude") {
        palColorTracks <- colorBin(palette = c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c'), domain = c(0,6))
        mapData@data$dataToMap <- mapData@data$mag
      }
      else if (basedOnColor == "Width") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getWidthLower(),getWidthUpper()))
        mapData@data$dataToMap <- mapData@data$wid
      }
      else if (basedOnColor == "Length") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getLengthLower(),getLengthUpper()))
        mapData@data$dataToMap <- mapData@data$len
      }
      else if (basedOnColor == "Injuries") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getInjuriesLower(),getInjuriesUpper()))
        mapData@data$dataToMap <- mapData@data$inj
      }
      else if (basedOnColor == "Fatalities") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getFatalitiesLower(),getFatalitiesUpper()))
        mapData@data$dataToMap <- mapData@data$fat
      }
      else if (basedOnColor == "Loss") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "Greens"), domain = c(getLossLower(),getLossUpper()))
        mapData@data$dataToMap <- mapData@data$loss_updated
      }
      
    }else {
      palColorTracks <- colorBin("#8a49bc", 1)
      mapData@data$dataToMap <- rep(1, nrow(mapData@data))
    }
    
    
    basedOnWidth <- input$basedOn2_Select
    
    
    # 
    if (length(lat_start) > 0 && !is.na(lat_start)) {
      for(i in 1:length(lat_start)){
        
        if ((!(lat_start[i] == 0.0 | lat_end[i] == 0.0 | lon_start[i] == 0.0 | lon_end[i] == 0.0))) {
          
          # default width: 15
          proxy <- addPolylines(data = mapData, proxy, lat = c(lat_start[i],lat_end[i]),
                                lng = c(lon_start[i],lon_end[i]),
                                weight = mapTrackWidth(mappings, basedOnWidth, i, mapData, 10, 55, "weight"), opacity = 0.85, color = ~palColorTracks(mapData@data$dataToMap[i]), group = "Tracks",
                                label = paste("TRACK"),
                                labelOptions = labelOptions(style = list(
                                  "padding" = "10px",
                                  "font-size" = "32px"
                                ))
          )
          
        }
      }
    }
    
    # default radius: 4000
    proxy %>%
      # start
      addCircles(
        lng = ~mapData@data$slon,
        lat = ~mapData@data$slat,
        weight = 12, fillOpacity = 0.0, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96, color = ~palColorTracks(mapData@data$dataToMap), dashArray = "17, 17",
        popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
                       "<br><strong>Time: </strong></b>", mapData@data$time,
                       "<br></b>",
                       "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
                       "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
                       "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
                       "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
                       "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
                       "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
        ),
        popupOptions = popupOptions(style = list(
          "width" = "500px",
          "padding" = "10px",
          "font-size" = "36px"
        )),
        label = paste("START"),
        labelOptions = labelOptions(style = list(
          "padding" = "10px",
          "font-size" = "32px"
        )), group = "Tracks") %>%
      # end
      addCircles(
        lng = ~mapData@data$elon,
        lat = ~mapData@data$elat,
        weight = 15, fillOpacity = 0.6, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96,
        color = ~palColorTracks(mapData@data$dataToMap),
        popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
                       "<br><strong>Time: </strong></b>", mapData@data$time,
                       "<br></b>",
                       "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
                       "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
                       "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
                       "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
                       "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
                       "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
        ),
        popupOptions = popupOptions(style = list(
          "width" = "500px",
          "padding" = "10px",
          "font-size" = "36px"
        )),
        label = paste("END"),
        labelOptions = labelOptions(style = list(
          "padding" = "10px",
          "font-size" = "32px"
        )), group = "Tracks")
    
    # -----------
    
    
  })
  
  # c9 state 2 leaflet
  observe({ # render track data, counties
    
    # -----------
    # counties data
    #countyData <- getCountyData()
    
    dataType <- getCountyOption()
    magnitudes <- getMagnitudes()
    
    stateCode <- filter(stateFips, stateFips$State == substr(input$state2_select, 6, stop = 1000))
    
    stateCounties <- subset(counties, counties$STATEFP == stateCode$FIPS.State[1])
    
    stateCountyData <- allTornadoes %>% filter(yr == getYearAsNum())
    
    stateCountyData <- subset(stateCountyData, wid >= getWidthLower() & wid <= getWidthUpper())
    stateCountyData <- subset(stateCountyData, len >= getLengthLower() & len <= getLengthUpper())
    stateCountyData <- subset(stateCountyData, inj >= getInjuriesLower() & inj <= getInjuriesUpper())
    stateCountyData <- subset(stateCountyData, fat >= getFatalitiesLower() & fat <= getFatalitiesUpper())
    stateCountyData <- subset(stateCountyData, loss_updated >= getLossLower() & loss_updated <= getLossUpper())
    
    stateCountyData <- stateCountyData %>% filter(f1 %in% stateCounties$COUNTYFP | f2 %in% stateCounties$COUNTYFP | f3 %in% stateCounties$COUNTYFP | f4 %in% stateCounties$COUNTYFP)
    
    # MAYBE TODO
    magStateCountyData <- stateCountyData %>% dplyr::filter(mag %in% magnitudes)
    
    if (dataType == "Tornadoes (magnitude)") {
      countyData <- magStateCountyData %>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = n())
    }
    else if (dataType ==  "Fatalities") {
      countyData <- magStateCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(fat))
    }
    else if (dataType == "Injuries") {
      countyData <- magStateCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(inj))
    }
    else if (dataType == "Loss") {
      countyData <- magStateCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(loss_updated))
    }
    
    # ----
    mapData <- filter(allTornadoes, st == getState2(), yr == getYearAsNum(), mag %in% getMagnitudes())
    #mapData <- filter(mapData, mag %in% getMagnitudes())
    #mapData <- filter(mapData, wid >= getWidthLower() & wid <= getWidthUpper())
    colnames(mapData)[8] <- "STUSPS"
    stateSelected <- subset(states, STUSPS == getState2())
    mapData <- sp::merge(stateSelected, mapData, by = "STUSPS", duplicateGeoms = TRUE)
    #mapData <- subset(mapData, STUSPS == getState1())
    
    
    mapData <- subset(mapData, wid >= getWidthLower() & wid <= getWidthUpper())
    mapData <- subset(mapData, len >= getLengthLower() & len <= getLengthUpper())
    mapData <- subset(mapData, inj >= getInjuriesLower() & inj <= getInjuriesUpper())
    mapData <- subset(mapData, fat >= getFatalitiesLower() & fat <= getFatalitiesUpper())
    mapData <- subset(mapData, loss_updated >= getLossLower() & loss_updated <= getLossUpper())
    
    # get map and clear shapes
    proxy <- leafletProxy("c9_state2_map", data = mapData) %>% clearShapes()
    
    activeState <- subset(states, STUSPS == getState2())
    
    # outline of all states
    proxy %>% addPolygons(data = states, weight = 3, opacity = .5, color = "black", fillOpacity = 0.2, fillColor = "black")
    
    # active state outline and fill
    proxy %>% addPolygons(data = activeState, weight = 6, opacity = 1, color = "black", fillOpacity = 0.3, fillColor = "black")
    
    # ----
    
    #message("-----OBSERVE 1 BP 1-----")
    if (nrow(countyData) != 0)
    {
      #return()
      
      countyData <- sp::merge(stateCounties, countyData, by = c("COUNTYFP"))
      
      #message("-----OBSERVE 1 BP 2-----")
      
      
      maxCount <- max(countyData$Count, na.rm = TRUE)
      
      if (maxCount <= 100)
        bins <- c(0, 1, 2, 5, 10, 20, 50, 100, Inf)
      else if (maxCount <= 1000)
        bins <- c(0, 10, 20, 50, 100, 200, 500, 1000, Inf)
      else
        bins <- c(0, maxCount * 0.01, maxCount * 0.02, maxCount * 0.05, maxCount * 0.1, maxCount * 0.2, maxCount * 0.5, maxCount * 0.7, maxCount)
      
      pal <- colorBin("OrRd", domain = countyData$Count, bins = bins, na.color = "#99999955")
      
      #message("-----OBSERVE 1 BP 3-----")
      
      if (maxCount > 1000000)
      {
        popupData <- paste0("<strong>County: </strong>", 
                            countyData$NAME, 
                            "<br><strong>Count: </strong>", 
                            countyData$Count)
      }
      else
      {
        popupData <- paste0("<strong>County: </strong>", 
                            countyData$NAME, 
                            "<br><strong>Count: </strong>", 
                            countyData$Count)
      }
      
      #message("-----OBSERVE 1 BP LAST-----")
      
      
      
      # -----------
      # counties
      
      proxy %>% addPolygons(data = countyData,
                            fillColor = ~pal(Count), 
                            fillOpacity = 0.7, 
                            color = "#333333", 
                            weight = 2,
                            popup = popupData,
                            popupOptions = popupOptions(style = list(
                              "width" = "500px",
                              "padding" = "10px",
                              "font-size" = "36px"
                            )),
                            highlightOptions = highlightOptions(color = "black", weight = 4, bringToFront = FALSE),
                            group = "Counties")
      
      #proxy %>% hideGroup("Counties")
      
      # -----------
      
      
      #message("-----OBSERVE 1 END-----")
    }
    
    #message("-----OBSERVE 1 START-----")
    # track data
    #mapData = state1_map_track_data()
    
    
    
    
    lat_start <- mapData@data$slat
    lat_end <- mapData@data$elat
    
    lon_start <- mapData@data$slon
    lon_end <- mapData@data$elon
    
    #state <- toString(mapData@data$STUSPS[1])
    
    # ---- color mapping
    mappings <- getTrackMappings()
    
    basedOnColor <- input$basedOn1_Select
    
    if ("Color" %in% mappings) {
      
      if (basedOnColor == "Magnitude") {
        palColorTracks <- colorBin(palette = c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c'), domain = c(0,6))
        mapData@data$dataToMap <- mapData@data$mag
      }
      else if (basedOnColor == "Width") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getWidthLower(),getWidthUpper()))
        mapData@data$dataToMap <- mapData@data$wid
      }
      else if (basedOnColor == "Length") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getLengthLower(),getLengthUpper()))
        mapData@data$dataToMap <- mapData@data$len
      }
      else if (basedOnColor == "Injuries") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getInjuriesLower(),getInjuriesUpper()))
        mapData@data$dataToMap <- mapData@data$inj
      }
      else if (basedOnColor == "Fatalities") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getFatalitiesLower(),getFatalitiesUpper()))
        mapData@data$dataToMap <- mapData@data$fat
      }
      else if (basedOnColor == "Loss") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "Greens"), domain = c(getLossLower(),getLossUpper()))
        mapData@data$dataToMap <- mapData@data$loss_updated
      }
      
    }else {
      palColorTracks <- colorBin("#8a49bc", 1)
      mapData@data$dataToMap <- rep(1, nrow(mapData@data))
    }
    
    
    basedOnWidth <- input$basedOn2_Select
    
    
    # 
    if (length(lat_start) > 0 && !is.na(lat_start)) {
      for(i in 1:length(lat_start)){
        
        if ((!(lat_start[i] == 0.0 | lat_end[i] == 0.0 | lon_start[i] == 0.0 | lon_end[i] == 0.0))) {
          
          # default width: 15
          proxy <- addPolylines(data = mapData, proxy, lat = c(lat_start[i],lat_end[i]),
                                lng = c(lon_start[i],lon_end[i]),
                                weight = mapTrackWidth(mappings, basedOnWidth, i, mapData, 10, 55, "weight"), opacity = 0.85, color = ~palColorTracks(mapData@data$dataToMap[i]), group = "Tracks",
                                label = paste("TRACK"),
                                labelOptions = labelOptions(style = list(
                                  "padding" = "10px",
                                  "font-size" = "32px"
                                ))
          )
          
        }
      }
    }
    
    # default radius: 4000
    proxy %>%
      # start
      addCircles(
        lng = ~mapData@data$slon,
        lat = ~mapData@data$slat,
        weight = 12, fillOpacity = 0.0, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96, color = ~palColorTracks(mapData@data$dataToMap), dashArray = "17, 17",
        popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
                       "<br><strong>Time: </strong></b>", mapData@data$time,
                       "<br></b>",
                       "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
                       "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
                       "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
                       "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
                       "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
                       "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
        ),
        popupOptions = popupOptions(style = list(
          "width" = "500px",
          "padding" = "10px",
          "font-size" = "36px"
        )),
        label = paste("START"),
        labelOptions = labelOptions(style = list(
          "padding" = "10px",
          "font-size" = "32px"
        )), group = "Tracks") %>%
      # end
      addCircles(
        lng = ~mapData@data$elon,
        lat = ~mapData@data$elat,
        weight = 15, fillOpacity = 0.6, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96,
        color = ~palColorTracks(mapData@data$dataToMap),
        popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
                       "<br><strong>Time: </strong></b>", mapData@data$time,
                       "<br></b>",
                       "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
                       "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
                       "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
                       "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
                       "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
                       "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
        ),
        popupOptions = popupOptions(style = list(
          "width" = "500px",
          "padding" = "10px",
          "font-size" = "36px"
        )),
        label = paste("END"),
        labelOptions = labelOptions(style = list(
          "padding" = "10px",
          "font-size" = "32px"
        )), group = "Tracks")
    
    # -----------
    
    
  })
  
  # ------------
  
  mapTrackWidth <- function(mappings, basedOnWidth, i, mapData, newLowerB, newUpperB, flagType){
    
    if (flagType == "weight") {
      
      if ("Size" %in% mappings) {
        
        if (basedOnWidth == "Magnitude") {
          #palColorTracks <- colorBin(palette = c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c'), domain = c(0,6))
          #mapData@data$dataToMap <- mapData@data$mag
          
          value <- mapData@data$mag[i]
          
          oldLowerB <- 0
          oldUpperB <- 5
          
        }
        else if (basedOnWidth == "Width") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getWidthLower(),getWidthUpper()))
          #mapData@data$dataToMap <- mapData@data$wid
          
          value <- mapData@data$wid[i]
          
          oldLowerB <- getWidthLower()
          oldUpperB <- getWidthUpper()
        }
        else if (basedOnWidth == "Length") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getLengthLower(),getLengthUpper()))
          #mapData@data$dataToMap <- mapData@data$len
          
          value <- mapData@data$len[i]
          
          oldLowerB <- getLengthLower()
          oldUpperB <- getLengthUpper()
        }
        else if (basedOnWidth == "Injuries") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getInjuriesLower(),getInjuriesUpper()))
          #mapData@data$dataToMap <- mapData@data$inj
          
          value <- mapData@data$inj[i]
          
          oldLowerB <- getInjuriesLower()
          oldUpperB <- getInjuriesUpper()
        }
        else if (basedOnWidth == "Fatalities") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getFatalitiesLower(),getFatalitiesUpper()))
          #mapData@data$dataToMap <- mapData@data$fat
          
          value <- mapData@data$fat[i]
          
          oldLowerB <- getFatalitiesLower()
          oldUpperB <- getFatalitiesUpper()
        }
        else if (basedOnWidth == "Loss") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "Greens"), domain = c(getLossLower(),getLossUpper()))
          #mapData@data$dataToMap <- mapData@data$loss_updated
          
          value <- mapData@data$loss_updated[i]
          
          oldLowerB <- getLossLower()
          oldUpperB <- getLossUpper() 
        }
        
      } else {
        return(15)
      }
      
      return ( (value - oldLowerB)*((newUpperB - newLowerB)/(oldUpperB - oldLowerB)) + newLowerB  )
      
    } else {
      
      if ("Size" %in% mappings) {
        
        if (basedOnWidth == "Magnitude") {
          #palColorTracks <- colorBin(palette = c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c'), domain = c(0,6))
          mapData@data$dataToMap2 <- mapData@data$mag
          
          #value <- mapData@data$mag[i]
          
          oldLowerB <- 0
          oldUpperB <- 5
          
        }
        else if (basedOnWidth == "Width") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getWidthLower(),getWidthUpper()))
          mapData@data$dataToMap2 <- mapData@data$wid
          
          #value <- mapData@data$wid[i]
          
          oldLowerB <- getWidthLower()
          oldUpperB <- getWidthUpper()
        }
        else if (basedOnWidth == "Length") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getLengthLower(),getLengthUpper()))
          mapData@data$dataToMap2 <- mapData@data$len
          
          #value <- mapData@data$len[i]
          
          oldLowerB <- getLengthLower()
          oldUpperB <- getLengthUpper()
        }
        else if (basedOnWidth == "Injuries") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getInjuriesLower(),getInjuriesUpper()))
          mapData@data$dataToMap2 <- mapData@data$inj
          
          #value <- mapData@data$inj[i]
          
          oldLowerB <- getInjuriesLower()
          oldUpperB <- getInjuriesUpper()
        }
        else if (basedOnWidth == "Fatalities") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getFatalitiesLower(),getFatalitiesUpper()))
          mapData@data$dataToMap2 <- mapData@data$fat
          
          #value <- mapData@data$fat[i]
          
          oldLowerB <- getFatalitiesLower()
          oldUpperB <- getFatalitiesUpper()
        }
        else if (basedOnWidth == "Loss") {
          #palColorTracks <- colorNumeric(palette = brewer.pal(7, "Greens"), domain = c(getLossLower(),getLossUpper()))
          mapData@data$dataToMap2 <- mapData@data$loss_updated
          
          #value <- mapData@data$loss_updated[i]
          
          oldLowerB <- getLossLower()
          oldUpperB <- getLossUpper() 
        }
        
      } else {
        
        mapData@data$dataToMap2 <- rep(4000, nrow(mapData@data))
        
        return(mapData@data$dataToMap2)
      }
      
      if (length(mapData@data$dataToMap2) > 0 && !is.na(mapData@data$dataToMap2)) {
        for(j in 1:length(mapData@data$dataToMap2)){
          
          mapData@data$dataToMap2[j] <- (mapData@data$dataToMap2[j] - oldLowerB)*((newUpperB - newLowerB)/(oldUpperB - oldLowerB)) + newLowerB
          
          
        }
      }
      
      return ( mapData@data$dataToMap2  )
      
    }
    
  }
  
  # ------------
  
  #outputOptions(output,"c9_state1_map",suspendWhenHidden=FALSE) # causes errors in console? don't use?
  
  # # c9 state 2 leaflet
  # output$c9_state2_map <- renderLeaflet({ # initialize map to default dropdown state value
  #   
  #   m <- leaflet(states, options = leafletOptions(minZoom = 7, maxZoom = 16)) %>%
  #     setView(lng = (-87.4 + -91.6) / 2.0, lat = (36.7 + 42.6) / 2.0, zoom = 8) %>%
  #     #fitBounds(-87.4, 36.7, -91.6, 42.6) %>%
  #     setMaxBounds(-87.4, 36.7, -91.6, 42.6) %>%
  #     addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
  #                      options = providerTileOptions(noWrap = TRUE)) #%>% addMeasure()
  #   
  #   m %>% addPolygons(weight = 5, opacity = 1, color = "black", fillOpacity = .1)
  # })
  # 
  
  # overview leaflet
  output$overview_map <- renderLeaflet({ 
    
    #activeState1 <- filter(stateBounds, STUSPS == "IL")
    #activeState2 <- filter(stateBounds, STUSPS == "KS")
    
    m <- leaflet(states, options = leafletOptions(minZoom = 6, maxZoom = 16)) %>%
      setView(lng = -96, lat = 37.8, zoom = 6) %>%
      setMaxBounds(-65, 20, -135, 50) %>%
      #fitBounds(-87.4, 36.7, -91.6, 42.6) %>%
      #setMaxBounds(-87.4, 36.7, -91.6, 42.6) %>%
      addProviderTiles(providers$Stamen.TonerLite, # CartoDB.Positron
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMiniMap(
        tiles = providers$Stamen.TonerLite,
        toggleDisplay = TRUE,
        width = 400, height = 400,
        collapsedWidth = 50, collapsedHeight = 50,
        aimingRectOptions = list(color = "#333333", weight = 1, clickable = FALSE)) %>% 
      addMeasure(    position = "bottomleft",
                     primaryLengthUnit = "meters",
                     primaryAreaUnit = "sqmeters",
                     activeColor = "#3D535D",
                     completedColor = "#3D535D")
    
    m %>% addPolygons(weight = 5, opacity = .5, color = "black", fillOpacity = .1)
    
    #m %>% addPolygons(data = activeState1, weight = 10, opacity = 1, color = "white", fillOpacity = 0.0, fillColor = "black")
    #m %>% addPolygons(data = activeState2, weight = 10, opacity = 1, color = "white", fillOpacity = 0.0, fillColor = "black")
  })
  
  # observe({ # select active states
  #   
  #   # get map and clear shapes
  #   proxy <- leafletProxy("overview_map", data = mapData) %>% clearShapes()
  #   
  #   activeState <- subset(states, STUSPS == getState1())
  #   
  #   # outline of all states
  #   proxy %>% addPolygons(data = states, weight = 3, opacity = .5, color = "black", fillOpacity = 0.2, fillColor = "black")
  #   
  #   # active state outline and fill
  #   proxy %>% addPolygons(data = activeState, weight = 6, opacity = 1, color = "black", fillOpacity = 0.3, fillColor = "black")
  #   
  # })
  
  # ALL TORNADO TRACKS PLUS MOST DESTRUCTIVE TORNAODES ON SAME MAP
  observe({ # render track data, counties
    
    # ALL TORNADO TRACKS ====
    mapData <- allTornadoes %>% dplyr::filter(yr == getYearAsNum())
    
    colnames(mapData)[8] <- "STUSPS"
    
    stateSelected <- subset(states, STUSPS %in% mapData$STUSPS)
    
    mapData <- sp::merge(stateSelected, mapData, by = "STUSPS", duplicateGeoms = TRUE)
    
    # get map and clear shapes
    proxy <- leafletProxy("overview_map", data = mapData) %>% clearShapes()
    
    activeState <- subset(states, STUSPS == getState1() | STUSPS == getState2())
    
    # outline of all states
    proxy %>% addPolygons(data = states, weight = 3, opacity = .5, color = "black", fillOpacity = 0.2, fillColor = "black")
    
    # active state outline and fill
    proxy %>% addPolygons(data = activeState, weight = 6, opacity = 1, color = "black", fillOpacity = 0.3, fillColor = "black")
    
    
    lat_start <- mapData@data$slat
    lat_end <- mapData@data$elat
    
    lon_start <- mapData@data$slon
    lon_end <- mapData@data$elon
    
    # ---- color mapping
    mappings <- getTrackMappings()
    
    basedOnColor <- input$basedOn1_Select
    
    if ("Color" %in% mappings) {
      
      if (basedOnColor == "Magnitude") {
        palColorTracks <- colorBin(palette = c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c'), domain = c(0,6))
        mapData@data$dataToMap <- mapData@data$mag
      }
      else if (basedOnColor == "Width") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getWidthLower(),getWidthUpper()))
        mapData@data$dataToMap <- mapData@data$wid
      }
      else if (basedOnColor == "Length") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getLengthLower(),getLengthUpper()))
        mapData@data$dataToMap <- mapData@data$len
      }
      else if (basedOnColor == "Injuries") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getInjuriesLower(),getInjuriesUpper()))
        mapData@data$dataToMap <- mapData@data$inj
      }
      else if (basedOnColor == "Fatalities") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getFatalitiesLower(),getFatalitiesUpper()))
        mapData@data$dataToMap <- mapData@data$fat
      }
      else if (basedOnColor == "Loss") {
        palColorTracks <- colorNumeric(palette = brewer.pal(7, "Greens"), domain = c(getLossLower(),getLossUpper()))
        mapData@data$dataToMap <- mapData@data$loss_updated
      }
      
    }
    else {
      palColorTracks <- colorBin("#8a49bc", 1)
      mapData@data$dataToMap <- rep(1, nrow(mapData@data))
    }
    
    
    basedOnWidth <- input$basedOn2_Select
    
    
    #
    if (length(lat_start) > 0 && !is.na(lat_start)) {
      for(i in 1:length(lat_start)){
        
        if ((!(lat_start[i] == 0.0 | lat_end[i] == 0.0 | lon_start[i] == 0.0 | lon_end[i] == 0.0))) {
          
          # default width: 15
          proxy <- addPolylines(data = mapData, proxy, lat = c(lat_start[i],lat_end[i]),
                                lng = c(lon_start[i],lon_end[i]),
                                weight = mapTrackWidth(mappings, basedOnWidth, i, mapData, 10, 55, "weight"), opacity = 0.85, color = ~palColorTracks(mapData@data$dataToMap[i]), group = "Tracks",
                                label = paste("TRACK"),
                                labelOptions = labelOptions(style = list(
                                  "padding" = "10px",
                                  "font-size" = "32px"
                                ))
          )
          
        }
      }
    }
    
    # default radius: 4000
    proxy %>%
      # start
      addCircles(
        lng = ~mapData@data$slon,
        lat = ~mapData@data$slat,
        weight = 12, fillOpacity = 0.0, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96, color = ~palColorTracks(mapData@data$dataToMap), dashArray = "17, 17",
        popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
                       "<br><strong>Time: </strong></b>", mapData@data$time,
                       "<br></b>",
                       "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
                       "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
                       "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
                       "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
                       "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
                       "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
        ),
        popupOptions = popupOptions(style = list(
          "width" = "500px",
          "padding" = "10px",
          "font-size" = "36px"
        )),
        label = paste("START"),
        labelOptions = labelOptions(style = list(
          "padding" = "10px",
          "font-size" = "32px"
        )), group = "Tracks") %>%
      # end
      addCircles(
        lng = ~mapData@data$elon,
        lat = ~mapData@data$elat,
        weight = 15, fillOpacity = 0.6, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96,
        color = ~palColorTracks(mapData@data$dataToMap),
        popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
                       "<br><strong>Time: </strong></b>", mapData@data$time,
                       "<br></b>",
                       "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
                       "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
                       "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
                       "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
                       "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
                       "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
        ),
        popupOptions = popupOptions(style = list(
          "width" = "500px",
          "padding" = "10px",
          "font-size" = "36px"
        )),
        label = paste("END"),
        labelOptions = labelOptions(style = list(
          "padding" = "10px",
          "font-size" = "32px"
        )), group = "Tracks")
    
    # -----------
    
    # MOST DESTRUCTIVE TORNADO TRACKS
    if (!("Color" %in% mappings | "Size" %in% mappings))
    {
      state1Tornaodes <- getMostDestructiveTornadoes(getState1())
      state2Tornaodes <- getMostDestructiveTornadoes(getState2())
      
      mapData <- rbind(state1Tornaodes, state2Tornaodes)
      
      colnames(mapData)[8] <- "STUSPS"
      
      stateSelected <- subset(states, STUSPS == getState1() | STUSPS == getState2())
      
      mapData <- sp::merge(stateSelected, mapData, by = "STUSPS", duplicateGeoms = TRUE)
      
      activeState <- subset(states, STUSPS == getState1() | STUSPS == getState2())
      
      # active state outline and fill
      proxy %>% addPolygons(data = activeState, weight = 6, opacity = 1, color = "black", fillOpacity = 0.3, fillColor = "black")
      
      palColorTracks <- colorBin("#56ffc1", 1)
      mapData@data$dataToMap <- rep(1, nrow(mapData@data))
      
      lat_start <- mapData@data$slat
      lat_end <- mapData@data$elat
      
      lon_start <- mapData@data$slon
      lon_end <- mapData@data$elon
      
      if (length(lat_start) > 0 && !is.na(lat_start)) {
        for(i in 1:length(lat_start)){
          
          if ((!(lat_start[i] == 0.0 | lat_end[i] == 0.0 | lon_start[i] == 0.0 | lon_end[i] == 0.0))) {
            
            # default width: 15
            proxy <- addPolylines(data = mapData, proxy, lat = c(lat_start[i],lat_end[i]),
                                  lng = c(lon_start[i],lon_end[i]),
                                  weight = mapTrackWidth(mappings, basedOnWidth, i, mapData, 10, 55, "weight"), opacity = 0.85, color = ~palColorTracks(mapData@data$dataToMap[i]), group = "Tracks",
                                  label = paste("TRACK"),
                                  labelOptions = labelOptions(style = list(
                                    "padding" = "10px",
                                    "font-size" = "32px"
                                  ))
            )
            
          }
        }
      }
      
      # default radius: 4000
      proxy %>%
        # start
        addCircles(
          lng = ~mapData@data$slon,
          lat = ~mapData@data$slat,
          weight = 12, fillOpacity = 0.0, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96, color = ~palColorTracks(mapData@data$dataToMap), dashArray = "17, 17",
          popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
                         "<br><strong>Time: </strong></b>", mapData@data$time,
                         "<br></b>",
                         "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
                         "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
                         "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
                         "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
                         "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
                         "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
          ),
          popupOptions = popupOptions(style = list(
            "width" = "500px",
            "padding" = "10px",
            "font-size" = "36px"
          )),
          label = paste("START"),
          labelOptions = labelOptions(style = list(
            "padding" = "10px",
            "font-size" = "32px"
          )), group = "Tracks") %>%
        # end
        addCircles(
          lng = ~mapData@data$elon,
          lat = ~mapData@data$elat,
          weight = 15, fillOpacity = 0.6, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96,
          color = ~palColorTracks(mapData@data$dataToMap),
          popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
                         "<br><strong>Time: </strong></b>", mapData@data$time,
                         "<br></b>",
                         "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
                         "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
                         "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
                         "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
                         "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
                         "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
          ),
          popupOptions = popupOptions(style = list(
            "width" = "500px",
            "padding" = "10px",
            "font-size" = "36px"
          )),
          label = paste("END"),
          labelOptions = labelOptions(style = list(
            "padding" = "10px",
            "font-size" = "32px"
          )), group = "Tracks")
      
      
    }
    
  })
  
  # # MOST DESTRUCTIVE TORNADO MAP
  # observe({ # render track data, counties
  # 
  #   # Get most destructive tornadoes for both states
  #   state1Tornaodes <- getMostDestructiveTornadoes(getState1())
  #   state2Tornaodes <- getMostDestructiveTornadoes(getState2())
  # 
  #   mapData <- rbind(state1Tornaodes, state2Tornaodes)
  # 
  #   colnames(mapData)[8] <- "STUSPS"
  # 
  #   stateSelected <- subset(states, STUSPS == getState1() | STUSPS == getState2())
  # 
  #   mapData <- sp::merge(stateSelected, mapData, by = "STUSPS", duplicateGeoms = TRUE)
  # 
  #   # get map and clear shapes
  #   proxy <- leafletProxy("overview_map", data = mapData) %>% clearShapes()
  # 
  #   activeState <- subset(states, STUSPS == getState1() | STUSPS == getState2())
  # 
  #   # outline of all states
  #   proxy %>% addPolygons(data = states, weight = 3, opacity = .5, color = "black", fillOpacity = 0.2, fillColor = "black")
  # 
  #   # active state outline and fill
  #   proxy %>% addPolygons(data = activeState, weight = 6, opacity = 1, color = "black", fillOpacity = 0.3, fillColor = "black")
  # 
  #   # ----
  #   lat_start <- mapData@data$slat
  #   lat_end <- mapData@data$elat
  # 
  #   lon_start <- mapData@data$slon
  #   lon_end <- mapData@data$elon
  # 
  #   # ---- color mapping
  #   mappings <- getTrackMappings()
  # 
  #   basedOnColor <- input$basedOn1_Select
  # 
  #   if ("Color" %in% mappings) {
  # 
  #     if (basedOnColor == "Magnitude") {
  #       palColorTracks <- colorBin(palette = c('#eff3ff','#c6dbef','#9ecae1','#6baed6','#3182bd','#08519c'), domain = c(0,6))
  #       mapData@data$dataToMap <- mapData@data$mag
  #     }
  #     else if (basedOnColor == "Width") {
  #       palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getWidthLower(),getWidthUpper()))
  #       mapData@data$dataToMap <- mapData@data$wid
  #     }
  #     else if (basedOnColor == "Length") {
  #       palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getLengthLower(),getLengthUpper()))
  #       mapData@data$dataToMap <- mapData@data$len
  #     }
  #     else if (basedOnColor == "Injuries") {
  #       palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getInjuriesLower(),getInjuriesUpper()))
  #       mapData@data$dataToMap <- mapData@data$inj
  #     }
  #     else if (basedOnColor == "Fatalities") {
  #       palColorTracks <- colorNumeric(palette = brewer.pal(7, "GnBu"), domain = c(getFatalitiesLower(),getFatalitiesUpper()))
  #       mapData@data$dataToMap <- mapData@data$fat
  #     }
  #     else if (basedOnColor == "Loss") {
  #       palColorTracks <- colorNumeric(palette = brewer.pal(7, "Greens"), domain = c(getLossLower(),getLossUpper()))
  #       mapData@data$dataToMap <- mapData@data$loss_updated
  #     }
  # 
  #   }
  #   else {
  #     palColorTracks <- colorBin("#56ffc1", 1)
  #     mapData@data$dataToMap <- rep(1, nrow(mapData@data))
  #   }
  # 
  # 
  #   basedOnWidth <- input$basedOn2_Select
  # 
  # 
  #   #
  #   if (length(lat_start) > 0 && !is.na(lat_start)) {
  #     for(i in 1:length(lat_start)){
  # 
  #       if ((!(lat_start[i] == 0.0 | lat_end[i] == 0.0 | lon_start[i] == 0.0 | lon_end[i] == 0.0))) {
  # 
  #         # default width: 15
  #         proxy <- addPolylines(data = mapData, proxy, lat = c(lat_start[i],lat_end[i]),
  #                               lng = c(lon_start[i],lon_end[i]),
  #                               weight = mapTrackWidth(mappings, basedOnWidth, i, mapData, 10, 55, "weight"), opacity = 0.85, color = ~palColorTracks(mapData@data$dataToMap[i]), group = "Tracks",
  #                               label = paste("TRACK"),
  #                               labelOptions = labelOptions(style = list(
  #                                 "padding" = "10px",
  #                                 "font-size" = "32px"
  #                               ))
  #         )
  # 
  #       }
  #     }
  #   }
  # 
  #   # default radius: 4000
  #   proxy %>%
  #     # start
  #     addCircles(
  #       lng = ~mapData@data$slon,
  #       lat = ~mapData@data$slat,
  #       weight = 12, fillOpacity = 0.0, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96, color = ~palColorTracks(mapData@data$dataToMap), dashArray = "17, 17",
  #       popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
  #                      "<br><strong>Time: </strong></b>", mapData@data$time,
  #                      "<br></b>",
  #                      "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
  #                      "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
  #                      "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
  #                      "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
  #                      "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
  #                      "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
  #       ),
  #       popupOptions = popupOptions(style = list(
  #         "width" = "500px",
  #         "padding" = "10px",
  #         "font-size" = "36px"
  #       )),
  #       label = paste("START"),
  #       labelOptions = labelOptions(style = list(
  #         "padding" = "10px",
  #         "font-size" = "32px"
  #       )), group = "Tracks") %>%
  #     # end
  #     addCircles(
  #       lng = ~mapData@data$elon,
  #       lat = ~mapData@data$elat,
  #       weight = 15, fillOpacity = 0.6, radius = mapTrackWidth(mappings, basedOnWidth, i, mapData, 3600, 20000, "radius"), opacity = .96,
  #       color = ~palColorTracks(mapData@data$dataToMap),
  #       popup = paste0("    <strong>Date: </strong>", mapData@data$mo, " - ", mapData@data$dy, " - ", mapData@data$yr,
  #                      "<br><strong>Time: </strong></b>", mapData@data$time,
  #                      "<br></b>",
  #                      "<br><strong>Magnitude   : </strong></b>", mapData@data$mag,
  #                      "<br><strong>Width (yds) : </strong></b>", mapData@data$wid,
  #                      "<br><strong>Length (mi) : </strong></b>", mapData@data$len,
  #                      "<br><strong>Injuries    : </strong></b>", mapData@data$inj,
  #                      "<br><strong>Fatalities  : </strong></b>", mapData@data$fat,
  #                      "<br><strong>Loss ($)    : </strong></b>", (mapData@data$loss_updated/1000000.0), " million"
  #       ),
  #       popupOptions = popupOptions(style = list(
  #         "width" = "500px",
  #         "padding" = "10px",
  #         "font-size" = "36px"
  #       )),
  #       label = paste("END"),
  #       labelOptions = labelOptions(style = list(
  #         "padding" = "10px",
  #         "font-size" = "32px"
  #       )), group = "Tracks")
  # 
  #   # -----------
  # 
  # 
  # })
  
  
  
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
  
  #illinoisCounties <- subset(counties, counties$STATEFP == 17)
  #illinoisCountyData <- allTornadoes %>% filter(f1 %in% illinoisCounties$COUNTYFP | f2 %in% illinoisCounties$COUNTYFP | f3 %in% illinoisCounties$COUNTYFP | f4 %in% illinoisCounties$COUNTYFP)
  getMostDestructiveTornadoes <- function(state)
  {
    topTornadoes <- allTornadoes %>% dplyr::filter(st == state) %>% dplyr::arrange(desc(destructionScore)) %>% top_n(10)
    
    return(topTornadoes)
  }
  
  # getCountyData <- function()
  # {
  #   dataType <- getCountyOption()
  #   magnitudes <- getMagnitudes()
  #   
  #   stateCode <- filter(stateFips, stateFips$State == substr(input$state1_select, 6, stop = 1000))
  #   
  #   
  #   
  #   stateCounties <- subset(counties, counties$STATEFP == stateCode$FIPS.State[1])
  #   
  #   stateCountyData <- allTornadoes %>% filter(yr == getYearAsNum())
  #   
  #   stateCountyData <- stateCountyData %>% filter(f1 %in% stateCounties$COUNTYFP | f2 %in% stateCounties$COUNTYFP | f3 %in% stateCounties$COUNTYFP | f4 %in% stateCounties$COUNTYFP)
  #   
  #   
  #   # MAYBE TODO
  #   magStateCountyData <- stateCountyData %>% dplyr::filter(mag %in% magnitudes)
  #   
  #   if (dataType == "Tornadoes (magnitude)")
  #   {
  #     countyData <- magStateCountyData %>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = n())
  #   }
  #   else if (dataType ==  "Fatalities")
  #   {
  #     countyData <- magStateCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(fat))
  #   }
  #   else if (dataType == "Injuries")
  #   {
  #     countyData <- magStateCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(inj))
  #   }
  #   else if (dataType == "Loss")
  #   {
  #     countyData <- magStateCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(loss_updated))
  #   }
  #   
  #   countyData <- sp::merge(stateCounties, countyData, by = c("COUNTYFP"))
  #   
  #   return(countyData)
  # }
  
  # getCountyData <- function(dataType)
  # {
  #   dataType <- getCountyOption()
  #   
  #   magnitudes <- getMagnitudes()
  #   
  #   illinoisCounties <- subset(counties, counties$STATEFP == getState1())
  #   illinoisCountyData <- allTornadoes %>% filter(f1 %in% stateCounties$COUNTYFP | f2 %in% stateCounties$COUNTYFP | f3 %in% stateCounties$COUNTYFP | f4 %in% stateCounties$COUNTYFP)
  #   
  #   
  #   # MAYBE TODO
  #   magIllinoisCountyData <- stateCountyData %>% dplyr::filter(mag %in% magnitudes)
  #   
  #   if (dataType == "Tornadoes")
  #   {
  #     countyData <- magIllinoisCountyData %>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = n())
  #   }
  #   else if (dataType ==  "Fatalities")
  #   {
  #     countyData <- magIllinoisCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(fat))
  #   }
  #   else if (dataType == "Injuries")
  #   {
  #     countyData <- magIllinoisCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(inj))
  #   }
  #   else if (dataType == "Loss")
  #   {
  #     countyData <- magIllinoisCountyData%>% dplyr::group_by(COUNTYFP = f1) %>% summarise(Count = sum(loss))
  #   }
  #   
  #   countyData <- sp::merge(illinoisCounties, countyData, by = c("COUNTYFP"))
  #   
  #   return(countyData)
  # }
  
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
        dplyr::group_by(Hour = format(strptime(time, "%H:%M:%S"), format="%H:00"), Magnitude = mag) %>% 
        dplyr::summarise(Count = n()) %>% 
        dplyr::mutate(Percent = (Count / sum(Count) * 100))
      
      c3$Hour <- ordered(c3$Hour, levels = hours24[,])
    }
    else
    {
      hours12 <- as.data.frame(c("12:00 AM", "01:00 AM", "02:00 AM", "03:00 AM", "04:00 AM", "05:00 AM", "06:00 AM", "07:00 AM", "08:00 AM", "09:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "01:00 PM", "02:00 PM", "03:00 PM", "04:00 PM", "05:00 PM", "06:00 PM", "07:00 PM", "08:00 PM", "09:00 PM", "10:00 PM", "11:00 PM"))
      
      c3 <- allTornadoes %>% dplyr::filter(st == state) %>%
        dplyr::group_by(Hour = format(strptime(time, "%H:%M:%S"), format="%I:00 %p"), Magnitude = mag) %>% 
        dplyr::summarise(Count = n()) %>% 
        dplyr::mutate(Percent = (Count / sum(Count) * 100))
      
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
  
  c5Data <- function(state)
  {
    c5 <- allTornadoes %>% dplyr::filter(st == state) %>%
      group_by(Year = yr) %>%
      summarise(Injury = sum(inj), Fatality = sum(fat), Loss = sum(loss_updated))
    
    c5$Loss <- c5$Loss / 1000000
    
    #c5 <- allTornadoes %>% dplyr::filter(st == state) %>%
    #  group_by( Year = yr, Injury = inj, Fatality = fat, Loss = loss_updated) %>% 
    #  summarise(Count = n())
    
    #c5$Percent <- format(round(c5$Percent, 2), nsmall = 2)
    #c5$Percent <- paste0(c5$Percent, "%")
    #Clean up currency - TODO
    #c5$Loss   <- currency(c5$Loss, digits = 0L)
    #c5$Loss   <- paste0('$',formatC(c5$Loss, big.mark=',', format = 'fg'))
    #c5$Loss   <- paste0('$', c5$Loss)
    
    #c5 <- dplyr::arrange(c5, Injury, Fatality, Loss)
    
    return(c5)
  }
  
  #C6 | Table and chart showing the injuries, fatalities, loss per month summed over all years
  c6Data <- function(state)
  {
    c6 <- allTornadoes %>% dplyr::filter(st == state) %>%
      group_by(Month = mo) %>%
      summarise(Injury = sum(inj), Fatality = sum(fat), Loss = sum(loss_updated))
    
    c6$Loss <- c6$Loss / 1000000
    
    # c6 <- allTornadoes %>% dplyr::filter(st == state) %>%
    #  group_by(Month = mo, Injury = inj, Fatality = fat, Loss = loss_updated) %>%
    #  summarise(Count = n()) %>%
    #  mutate(Percent = (Count / sum(Count) * 100))
    
    #c6$Percent <- format(round(c6$Percent, 2), nsmall = 2)
    #c6$Percent <- paste0(c6$Percent, "%")
    
    return(c6)
  }
  
  #C7 | Table and chart showing the injuries, fatalities, loss per hour of the day summed over all years
  c7Data <- function(state)
  {
    #Check for 12 or 24 hour time format
    if (getHourFormat())
    {
      hours24 <- as.data.frame(c("00:00", "01:00", "02:00", "03:00", "04:00", "05:00", "06:00", "07:00", "08:00", "09:00", "10:00", "11:00", "12:00", "13:00", "14:00", "15:00", "16:00", "17:00", "18:00", "19:00", "20:00", "21:00", "22:00", "23:00"))
      
      c7 <- allTornadoes %>% dplyr::filter(st == state) %>%
        dplyr::group_by(Hour = format(strptime(time, "%H:%M:%S"), format="%H:00")) %>% 
        dplyr::summarise(Injury = sum(inj), Fatality = sum(fat), Loss = sum(loss_updated)) 
      
      c7$Hour <- ordered(c7$Hour, levels = hours24[,])
    }
    else
    {
      hours12 <- as.data.frame(c("12:00 AM", "01:00 AM", "02:00 AM", "03:00 AM", "04:00 AM", "05:00 AM", "06:00 AM", "07:00 AM", "08:00 AM", "09:00 AM", "10:00 AM", "11:00 AM", "12:00 PM", "01:00 PM", "02:00 PM", "03:00 PM", "04:00 PM", "05:00 PM", "06:00 PM", "07:00 PM", "08:00 PM", "09:00 PM", "10:00 PM", "11:00 PM"))
      
      c7 <- allTornadoes %>% dplyr::filter(st == state) %>%
        dplyr::group_by(Hour = format(strptime(time, "%H:%M:%S"), format="%I:00 %p")) %>% 
        dplyr::summarise(Injury = sum(inj), Fatality = sum(fat), Loss = sum(loss_updated)) 
      
      c7$Hour <- ordered(c7$Hour, levels = hours12[,])
    }
    
    c7$Loss <- c7$Loss / 1000000
    
    return(c7)
  }
  
  comprss <- function(tx) { 
    div <- findInterval(as.numeric(gsub("\\,", "", tx)), 
                        c(1, 1e3, 1e6, 1e9, 1e12) )
    paste(round( as.numeric(gsub("\\,","",tx))/10^(3*(div-1)), 2), 
          c("","K","M","B","T")[div] )}
  
  compressBigNumbers <- function(num)
  {
    if (num >= 1e12) {
      paste(format(round(num / 1e12, 1), trim = TRUE), "T")
    } else if (num >= 1e9) {
      paste(format(round(num / 1e9, 1), trim = TRUE), "B")
    } else if (num >= 1e6) {
      paste(format(round(num / 1e6, 1), trim = TRUE), "M")
    } else if (num >= 1e3) {
      paste(format(round(num / 1e3, 1), trim = TRUE), "K")
    } else {
      paste(num)
    }
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
      layout(xaxis = list(fixedrange = TRUE))%>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
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
      layout(xaxis = list(fixedrange = TRUE)) %>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
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
               theme(axis.text.x = element_text(angle = 30)) +
               scale_fill_brewer(type = "seq"), tooltip = c("text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE)) %>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
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
      layout(xaxis = list(fixedrange = TRUE)) %>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  output$c1_state1_table <- renderDT({
    c1 <- c1Data(getState1())
    
    datatable(c1, extensions = 'Scroller', options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE
    ) %>% formatStyle("Year", target = "row", backgroundColor = styleEqual(c(getYearAsNum()), c("gray")))
  })
  
  output$c2_state1_table <- renderDT({
    c2 <- c2Data(getState1())
    
    datatable(c2, extensions = 'Scroller', options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE
    )
  })
  
  output$c3_state1_table <- renderDT({
    c3 <- c3Data(getState1())
    
    datatable(c3, extensions = 'Scroller', options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE
    )
  })
  
  output$c4_state1_table <- renderDT({
    c4 <- c4Data(getState1())
    
    datatable(c4, extensions = 'Scroller', options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
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
      layout(xaxis = list(fixedrange = TRUE))%>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
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
      layout(xaxis = list(fixedrange = TRUE))%>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
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
               theme(axis.text.x = element_text(angle = 30)) +
               scale_fill_brewer(type = "seq"), tooltip = c("text", "fill")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))%>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
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
      layout(xaxis = list(fixedrange = TRUE))%>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  output$c1_state2_table <- renderDT({
    c1 <- c1Data(getState2())
    
    datatable(c1, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE
    ) %>% formatStyle("Year", target = "row", backgroundColor = styleEqual(c(getYearAsNum()), c("gray")))
  })
  
  output$c2_state2_table <- renderDT({
    c2 <- c2Data(getState2())
    
    datatable(c2, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE
    )
  })
  
  output$c3_state2_table <- renderDT({
    c3 <- c3Data(getState2())
    
    datatable(c3, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE
    )
  })
  
  output$c4_state2_table <- renderDT({
    c4 <- c4Data(getState2())
    
    datatable(c4, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE
    )
  })
  
  output$b4_state1_table <- renderDT({
    b4 <- getMostDestructiveTornadoes(getState1())
    
    b4 <- b4 %>% dplyr::select(Date = date, Magnitude = mag, Injuries = inj, Fatalities = fat, Loss = loss_updated, Length = len)
    b4$Loss <- paste0("$", b4$Loss, " (", sapply(b4$Loss, function(x) compressBigNumbers(x)), ")")
    #b4$Loss <- paste0("$", b4$Loss, " (", sapply(b4$Loss, function(x) compressBigNumbers(x), ")"))
    
    datatable(b4, options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "t",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE)
  })
  
  output$b4_state2_table <- renderDT({
    b4 <- getMostDestructiveTornadoes(getState2())
    
    b4 <- b4 %>% dplyr::select(Date = date, Magnitude = mag, Injuries = inj, Fatalities = fat, Loss = loss_updated, Length = len)
    b4$Loss <- paste0("$", b4$Loss, " (", sapply(b4$Loss, function(x) compressBigNumbers(x)), ")")
    
    datatable(b4, options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "t",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE)
  })
  
  #C5 Chart Output | State 1 & 2
  output$c5_state1 <- renderPlotly({
    
    c5 <- c5Data(getState1())
    
    c5 <- reshape2::melt(c5, id.vars = "Year")
    
    ggplotly(ggplot(c5, aes(x = Year, 
                            y = value, 
                            fill = variable,
                            text = paste0(variable, ": ", value))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_fill_manual(values = c("#ff7f00", "#e41a1c", "#4daf4a")), tooltip = c("x", "text")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  output$c5_state2 <- renderPlotly({
    
    c5 <- c5Data(getState2())
    
    c5 <- reshape2::melt(c5, id.vars = "Year")
    
    ggplotly(ggplot(c5, aes(x = Year, 
                            y = value, 
                            fill = variable,
                            text = paste0(variable, ": ", value))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_fill_manual(values = c("#ff7f00", "#e41a1c", "#4daf4a")), tooltip = c("x", "text")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  #C5 Table Output | State 1 & 2
  output$c5_state1_table <- renderDT({
    c5 <- c5Data(getState1())
    
    datatable(c5, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE) %>% formatStyle("Year", target = "row", backgroundColor = styleEqual(c(getYearAsNum()), c("gray")))
  })
  
  output$c5_state2_table <- renderDT({
    c5 <- c5Data(getState2())
    
    datatable(c5, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE) %>% formatStyle("Year", target = "row", backgroundColor = styleEqual(c(getYearAsNum()), c("gray")))
  })
  
  #c6 Chart Output | State 1 & 2
  output$c6_state1 <- renderPlotly({
    
    c6 <- c6Data(getState1())
    c6 <- reshape2::melt(c6, id.vars = "Month")
    
    ggplotly(ggplot(c6, aes(x = Month, 
                            y = value, 
                            fill = variable,
                            text = paste0(variable, ": ", value))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_fill_manual(values = c("#ff7f00", "#e41a1c", "#4daf4a")), tooltip = c("x", "text")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  output$c6_state2 <- renderPlotly({
    
    c6 <- c6Data(getState2())
    c6 <- reshape2::melt(c6, id.vars = "Month")
    
    ggplotly(ggplot(c6, aes(x = Month, 
                            y = value, 
                            fill = variable,
                            text = paste0(variable, ": ", value))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               scale_fill_manual(values = c("#ff7f00", "#e41a1c", "#4daf4a")), tooltip = c("x", "text")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  #C6 Table Output | State 1 & 2
  output$c6_state1_table <- renderDT({
    c6 <- c6Data(getState1())
    
    datatable(c6, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE)
  })
  
  output$c6_state2_table <- renderDT({
    c6 <- c6Data(getState2())
    
    datatable(c6, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE)
  })
  
  #C7 Table Output | State 1 & 2
  output$c7_state1_table <- renderDT({
    c7 <- c7Data(getState1())
    
    datatable(c7, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE)
  })
  
  output$c7_state2_table <- renderDT({
    c7 <- c7Data(getState2())
    
    datatable(c7, extensions = "Scroller", options = list(
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE,
      deferRender = TRUE,
      scrollY = 800,
      scroller = TRUE),
      rownames = FALSE)
  })
  
  output$c7_state1 <- renderPlotly({
    c7 <- c7Data(getState1())
    c7 <- reshape2::melt(c7, id.vars = "Hour")
    
    ggplotly(ggplot(c7, aes(x = Hour, 
                            y = value, 
                            fill = variable,
                            text = paste0(variable, ": ", value))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               theme(axis.text.x = element_text(angle = 30)) +
               scale_fill_manual(values = c("#ff7f00", "#e41a1c", "#4daf4a")), tooltip = c("x", "text")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  output$c7_state2 <- renderPlotly({
    c7 <- c7Data(getState2())
    c7 <- reshape2::melt(c7, id.vars = "Hour")
    
    ggplotly(ggplot(c7, aes(x = Hour, 
                            y = value, 
                            fill = variable,
                            text = paste0(variable, ": ", value))) + 
               geom_bar(stat = "identity") + 
               plotTheme + 
               theme(axis.text.x = element_text(angle = 30)) +
               scale_fill_manual(values = c("#ff7f00", "#e41a1c", "#4daf4a")), tooltip = c("x", "text")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  #C8 | Table and chart showing which counties were most hit by tornadoes summed over all years
  c8Data <- function(state)
  {
    c8 <- allTornadoes %>% 
      group_by(FIPS = stf, f1=f1) %>% 
      summarise(Count = n()) %>% 
      mutate(Percent = (Count / sum(Count) * 100))
    County <- sprintf("%03d",stateFips$FIPS.County)
    stateFips <- cbind(County, stateFips)
    
    #c8$ <- c8$ %>% filter(f1 %in% stateCounties$COUNTYFP | f2 %in% stateCounties$COUNTYFP | f3 %in% stateCounties$COUNTYFP | f4 %in% stateCounties$COUNTYFP)
    c8$State   <- stateFips$State[match(c8$FIPS, stateFips$FIPS.State)] 
    c8$County  <- stateFips$County.Name[match(c8$f1, stateFips$County)] 
    c8         <- na.omit(c8)
    c8$Percent <- format(round(c8$Percent, 2), nsmall = 2)
    c8$Percent <- paste0(c8$Percent, "%")
    
    return(c8)
  }
  
  c8DataByState <- function(state)
  {
    c8 <- allTornadoes %>% dplyr::filter(st == state) %>%
      group_by(FIPS = stf, f1=f1, f2=f2, f3=f3, f4=f4) %>% 
      summarise(Count = n()) %>% 
      mutate(Percent = (Count / sum(Count) * 100))
    County <- sprintf("%03d",stateFips$FIPS.County)
    stateFips <- cbind(County, stateFips)
    
    #c8 <- c8 %>% filter(f1 %in% stateCounties$COUNTYFP | f2 %in% stateCounties$COUNTYFP | f3 %in% stateCounties$COUNTYFP | f4 %in% stateCounties$COUNTYFP)
    c8$State   <- stateFips$State[match(c8$FIPS, stateFips$FIPS.State)] 
    c8$County  <- stateFips$County.Name[match(c8$f1, stateFips$County)] 
    c8         <- na.omit(c8)
    c8$Percent <- format(round(c8$Percent, 2), nsmall = 2)
    c8$Percent <- paste0(c8$Percent, "%")
    
    return(c8)
  }
  
  #C8 Table Output | State 1 & 2
  output$c8_state1_table <- renderDT({
    c8 <- c8DataByState(getState1())
    
    datatable(c8, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  output$c8_state2_table <- renderDT({
    c8 <- c8DataByState(getState2())
    c8$Percent <- NULL
    
    datatable(c8, options = list(
      searching = FALSE,
      pageLength = 10,
      dom = "tp",
      ordering = T,
      lengthChange = FALSE),
      rownames = FALSE
    )
  })
  
  output$c8_state1 <- renderPlotly({
    
    c8 <- c8DataByState(getState1())
    # Limit to top 10 for space of graph
    c8 <- c8[order(-c8$Count), ][1:10,]
    
    ggplotly(ggplot(c8, aes(x = County,
                            y = Count,
                            group = "State",
                            fill = State)) +
               geom_bar(stat = "identity") +
               plotTheme + 
               scale_fill_brewer(type = "qual", palette = "Set1"), tooltip = c("x", "y")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))%>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  output$c8_state2 <- renderPlotly({
    
    c8 <- c8DataByState(getState2())
    # Top 15 for graph room
    c8 <- c8[order(-c8$Count), ][1:15,]
    
    ggplotly(ggplot(c8, aes(x = County,
                            y = Count,
                            group = "State",
                            fill = State )) +
               geom_bar(stat = "identity") +
               plotTheme + 
               scale_fill_brewer(type = "qual", palette = "Set1"), tooltip = c("x", "y")) %>%
      config(staticPlot = FALSE, displayModeBar = FALSE) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      layout(xaxis = list(fixedrange = TRUE))%>% 
      layout(plot_bgcolor='rgba(0, 0, 0, 0)') %>% 
      layout(paper_bgcolor='rgba(0, 0, 0, 0)')
  })
  
  # -----------------------------
  
  
  
  
  
  
  
}



# -----------------------------------------
# what I did to .Rdata file

# 
# linMap <- function(x, from, to)
#   (x - min(x)) / max(x - min(x)) * (to - from) + from
# 
# 
# allTornadoes <- read.csv(file = "data/all_tornadoes.csv", header = TRUE)
# allTornadoes$timestamp <- as.POSIXct(paste(allTornadoes$date, allTornadoes$time), format="%Y-%m-%d %H:%M:%S")
# 
# for (i in 1:nrow(allTornadoes)) {
#   
#   if (allTornadoes$yr[i] <= 1995) { # categories
#     
#     if (allTornadoes$loss[i] <= 0)
#       allTornadoes$loss_updated[i] <- 0
#     else if (allTornadoes$loss[i] == 1)
#       allTornadoes$loss_updated[i] <- 50
#     else if (allTornadoes$loss[i] == 2)
#       allTornadoes$loss_updated[i] <- 250
#     else if (allTornadoes$loss[i] == 3)
#       allTornadoes$loss_updated[i] <- 2500
#     else if (allTornadoes$loss[i] == 4)
#       allTornadoes$loss_updated[i] <- 25000
#     else if (allTornadoes$loss[i] == 5)
#       allTornadoes$loss_updated[i] <- 250000
#     else if (allTornadoes$loss[i] == 6)
#       allTornadoes$loss_updated[i] <- 2500000
#     else if (allTornadoes$loss[i] == 7)
#       allTornadoes$loss_updated[i] <- 25000000
#     else if (allTornadoes$loss[i] == 8)
#       allTornadoes$loss_updated[i] <- 250000000
#     else if (allTornadoes$loss[i] == 9)
#       allTornadoes$loss_updated[i] <- 2500000000
#     else
#       allTornadoes$loss_updated[i] <- allTornadoes$loss[i]
#   }
#   else if (allTornadoes$yr[i] >= 1996 && allTornadoes$yr[i] <= 2015) # multiply times million
#     allTornadoes$loss_updated[i] <- (allTornadoes$loss[i] * 1000000)
#   else if (allTornadoes$yr[i] >= 2016) # dollar amount
#     allTornadoes$loss_updated[i] <- (allTornadoes$loss[i])
#   
# }
# 
# allTornadoes$f1 <- formatC(allTornadoes$f1, width = 3, format = "d", flag = "0")
# allTornadoes$f2 <- formatC(allTornadoes$f2, width = 3, format = "d", flag = "0")
# allTornadoes$f3 <- formatC(allTornadoes$f3, width = 3, format = "d", flag = "0")
# allTornadoes$f4 <- formatC(allTornadoes$f4, width = 3, format = "d", flag = "0")
# 
# injWeight = 1
# fatWeight = 1
# lossWeight = 1
# 
# allTornadoes$injScore <- linMap(allTornadoes$inj, 0, 100) * injWeight
# allTornadoes$fatScore <- linMap(allTornadoes$fat, 0, 100) * fatWeight
# allTornadoes$lossScore <- linMap(allTornadoes$loss, 0, 100) * lossWeight
# 
# allTornadoes <- allTornadoes %>% mutate(destructionScore = injScore + fatScore + lossScore)
# 
# save(allTornadoes, file = "allTornadoes_Final.RData")
# 
