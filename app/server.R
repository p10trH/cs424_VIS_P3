
# Define Server
# -----------------------------

# getYear < - as.numeric(input$slider_years)


# -----------------------------
server <- function(input, output) {
  
  message("SDSDS")
  
  #getYear <- reactive({
  #  as.numeric(input$slider_years)
  #})
  
  output$selected_var <- renderText({ 
    "You have selected this"
  })
  
 
}
