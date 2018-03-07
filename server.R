library(shiny)
library(dplyr)
library(ggplot2)

# Define server logic required
shinyServer(function(input, output) {

  filtered <- reactive({
    
    if(input$parameter.key == "Overview"){
      
      test.run <- input$parameter.key
      
    } else if(input$parameter.key == "Income"){
      
      test.run <- input$parameter.key
      
    } else if(input$parameter.key == "Age") {
  
      test.run <- input$parameter.key
      
    } else if(input$parameter.key == "Ethnicity") {
      
      test.run <- input$parameter.key
      
    } else {
      
      test.run <- paste(input$parameter.key, "choot")
      
    }
    return(test.run)
  })
  
  # Return the rendered Stuff 
  output$out.text <- renderText({
    return( paste(filtered()) )
  })
})