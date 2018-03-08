library(shiny)
library(dplyr)
library(ggplot2)
library(randomcoloR)
library(ggmap)


data.race <- read.csv("data/race.csv", stringsAsFactors = F)
data.income <- read.csv("data/income.csv", stringsAsFactors = F)
color.frame <- data.frame(data.income$County, randomColor(39), stringsAsFactors = F)
colnames(color.frame) <- c("subregion", "color")

# Define server logic required
shinyServer(function(input, output) {

  filtered <- reactive({
    
    if(input$parameter.key == "Overview"){
      
      test.run <- input$parameter.key
      
    } else if(input$parameter.key == "Income"){
      
      test.run <- input$parameter.key
      
    } else if(input$parameter.key == "Crime Rate") {
  
      test.run <- input$parameter.key
      
    } else if(input$parameter.key == "Ethnicity") {
      
      data.table <- 
      test.run <- input$parameter.key
      
    } else {
      
      test.run <- paste(input$parameter.key, "change")
      
    }
    return(test.run)
  })
  
  # Return the rendered Test Text  
  output$out.text <- renderText({
    return( paste(filtered()) )
    
  })
  
  # Creates Base Map
  output$base.map <- renderPlot({
    states <- map_data("state")
    west_coast <- subset(states, region %in% c("washington"))
    only_wa <- subset(states, region == "washington")
    
    all.counties <- map_data("county")
    washington_county <- subset(all.counties, region == "washington")
    head(washington_county)
    
    washington_county <- left_join(washington_county, color.frame)
    
    
    create_base <- ggplot(data = only_wa, mapping = aes(x = long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_polygon(color = "black", fill = "white") #color = perimeter of map , fill = whole map color fill 
    final_base <- create_base + theme_nothing() +
      geom_polygon(data = washington_county, fill = washington_county$color, color = NA) + # color = color of county outline
      geom_polygon(color = "black", fill = NA) # color = perimeter of map that should be kept same as previous color
    return(final_base)
    })
})

