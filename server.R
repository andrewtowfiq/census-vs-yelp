library(shiny)
library(dplyr)
library(ggplot2)
library(randomcoloR)
library(ggmap)
library(plotly)
library(Hmisc)
source("spatial_utils.R")

###################### DATA FRAMES ######################

data.race <- read.csv("data/income.csv", stringsAsFactors = T)
data.race.df <- as.data.frame(data.race) %>%
  select(County.name, Population, White, Black, Native, Asian, Islander, Hispanic)

data.age <- read.csv("data/age_groups_washington.csv", stringsAsFactors = T)
data.age.df <- as.data.frame(data.age) %>%
  select(Area.Name:Female)

data.income <- read.csv("data/income.csv", stringsAsFactors = T)

color.frame <- data.frame(data.income$lowercase, randomColor(39), stringsAsFactors = F)
colnames(color.frame) <- c("subregion", "color")

data.crime <- read.csv("data/crime.csv", stringsAsFactors = T)
data.crime.df <- as.data.frame(data.crime) %>%
  select(year, county, "SRS_BURGLARY") %>%
  filter( year < 2011)


###################### SHINY SERVER ######################

# Define server logic required
shinyServer(function(input, output) {
  ############## FOR TESTING PURPOSES ONLY ##############
  output$out.text <- renderText({
    
    input.county <- GetCountyAtPoint(input$plot_click$x, input$plot_click$y)
    input.county <- capitalize(substr(input.county, 12, 1000000))
    
    if( is.na(input.county) ) { 
      return(paste0("You Have Not Selected a County"))
    } else (
      return(paste0("You Have Selected ",input.county, " County"))  
    )
  })
  
  filtered <- reactive({
    if (input$parameter.key == "Income") {
      data.filtered <- data.income
    } else if (input$parameter.key == "Age") {
      data.filtered <- data.age.df
    } else if (input$parameter.key == "Ethnicity") {
      data.filtered <- data.race.df
    } else if(input$parameter.key == "Burglary"){
      data.filtered <- data.crime.df
    } else {
      data.filtered <- NULL 
    }
    return(data.filtered)
  })
  
  output$trial <- renderText({
    input.county <- GetCountyAtPoint(input$plot_click$x, input$plot_click$y)
    input.county <- capitalize(substr(input.county, 12, 1000000))
    return(input.county)
  })
  
  output$table <- renderTable({
    
    input.county <- GetCountyAtPoint(input$plot_click$x, input$plot_click$y)
    input.county <- capitalize(substr(input.county, 12, 1000000))
    
      final.df <- filtered()
      print(final.df)
      print(input.county)
      final.df <- filter(final.df, County == input.county)
      print(final.df)
  })
  
  # Creates Base Map
  output$base.map <- renderPlot({
      states <- map_data("state")
      west_coast <- subset(states, region %in% c("washington"))
      only_wa <- subset(states, region == "washington")
      
      #creates Data Frame of Washington County with information about different counties that will sever as main data to create map
      all.counties <- map_data("county")
      washington_county <- subset(all.counties, region == "washington")
      head(washington_county)
      #Adds the different colour values to the respective counties in washington.
      washington_county <- left_join(washington_county, color.frame)
      
      #Creates the actual map
      create_base <- ggplot(data = only_wa,
        mapping = aes(x = long, y = lat, group = group)) +
        coord_fixed(1.3) +
        geom_polygon(color = "black", fill = "white") #color = perimeter of map , fill = whole map color fill
      final_base <- create_base + theme_nothing() +
        geom_polygon(data = washington_county,
                     fill = washington_county$color,
                     color = NA) + # color = color of county outline
        geom_polygon(color = "black", fill = NA) # color = perimeter of map that should be kept same as previous color
    return(final_base)
  })
  
  #creates a bar graph for age distrtibution for a county
  output$plotly <- renderPlotly({
    
    input.county <- GetCountyAtPoint(input$plot_click$x, input$plot_click$y)
    input.county <- capitalize(substr(input.county, 12, 1000000))
    
    if (input$parameter.key == "Age") {
      final.df <- filtered()
      curr.county.age.df <- filter(final.df, Area.Name == input.county & Age.Group != "Total")
      totals.num <- as.numeric(gsub(",", "", curr.county.age.df$Total))
      p <- plot_ly(x = curr.county.age.df$Age.Group,
              y = totals.num,
              type = "bar") %>%
        layout(yaxis = list(title = 'Population'), xaxis = list(title = 'Age Group'))
      
      
    } else if (input$parameter.key == "Burglary"){
      final.df <- filtered()
      final.df <- filter(final.df, county == toupper(input.county))
      p <- plot_ly(x = final.df$year,
                   y = final.df$SRS_BURGLARY ,
                   type = "bar") %>%
        layout(yaxis = list(title = 'Number of Burglaries'), xaxis = list(title = 'Year'))
      

    } else if (input$parameter.key == "Ethnicity") {
      final.df <- as.data.frame(filtered())
      View(final.df)
      curr.county.race.df <- filter(final.df, County.name == input.county)
      View(input.county)
      View(curr.county.race.df)
      race.stats <-
        c(
          curr.county.race.df$X2010.White.population,
          curr.county.race.df$X2010.Black.population,
          curr.county.race.df$X2010.Native.population,
          curr.county.race.df$X2010.Asian.population,
          curr.county.race.df$X2010.Islander.population,
          curr.county.race.df$X2010.Hispanic.population
        )
      race.stats.num <- as.numeric(gsub(",", "", race.stats))
      
      p <- plot_ly(
        x = c(
          "White.Population",
          "Black.Population",
          "Native.Population",
          "Asian.Population",
          "Islander.Population",
          "Hispanic.Population"
        ),
        y = race.stats.num,
        type = "bar"
      ) %>%
        layout(yaxis = list(title = 'Population'), xaxis = list(title = 'Race'))
    } else {
      p <- NULL
    }
    return(p)
  })
  
  output$datatable <- renderTable({
    
    input.county <- GetCountyAtPoint(input$plot_click$x, input$plot_click$y)
    input.county <- capitalize(substr(input.county, 12, 1000000))
    
    if (input$parameter.key == "Income") {
      data.table <- filtered()
      data.table <- select(data.table, "County", "Median.household.Income")
      colnames(data.table) <- c("County", "Median Household Income($) ")
      data.table <- filter(data.table, data.table$County == input.county)
    } else {
      data.table <- NULL
    }
      return(data.table)
  })
})
