library("shiny")
library("shinythemes")

# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = shinytheme("slate"),
                  
  # Title place holder for project 
  titlePanel("Analysis of Factors affecting Restaraunts based on Counties In WA"),
  
  # Creation of SideBar and Widgets 
  sidebarLayout(
    sidebarPanel(
      radioButtons("parameter.key", "Select Parameter",
                   choices = c("Overview", "Income", "Age", "Ethnicity", "Burglary")),
      textOutput("out.text")
      
      #For Testing
      #tableOutput("table")
      ),
    
    mainPanel(
      plotOutput("base.map", width = "100%", click = "plot_click"),
      tableOutput("datatable"),
      plotlyOutput("plotly")
      )
    )
  )
)