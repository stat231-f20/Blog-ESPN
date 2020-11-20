library(shiny)
library(ggplot2)
library(tidyverse)
library(png)
library(grid)




plot3 <- read.csv("Teamstats.csv")
plot4 <- read.csv("teamstatsmap.csv")
Teamstats2 <- read.csv("Teamstats2.csv")
footballfield <- png::readPNG("footballfield.png")
PassingStats <- read.csv("passstats.csv")  



Team <-  c("ARI", "NYJ", "OAK", "SF", "JAX", "NYG", "TB", "BUF", "CIN", "DEN", "DET", "GB", "ATL", "CAR", "CLE", "MIA", "WAS", "MIN", "PHI",
           "PIT", "TEN", "BAL", "DAL", "IND", "SEA", "HOU", "NE", "CHI", "KC", "LAC", "LA", "NO")
SeasonStats <- c("W", "L", "T", "AverageYards", "CompletionRate")
ui <- fluidPage(
  
  titlePanel("NFL Passing"),
  
  mainPanel(
    selectInput(inputId = "Team", label = "NFL Teams:", choices = Team), 
    selectInput(inputId = "SeasonStats", label = "Choose a Season Statistic:", choices = SeasonStats),
    tabsetPanel(
      
      tabPanel("Averages vs. New England Patriots", plotOutput("Plot2")
      ),
      tabPanel("Season Statistics By Team", DT::dataTableOutput("Table")
      ),
      tabPanel("Total Passing Yards vs. New England Patriots", plotOutput("Plot3")
      ),
      tabPanel("Passsing Statistics By Team", DT::dataTableOutput("Table2"))
    )
  )
  
)



server <- function(input, output) {
  
  use_data3 <- reactive({
    data <- plot3 %>%
      filter(Team == input$Team | Team == "NE")
  })
  use_data4 <- reactive({
    data <- PassingStats %>%
      filter(Team == input$Team | Team == "NE")
  })
  
  
  output$Plot2 <- renderPlot(
    ggplot(data = use_data3(), aes_string(x = "Team", y = input$SeasonStats, fill = "Team")) + 
      geom_bar(position = "dodge", stat = "identity")
  )
  output$Plot3 <- renderPlot(
    ggplot(data = use_data4(), aes_string(x = "Team", y = "TotalYards", fill = "Team")) + 
      geom_bar(position = "dodge", stat = "identity"),
  )
  output$Table2 <- DT::renderDataTable(
    DT::datatable(PassingStats)
  )
  output$Table <- DT::renderDataTable(
    DT::datatable(plot3)
  )
  
}








shinyApp(ui = ui, server = server)


