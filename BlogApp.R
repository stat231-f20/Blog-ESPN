library(shiny)
library(ggplot2)
library(tidyverse)
library(png)
library(grid)

plot1 <- read.csv("plot1finished.csv")
plot2 <- read.csv("plot2finished.csv")
plot3 <- read.csv("Teamstats.csv")
footballfield <- png::readPNG("footballfield.png")
PassingStats <- read.csv("PassStats.csv")




togo <- c("short", "medium", "long")
direction1 <- c("left", "right")
Team <-  c( "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", 
            "DAL", "DEN", "DET", "FREE", "GB", "HOU", "IND", "JAX", "KC", "LA", "MIA", "MIN",
            "NE", "NO", "NYG", "NYJ", "OAK", "PHI", "PIT", "SD", "SEA", "SF",
            "STL", "TB", "TEN", "WAS")
SeasonStats <- c("W", "L", "T", "AverageYards", "CompletionRate")
ui <- fluidPage(
  
  titlePanel("NFL Passing"),
  
  sidebarPanel(
    sliderInput(inputId = "down1", label = "Down",
                min = 1, max = 4,
                value = 1),
    selectInput(inputId = "ToGo", label = "Yards To Go", choices = togo),
    sliderInput(inputId = "yardline", label = "Absolute Yardline",
                min = 0, max = 110,
                value = 0),
    selectInput(inputId = "direction", label = "Direction of Play", choices = direction1),
    
  ),
  mainPanel(
    selectInput(inputId = "Team", label = "NFL Teams:", choices = Team), 
    selectInput(inputId = "SeasonStats", label = "Choose a Season Statistic:", choices = SeasonStats),
    tabsetPanel(
      tabPanel("Plot of Passing Play Outcomes", plotOutput("Plot")
      ),
      tabPanel("Averages vs. New England Patriots", plotOutput("Plot2")
      ),
      tabPanel("Season Statistics By Team", DT::dataTableOutput("Table")
      ),
      tabPanel("Table of Passing Play Outcomes",  DT::dataTableOutput("Table_plot")
      )
    
    )
  )
  
)



server <- function(input, output) {
  
  use_data <- reactive({
    data <- plot1 %>%
      filter(down == input$down1)%>%
      filter(togo == input$ToGo) %>%
      filter(absoluteYardlineNumber == input$yardline)%>%
      filter(playDirection == input$direction)
  })
  
  use_data2 <- reactive({
    data <- plot2 %>%
      filter(down == input$down1)%>%
      filter(togo == input$ToGo) %>%
      filter(absoluteYardlineNumber == input$yardline)%>%
      filter(playDirection == input$direction)
  })
  use_data3 <- reactive({
    data <- plot3 %>%
      filter(Team == input$Team | Team == "NE")
  })
  use_data4 <- reactive({
    data <- PassingStats %>%
      filter(Team = input$Team | Team == "NE")
  })
  
  output$Plot <- renderPlot(
    ggplot(data = use_data(), aes_string(x = "x", y = "y", shape = "event", color = "event"))+
      annotation_custom(rasterGrob(footballfield, 
                                   width = unit(1,"npc"), 
                                   height = unit(1,"npc")), 
                        -Inf, Inf, -Inf, Inf) + 
      geom_point(size = 5)+
      xlim(0,120)+
      ylim(-5,60)+
      scale_color_manual(values=c('orange','yellow', 'red', 'blue'))+
      scale_shape_manual(values=c(15, 16, 17, 18))
    
  )
  output$Plot2 <- renderPlot(
    ggplot(data = use_data3(), aes_string(x = "Team", y = input$SeasonStats, fill = "Team")) + 
             geom_bar(position = "dodge", stat = "identity")
  )
  output$Plot3 <- renderPlot(
    ggplot(data = use_data3(), aes_string(x = "Team", y = "TotalYards", fill = "Team")) + 
      geom_bar(position = "dodge", stat = "identity")
  )
  output$Table <- DT::renderDataTable(
    DT::datatable(Teamstats)
  )
  output$Table_plot <- DT::renderDataTable(
    DT::datatable(plot2finished)
  )
}

  






shinyApp(ui = ui, server = server)