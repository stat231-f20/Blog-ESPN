library(shiny)
library(ggplot2)
library(tidyverse)
library(png)
library(grid)

plot1 <- read.csv("plot1finished.csv")
footballfield <- png::readPNG("footballfield.png")
PassingStats <- read.csv("passstats.csv")
togo <- c("short", "medium", "long")

direction1 <- c("left", "right")

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
    tabsetPanel(
      tabPanel("Play Stats", plotOutput("Plot")
      ),
      tabPanel("Overall Passing Statistics", DT::dataTableOutput("Table")
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
  
  
  output$Plot <- renderPlot(
    
    ggplot(data = use_data(), aes_string(x = "x", y = "y", color = "event"))+
      annotation_custom(rasterGrob(footballfield, 
                                   width = unit(1,"npc"), 
                                   height = unit(1,"npc")), 
                        -Inf, Inf, -Inf, Inf) + 
      geom_point()+
      xlim(0,120)+
      ylim(-5,60)
    
  )

    output$Table <- DT::renderDataTable({
      DT::datatable(PassingStats)
    })
}
  
  






shinyApp(ui = ui, server = server)