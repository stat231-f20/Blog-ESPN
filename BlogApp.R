library(shiny)
library(ggplot2)
library(tidyverse)
library(png)
library(grid)

plot1 <- read.csv("plot1finished.csv")
plot2 <- read.csv("plot2finished.csv")
footballfield <- png::readPNG("footballfield.png")

PassingStats <- read.csv("PassStats.csv")
CompletionPercentage <- read.csv("CompletionPercentage.csv")
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
      tabPanel("Plot of Passing Play Outcomes", plotOutput("Plot")
      ),
      tabPanel("Table of Passing Play Outcomes",  DT::dataTableOutput("Table_plot")
      ),
      tabPanel("Overall Passing Statistics", DT::dataTableOutput("Table")
    ),
      tabPanel("Completion Percentage", DT::dataTableOutput("Table2"))
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
  
  output$Plot <- renderPlot(
    
    ggplot(data = use_data(), aes_string(x = "x", y = "y", shape = "event", color = "event"))+
      annotation_custom(rasterGrob(footballfield, 
                                   width = unit(1,"npc"), 
                                   height = unit(1,"npc")), 
                        -Inf, Inf, -Inf, Inf) + 
      geom_point(size = 3)+
      xlim(0,120)+
      ylim(-5,60)+
      scale_color_manual(values=c('orange','yellow', 'red', 'blue'))+
      scale_shape_manual(values=c(15, 16, 17, 18))
    
  )
  
  output$Table_plot <- DT::renderDataTable({
    DT::datatable(use_data2())
  })

    output$Table <- DT::renderDataTable({
      DT::datatable(PassingStats)
    })
    
    output$Table2 <- DT::renderDataTable({
      DT::datatable(CompletionPercentage)
    })
}
  
  






shinyApp(ui = ui, server = server)