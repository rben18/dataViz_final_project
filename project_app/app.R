#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team 2 Final Project"),
    h4("We will be doing our analysis based on X dataset"),
    tabsetPanel(
        tabPanel("Rodrigo",
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput("bins",
                                     "Number of bins:",
                                     min = 1,
                                     max = 50,
                                     value = 30)
                         ),
                     mainPanel(
                         plotOutput("distPlot")
                         )
                     )
                 ),
        tabPanel("Raj",
                 sidebarLayout(
                     sidebarPanel(
                         
                     ),
                     mainPanel(
                         
                         )
                     )
                 ),
        tabPanel("Evan",
                 sidebarLayout(
                     sidebarPanel(
                         
                     ),
                     mainPanel(
                         
                         )
                     )
                 ),
        tabPanel("Charle",
                 sidebarLayout(
                     sidebarPanel(
                         
                     ),
                     mainPanel(
                         
                         )
                     )
                 )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
