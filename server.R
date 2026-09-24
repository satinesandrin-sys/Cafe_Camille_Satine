#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define server logic required to draw a histogram
function(input, output, session) {
  output$plot <- renderPlot({
    plot(cars)
  })
  
  output$table <- renderTable({
    head(cars)
  })
}

