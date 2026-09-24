#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

function(input, output, session) {
  
  showModal(modalDialog(
    title = "Bienvenue sur CaféViz",
    tags$img(
      src = "cafe.png",
      style = "display: block; margin: 0 auto; width: 50%;"
    ),
    "Truc à dire sur le projet.",
    easyClose = TRUE,
    footer = NULL
  ))
  
  output$plot <- renderPlot({
    hist(cafe[[input$var]],
         breaks = input$bins,
         col = input$col,
         border = "white",
         main = paste("Distribution de", input$var),
         xlab = input$var)
  })
  
  output$table <- renderTable({
    head(cafe, 20)
  })
}
