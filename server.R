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
    tags$img(src = "cafe.jpg",
             style = "display: block; margin: 0 auto; width: 50%;"),
    p("Truc à dire sur le projet.", style = "text-align: center; margin-top: 15px;"),
    easyClose = TRUE,
    footer = NULL
  ))
  
  output$plot <- renderPlot({
    hist(cafe[[input$var]],
         breaks = input$bins,
         col = input$col,
         border = "white",
         main = paste("Distribution de la variable", input$var),,
         xlab = input$var)
  })
  
  output$table <- renderTable({
    d <- cafe
    if (input$pays != "Tous") d <- d[d$Country.of.Origin == input$pays, ]
    head(d[, c("Country.of.Origin", "Region", "Variety", "Processing.Method",
               "Aroma", "Flavor", "Acidity", "Total.Cup.Points")], 50)
  }, striped = TRUE, digits = 2)
}