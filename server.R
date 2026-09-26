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
         main = paste("Distribution de la variable", input$var),
         xlab = input$var)
  })
  
  output$table <- renderTable({
    cafe_filtre <- reactive({
      d <- cafe
      if (input$pays != "Tous")      d <- d[d$Country.of.Origin == input$pays, ]
      if (input$variete != "Tous")   d <- d[d$Variety == input$variete, ]
      if (input$methode != "Tous")   d <- d[d$Processing.Method == input$methode, ]
      if (input$altitude_num != "Tous") d <- d[d$Altitude == input$altitude_num, ]
      d
    })
    
    head(cafe_filtre()[, c("Country.of.Origin", "Region", "Variety", "Processing.Method",
               "Aroma", "Flavor", "Acidity", "Total.Cup.Points")], 50)
  }, striped = TRUE, digits = 2)
}