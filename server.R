function(input, output, session) {
  #Popup bienvenue
  showModal(modalDialog(
    title = "Bienvenue sur CaféViz",
    tags$img(src = "cafe.jpg",
             style = "display: block; margin: 0 auto; width: 50%;"),
    p("Truc à dire sur le projet.", style = "text-align: center; margin-top: 15px;"),
    easyClose = TRUE,
    footer = NULL
  ))
  #Histogramme cafés 
  output$plot <- renderPlot({
    hist(cafe[[input$var]],
         breaks = input$bins,
         col = input$col,
         border = "white",
         main = paste("Distribution de la variable", input$var),
         xlab = input$var)
  })
  #Filtre
  cafe_filtre <- reactive({
    d <- cafe
    if (input$pays != "Tous")    d <- d[d$Country.of.Origin == input$pays, ]
    if (input$variete != "Tous") d <- d[d$Variety_clean == input$variete, ]
    if (input$methode != "Tous") d <- d[d$Processing_clean == input$methode, ]
    d <- d[!is.na(d$Altitude_num) &
             d$Altitude_num >= input$altitude_range[1] &
             d$Altitude_num <= input$altitude_range[2], ]
    if (input$annee != "Tous")   d <- d[d$Harvest_year_clean == input$annee, ]
    d
  })
  
  #Carte du monde 
  map_summary <- reactive({
    cafe_filtre() %>%
      group_by(Country) %>%
      summarise(
        n = n(),
        score_moyen = mean(`Total.Cup.Points`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      left_join(country_centroids, by = "Country") %>%
      mutate(Country_label = clean_country_name(Country))
  })
  output$world_map <- renderLeaflet({
    df <- map_summary()
    
    pal <- colorNumeric("YlOrBr", domain = df$score_moyen)
    
    leaflet(df) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng = 10, lat = 15, zoom = 2) %>%
      addCircleMarkers(
        lng = ~lng, lat = ~lat,
        layerId = ~Country,
        radius = ~pmax(6, sqrt(n) * 4),
        color = ~pal(score_moyen),
        fillOpacity = 0.8, stroke = TRUE, weight = 1,
        label = ~paste0(Country_label, " — ", n, " cafes, score moyen ",
                        round(score_moyen, 2)),
        popup = ~paste0("<b>", Country_label, "</b><br/>",
                        "Nombre de cafes : ", n, "<br/>",
                        "Score moyen : ", round(score_moyen, 2))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~score_moyen,
                title = "Score moyen")
  })
  #Table des cafés
  output$table <- renderTable({
    d <- cafe_filtre()[, c("Country.of.Origin", "Region_clean", "Variety_clean", "Processing_clean",
                           "Altitude_num", "Aroma", "Flavor", "Acidity", "Total.Cup.Points")]
    names(d) <- c("Pays d'origine", "Région", "Variété", "Méthode de traitement",
                  "Altitude", "Arômes", "Saveurs", "Acidité", "Total des points")
    head(d, 50)
  }, striped = TRUE, digits = 2)
}