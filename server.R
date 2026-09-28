function(input, output, session) {
  
  # ---- Popup de bienvenue ---------------------------------------------------
  # L'image doit se trouver dans www/cafe.jpg (dossier "www" à côté de ui.R)
  showModal(modalDialog(
    title = "Bienvenue sur CaféViz",
    tags$img(src = "cafe.jpg",
             style = "display: block; margin: 0 auto; width: 50%;"),
    p("Truc à dire sur le projet.", style = "text-align: center; margin-top: 15px;"),
    easyClose = TRUE,
    footer = NULL
  ))
  
  # ---- Onglet Explorer : filtre ---------------------------------------------
  # filter() de dplyr et non d[d$x == y, ] : ce dernier renvoie des lignes
  # remplies de NA quand la colonne contient des NA (cas de Variety_clean...).
  cafe_filtre <- reactive({
    d <- cafe
    if (input$pays != "Tous")    d <- d %>% filter(Country.of.Origin == input$pays)
    if (input$variete != "Tous") d <- d %>% filter(Variety_clean == input$variete)
    if (input$methode != "Tous") d <- d %>% filter(Processing_clean == input$methode)
    if (input$annee != "Tous")   d <- d %>% filter(Harvest_year_clean == as.integer(input$annee))
    
    # Les cafés sans altitude connue sont gardés tant que le slider couvre toute
    # la plage ; dès qu'on le restreint, ils sont écartés.
    alt_complet <- input$altitude_range[1] <= alt_bornes[1] &&
      input$altitude_range[2] >= alt_bornes[2]
    d %>% filter(
      (is.na(Altitude_num) & alt_complet) |
        (Altitude_num >= input$altitude_range[1] & Altitude_num <= input$altitude_range[2])
    )
  })
  
  # ---- Onglet Explorer : carte ----------------------------------------------
  
  map_summary <- reactive({
    cafe_filtre() %>%
      filter(!is.na(Country.of.Origin)) %>%
      group_by(Country.of.Origin) %>%
      summarise(
        n = n(),
        score_moyen = mean(`Total.Cup.Points`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      inner_join(
        country_centroids,
        by = c("Country.of.Origin" = "Country")
      ) %>%
      mutate(
        Country_label = clean_country_name(Country.of.Origin)
      ) %>%
      as.data.frame()
  })
  
  # ---- Onglet Explorer : carte ----------------------------------------------
  
  map_summary <- reactive({
    
    cafe_filtre() %>%
      filter(!is.na(Country.of.Origin)) %>%
      group_by(Country.of.Origin) %>%
      summarise(
        n = n(),
        score_moyen = mean(`Total.Cup.Points`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      inner_join(
        country_centroids,
        by = c("Country.of.Origin" = "Country")
      ) %>%
      mutate(
        Country_label = clean_country_name(Country.of.Origin)
      ) %>%
      as.data.frame()
  })
  

  output$world_map <- renderLeaflet({
    
    d <- map_summary()
    
    # Palette
    pal_map <- colorNumeric(
      palette = "YlOrBr",
      domain = range(d$score_moyen, na.rm = TRUE)
    )
    
    leaflet() %>%
      
      # Fond OpenStreetMap
      addTiles() %>%
      
      # Position initiale
      setView(
        lng = 10,
        lat = 15,
        zoom = 2
      ) %>%
      
      # Cercles
      addCircleMarkers(
        data = d,
        lng = ~lng,
        lat = ~lat,
        
        # Taille proportionnelle au nombre de cafés
        radius = ~pmax(6, sqrt(n) * 4),
        
        # Couleur selon le score moyen
        color = ~pal_map(score_moyen),
        fillColor = ~pal_map(score_moyen),
        fillOpacity = 0.8,
        
        stroke = TRUE,
        weight = 1,
        
        # Étiquette au survol
        label = ~paste0(
          Country_label,
          " — ",
          n,
          " cafés, score moyen ",
          round(score_moyen, 2)
        ),
        
        # Popup au clic
        popup = ~paste0(
          "<b>", Country_label, "</b><br/>",
          "Nombre de cafés : ", n, "<br/>",
          "Score moyen : ", round(score_moyen, 2)
        )
      ) %>%
      
      # Légende
      addLegend(
        "bottomright",
        pal = pal_map,
        values = d$score_moyen,
        title = "Score moyen"
      )
  })
  
  
  # ---- Onglet Explorer : table des cafés ------------------------------------
  output$table <- renderTable({
    d <- cafe_filtre()[, c("Country.of.Origin", "Region_clean", "Variety_clean", "Processing_clean",
                           "Altitude_num", "Aroma", "Flavor", "Acidity", "Total.Cup.Points")]
    names(d) <- c("Pays d'origine", "Région", "Variété", "Méthode de traitement",
                  "Altitude", "Arômes", "Saveurs", "Acidité", "Total des points")
    head(d, 50)
  }, striped = TRUE, digits = 2)
  
  # ---- Onglet Analyses : histogramme ----------------------------------------
  output$plot <- renderPlot({
    hist(cafe[[input$var]],
         breaks = input$bins,
         col = input$col,
         border = "white",
         main = paste("Distribution de la variable", input$var),
         xlab = input$var)
  })
}
