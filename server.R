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
  # Résumé par pays : nombre de cafés et score moyen
  map_summary <- reactive({
    cafe_filtre() %>%
      filter(!is.na(Country.of.Origin)) %>%
      group_by(Country.of.Origin) %>%
      summarise(
        n = n(),
        score_moyen = mean(`Total.Cup.Points`, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      # country_centroids a une colonne "Country" : d'où le by = c(... = ...).
      # inner_join écarte les pays sans coordonnées (lat/lng = NA feraient
      # planter la carte avec l'erreur "[object Object]").
      inner_join(country_centroids, by = c("Country.of.Origin" = "Country")) %>%
      mutate(Country_label = clean_country_name(Country.of.Origin)) %>%
      as.data.frame()
  })
  
  # Fond de carte dessiné UNE seule fois (OpenStreetMap : gratuit, sans clé API)
  output$world_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 10, lat = 15, zoom = 2)
  })
  
  # Les bulles sont mises à jour à chaque changement de filtre, sans
  # reconstruire la carte (le zoom de l'utilisateur est conservé)
  observe({
    d <- map_summary()
    
    proxy <- leafletProxy("world_map") %>%
      clearMarkers() %>%
      clearControls()
    
    if (nrow(d) == 0) return()   # aucun café avec ces filtres
    
    pal <- colorNumeric("YlOrBr", domain = domaine_score)
    
    proxy %>%
      addCircleMarkers(
        data = d,
        lng = ~lng, lat = ~lat,
        layerId = ~Country.of.Origin,
        radius = ~pmax(6, sqrt(n) * 4),
        color = ~pal(score_moyen),
        fillOpacity = 0.8, stroke = TRUE, weight = 1,
        label = ~paste0(Country_label, " — ", n, " cafés, score moyen ",
                        round(score_moyen, 2)),
        popup = ~paste0("<b>", Country_label, "</b><br/>",
                        "Nombre de cafés : ", n, "<br/>",
                        "Score moyen : ", round(score_moyen, 2))
      ) %>%
      addLegend("bottomright", pal = pal, values = domaine_score,
                title = "Score moyen")
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
