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
  
  # ---- Filtres --------------------------------------------------------------
  # 1) Tous les filtres SAUF le pays : la carte s'appuie sur celui-ci, sinon
  #    cliquer sur un pays ferait disparaître toutes les autres bulles.
  cafe_hors_pays <- reactive({
    d <- cafe
    if (input$variete != "Tous") d <- d %>% filter(Variety_clean == input$variete)
    if (input$methode != "Tous") d <- d %>% filter(Processing_clean == input$methode)
    if (input$annee != "Tous")   d <- d %>% filter(Harvest_year_clean == as.integer(input$annee))
    
    # Curseur Total Cup Points
    d <- d %>% filter(Total.Cup.Points >= input$f_cup[1],
                      Total.Cup.Points <= input$f_cup[2])
    
    # Les cafés sans altitude connue sont gardés tant que le slider couvre toute
    # la plage ; dès qu'on le restreint, ils sont écartés.
    alt_complet <- input$altitude_range[1] <= alt_bornes[1] &&
      input$altitude_range[2] >= alt_bornes[2]
    d %>% filter(
      (is.na(Altitude_num) & alt_complet) |
        (Altitude_num >= input$altitude_range[1] & Altitude_num <= input$altitude_range[2])
    )
  })
  
  # 2) Filtre final : on ajoute le pays (tableau et KPI)
  cafe_filtre <- reactive({
    d <- cafe_hors_pays()
    if (input$pays != "Tous") d <- d %>% filter(Country.of.Origin == input$pays)
    d
  })
  
  # ---- Cartes KPI -----------------------------------------------------------
  output$kpi_n     <- renderText(as.character(nrow(cafe_filtre())))
  output$kpi_pays  <- renderText(as.character(n_distinct(cafe_filtre()$Country.of.Origin)))
  output$kpi_score <- renderText({
    d <- cafe_filtre()
    if (nrow(d) == 0) "–" else sprintf("%.2f", mean(d$Total.Cup.Points, na.rm = TRUE))
  })
  output$kpi_alt <- renderText({
    a <- cafe_filtre()$Altitude_num
    if (all(is.na(a))) "–" else as.character(round(mean(a, na.rm = TRUE)))
  })
  
  # ---- Réinitialisation des filtres -----------------------------------------
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "pays",    selected = "Tous")
    updateSelectInput(session, "variete", selected = "Tous")
    updateSelectInput(session, "methode", selected = "Tous")
    updateSelectInput(session, "annee",   selected = "Tous")
    updateSliderInput(session, "altitude_range",
                      value = c(floor(alt_bornes[1]), ceiling(alt_bornes[2])))
    updateSliderInput(session, "f_cup", value = c(cup_min, cup_max))
  })
  
  # ---- Carte ----------------------------------------------------------------
  # Résumé par pays (sans le filtre pays) : nombre de cafés et score moyen
  map_summary <- reactive({
    cafe_hors_pays() %>%
      filter(!is.na(Country.of.Origin)) %>%
      group_by(Country.of.Origin) %>%
      summarise(
        n = n(),
        score_moyen = mean(Total.Cup.Points, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      inner_join(country_centroids, by = c("Country.of.Origin" = "Country")) %>%
      mutate(Country_label = clean_country_name(Country.of.Origin)) %>%
      as.data.frame()
  })
  
  # Fond de carte dessiné UNE seule fois (le zoom de l'utilisateur est conservé)
  output$world_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 10, lat = 15, zoom = 2)
  })
  
  # Les bulles sont mises à jour à chaque changement de filtre
  observe({
    d <- map_summary()
    
    proxy <- leafletProxy("world_map") %>%
      clearMarkers() %>%
      clearControls()
    
    if (nrow(d) == 0) return()   # aucun café avec ces filtres
    
    # Palette à domaine fixe (défini dans global.R) : couleurs stables
    pal <- colorNumeric("YlOrBr", domain = domaine_score)
    choisi <- d$Country.of.Origin == input$pays   # contour noir sur le pays filtré
    
    proxy %>%
      addCircleMarkers(
        data = d,
        lng = ~lng, lat = ~lat,
        layerId = ~Country.of.Origin,             # nécessaire pour le clic
        radius = ~pmax(6, sqrt(n) * 4),
        fillColor = ~pal(score_moyen),
        color = ifelse(choisi, "#000000", "#ffffff"),
        weight = ifelse(choisi, 3, 1),
        fillOpacity = 0.8, stroke = TRUE,
        label = ~paste0(Country_label, " — ", n, " cafés, score moyen ",
                        round(score_moyen, 2))
      ) %>%
      addLegend("bottomright", pal = pal, values = domaine_score,
                title = "Score moyen")
  })
  
  # Clic sur une bulle : filtre sur ce pays (re-clic = on retire le filtre)
  observeEvent(input$world_map_marker_click, {
    clic <- input$world_map_marker_click$id
    updateSelectInput(session, "pays",
                      selected = if (input$pays == clic) "Tous" else clic)
  })
  
  output$map_selection_info <- renderUI({
    if (input$pays == "Tous") {
      helpText("Clique sur une bulle de la carte pour filtrer sur un pays.")
    } else {
      helpText(paste0("Filtré sur : ", clean_country_name(input$pays),
                      ". Re-clique sur la bulle pour retirer le filtre."))
    }
  })
  
  # ---- Table des cafés ------------------------------------------------------
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