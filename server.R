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
  
  output$coffee_table <- renderDT({
    cafe_filtre() %>%
      select(
        Country.of.Origin,
        Region_clean,
        Variety_clean,
        Processing_clean,
        Altitude_num,
        Total.Cup.Points
      ) %>%
      datatable()
  })
  
  output$score_plot <- renderPlot({
    ggplot(
      cafe_filtre(),
      aes(x = Total.Cup.Points)
    ) +
      
      geom_histogram(
        bins = 20,
        fill = "#8B5A2B",
        color = "white"
      ) +
      
      labs(
        title = "Distribution des scores",
        x = "Total Cup Points",
        y = "Nombre de cafés"
      ) +
      
      theme_minimal()
  })
  
  
  # ---- Onglet Analyses : Régression -----------------------------------------
  reg_data <- reactive({
    cafe %>% filter(!is.na(Altitude_num), !is.na(Total.Cup.Points))
  })
  
  reg_model <- reactive({
    lm(Total.Cup.Points ~ Altitude_num, data = reg_data())
  })
  
  output$reg_plot <- renderPlot({
    p <- ggplot(reg_data(), aes(x = Altitude_num, y = Total.Cup.Points))
    
    if (input$reg_par_variete) {
      p <- p + geom_point(aes(color = Variety_clean), alpha = 0.5)
    } else {
      p <- p + geom_point(alpha = 0.4, color = "#8B5A2B")
    }
    
    p +
      geom_smooth(method = "lm", color = "#5A3A1A", se = TRUE) +
      labs(title = "Score total en fonction de l'altitude",
           x = "Altitude (m)", y = "Total Cup Points", color = "Variété") +
      theme_minimal()
  })
  
  output$reg_summary <- renderPrint({
    summary(reg_model())
  })
}
 
# ---- Onglet "Trouve ton café" ---------------------------------------------
extraire_criteres_texte <- function(texte) {
  t <- normaliser_texte(texte)
  
  pays_trouves <- names(alias_pays)[sapply(alias_pays, function(a) any(sapply(a, grepl, t, fixed = TRUE)))]
  pays_trouves <- c(pays_trouves,
                    choix_pays[sapply(tolower(choix_pays), grepl, t, fixed = TRUE)])
  
  variete_trouvee <- choix_variete[sapply(tolower(choix_variete), grepl, t, fixed = TRUE)]
  
  methode_trouvee <- names(alias_methode)[sapply(alias_methode, function(a) any(sapply(a, grepl, t, fixed = TRUE)))]
  
  descripteurs_trouves <- names(synonymes_descripteurs)[
    sapply(synonymes_descripteurs, function(a) any(sapply(a, grepl, t, fixed = TRUE)))
  ]
  
  list(pays = unique(pays_trouves), variete = unique(variete_trouvee),
       methode = unique(methode_trouvee), descripteurs = descripteurs_trouves)
}

resultats_recherche <- eventReactive(input$chercher_cafe, {
  texte <- input$recherche_texte
  validate(need(!is.null(texte) && nchar(trimws(texte)) > 0,
                "Tape une phrase décrivant le café que tu recherches."))
  
  criteres <- extraire_criteres_texte(texte)
  rien_reconnu <- length(criteres$pays) == 0 && length(criteres$variete) == 0 &&
    length(criteres$methode) == 0 && length(criteres$descripteurs) == 0
  validate(need(!rien_reconnu,
                "Aucun terme reconnu dans ta phrase. Essaie avec un pays, une méthode, ou un mot comme \"fruité\", \"corsé\", \"doux\"..."))
  
  d <- cafe
  if (length(criteres$pays) > 0)    d <- d %>% filter(Country.of.Origin %in% criteres$pays)
  if (length(criteres$variete) > 0) d <- d %>% filter(Variety_clean %in% criteres$variete)
  if (length(criteres$methode) > 0) d <- d %>% filter(Processing_clean %in% criteres$methode)
  
  filtres_actifs <- length(criteres$pays) + length(criteres$variete) + length(criteres$methode) > 0
  elargi <- FALSE
  if (nrow(d) < 5 && filtres_actifs) { d <- cafe; elargi <- TRUE }
  
  cible <- sapply(vars_profil, function(v) mean(cafe[[v]], na.rm = TRUE))
  for (desc in criteres$descripteurs) {
    bump <- descripteurs_profil[[desc]]
    for (v in names(bump)) cible[v] <- cible[v] + bump[v] * sd(cafe[[v]], na.rm = TRUE)
  }
  
  cible_proj <- as.numeric(
    predict(pca_profil, newdata = as.data.frame(t(cible)))[, 1:k_composantes, drop = FALSE]
  )
  
  ids_dispo   <- intersect(d$.id, ids_pca)
  scores_pca  <- pca_profil$x[ids_pca %in% ids_dispo, 1:k_composantes, drop = FALSE]
  ids_valides <- ids_pca[ids_pca %in% ids_dispo]
  
  distances <- sqrt(rowSums(sweep(scores_pca, 2, cible_proj)^2))
  top5_ids  <- ids_valides[order(distances)][seq_len(min(5, length(distances)))]
  
  list(cafes = cafe %>% filter(.id %in% top5_ids) %>% arrange(match(.id, top5_ids)),
       elargi = elargi, criteres = criteres)
})
