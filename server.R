function(input, output, session) {
  
  output$acc_variables <- renderTable({
    data.frame(
      Variable = c("Score total (Total Cup Points)", "Critères sensoriels", "Pays d'origine",
                   "Altitude", "Année de récolte", "Méthode de production", "Compatibilité"),
      Définition = c(
        "La note globale d'un lot : la somme des notes des dégustateurs.",
        paste0("Les notes de goût du lot : ", paste(unname(ATTR_LAB[attr_cols]), collapse = ", "),
               ". Nous mettons en avant quatre axes : sucré, acide, arôme et corps."),
        "Le pays où le café a été cultivé.",
        "L'altitude de la plantation (moyenne quand elle est donnée en fourchette).",
        "L'année de la récolte.",
        "Le traitement des cerises après la récolte : lavé, nature (séchage de la cerise entière), semi-lavé, honey / pulped natural.",
        "Calculée par l'application : proximité entre vos goûts et le profil moyen d'un pays."),
      Échelle = c("environ 0 à 100 ; 80 et plus : café de spécialité", "0 à 10 par critère",
                  "texte", "mètres", "année", "catégories",
                  "0 à 100, relative : 100 = pays le plus proche de vos goûts"),
      check.names = FALSE)
  }, striped = TRUE, spacing = "s")
  
  showModal(
    modalDialog(
      div(
        class = "accueil",
        image_accueil,
        h2("Choisir son café"),
        p(
          "Parcourez la carte des cafés du monde, découvrez leurs profils de goût",
          "et laissez le conseiller vous orienter selon vos envies."
        )
      ),
      footer = modalButton("Découvrir"),
      easyClose = TRUE,
      size = "m",
      fade = TRUE
    )
  )
  
  filtre <- reactive({
    d <- cafe
    if (length(input$f_pays)) d <- d[d$pays %in% input$f_pays, ]
    dans <- function(x, b, garde_na) (!is.na(x) & x >= b[1] & x <= b[2]) | (is.na(x) & garde_na)
    d[dans(d$altitude, input$f_alt, input$f_alt_na) & dans(d$annee, input$f_an, input$f_an_na) &
        d$methode %in% input$f_meth & d$espece %in% input$f_esp, ]
  })
  
  
  output$n_lots <- renderText(
    sprintf(
      "%d lots correspondent aux filtres.",
      nrow(filtre())
    )
  )
  
  observeEvent(input$reset_filtres, {
    updateSelectizeInput(session, "f_pays", selected = character(0))
    updateSliderInput(session, "f_alt", value = c(0, alt_max))
    updateSliderInput(session, "f_an", value = an_range)
    updateCheckboxInput(session, "f_alt_na", value = TRUE)
    updateCheckboxInput(session, "f_an_na", value = TRUE)
    updateCheckboxGroupInput(session, "f_meth", selected = methodes)
    updateCheckboxGroupInput(session, "f_esp", selected = unique(cafe$espece))
  })
  
  stats_pays <- reactive({
    
    d <- filtre()
    d <- d[!is.na(d$iso), ]
    
    if (!nrow(d))
      return(NULL)
    
    do.call(
      rbind,
      lapply(
        split(d, d$iso),
        function(x)
          data.frame(
            iso = x$iso[1],
            pays = x$pays[1],
            n = nrow(x),
            score = mean(x$score, na.rm = TRUE),
            as.list(colMeans(x[attr_cols], na.rm = TRUE))
          )
      )
    )
  })
  # ---- CARTE -----------------------------------------------------------------
  
  output$carte <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(minZoom = 2)
    ) |>
      setView(
        lng = 10,
        lat = 15,
        zoom = 2
      )
  })
  
  observeEvent(input$fond_osm, {
    
    proxy <- leafletProxy("carte") |>
      clearTiles()
    
    if (isTRUE(input$fond_osm))
      proxy |> addTiles()
  })
  
  carte_prete <- reactiveVal(FALSE)
  observeEvent(input$carte_bounds, carte_prete(TRUE), once = TRUE)
  
  observe({
    req(carte_prete())
    st <- stats_pays()
    
    proxy <- leafletProxy("carte") |>
      clearShapes() |>
      clearControls()
    
    if (is.null(st))
      return()
    
    i <- match(monde$ISO_A3_EH, st$iso)
    v <- st[[input$indicateur]][i]
    
    rng <- range(v, na.rm = TRUE)
    
    if (!is.finite(rng[1]))
      return()
    
    if (diff(rng) == 0)
      rng <- rng + c(-0.5, 0.5)
    
    pal <- colorNumeric(
      c(
        "#f2e4cf",
        "#d2a572",
        "#a56a35",
        "#6b3f1f",
        "#33190d"
      ),
      rng,
      na.color = "#e9dfd0"
    )
    
    
    etiq <- ifelse(
      is.na(i),
      sprintf(
        "<b>%s</b><br>pas de lot",
        monde$NAME
      ),
      sprintf(
        "<b>%s</b><br>%d lots<br>Score moyen : %.1f",
        st$pays[i],
        st$n[i],
        st$score[i]
      )
    )
    
    nom_ind <- names(IND)[IND == input$indicateur]
    
    proxy |>
      addPolygons(
        data = monde,
        layerId = ~ISO_A3_EH,
        fillColor = pal(v),
        fillOpacity = 0.85,
        color = "white",
        weight = 0.7,
        label = lapply(etiq, HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#333"
        )
      ) |>
      addLegend(
        "bottomright",
        pal = pal,
        values = v,
        title = nom_ind,
        opacity = 0.9
      )
  })
  
  output$table_carte <- renderTable({
    st <- stats_pays(); req(st)
    st <- st[order(-st[[input$indicateur]]), ]
    out <- data.frame(Pays = st$pays, Lots = st$n,
                      `Score total moyen` = round(st$score, 2), check.names = FALSE)
    for (a in attr_cols) out[[ATTR_LAB[[a]]]] <- round(st[[a]], 2)
    out
  }, striped = TRUE, spacing = "xs")
  
  pays_clic <- reactiveVal(NULL)
  
  observeEvent(
    input$carte_shape_click,
    pays_clic(input$carte_shape_click$id)
  )
  
  output$detail_pays <- renderUI({
    
    iso <- pays_clic()
    
    if (is.null(iso))
      return(
        helpText(
          "Cliquez sur un pays pour voir le détail de ses cafés."
        )
      )
    
    d <- d_pays()
    
    if (!nrow(d))
      return(
        helpText(
          "Aucun lot pour ce pays avec les filtres actuels."
        )
      )
    
    tagList(
      
      h4(d$pays[1]),
      
      p(
        sprintf(
          "%d lots - score moyen %.1f - altitude médiane %s m",
          nrow(d),
          mean(d$score, na.rm = TRUE),
          ifelse(
            all(is.na(d$altitude)),
            "n.d.",
            round(median(d$altitude, na.rm = TRUE))
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          plotlyOutput(
            "hist_pays",
            height = 260
          )
        ),
        
        column(
          6,
          h5("Les 10 meilleurs lots"),
          tableOutput("top_pays")
        )
      )
    )
  })
  
  d_pays <- reactive({
    d <- filtre()
    d[
      !is.na(d$iso) &
        d$iso == pays_clic(),
    ]
  })
  
  output$hist_pays <- renderPlotly({
    
    plot_ly(
      d_pays(),
      x = ~score,
      type = "histogram",
      marker = list(color = "#8c5a2b")
    ) |>
      layout(
        xaxis = list(title = "Score total"),
        yaxis = list(title = "Lots")
      ) |>
      theme_cafe() |>
      config(displayModeBar = FALSE)
  })
  
  output$top_pays <- renderTable({
    
    d <- head(
      d_pays()[order(-d_pays()$score), ],
      10
    )
    
    data.frame(
      Score = round(d$score, 1),
      Méthode = d$methode,
      `Altitude (m)` = round(d$altitude),
      Année = d$annee,
      Variété = d$variete,
      check.names = FALSE
    )
    
  }, na = "n.d.", striped = TRUE, spacing = "xs")
  # ---- PROFIL SENSORIEL ------------------------------------------------------
  
  cols_sens <- reactive(if (input$s_tous) attr_cols else KEY)
  
  # moyennes par pays, axe commun et référence : calculés une seule fois
  radar_data <- reactive({
    req(length(input$s_pays) > 0)
    d <- filtre(); cols <- cols_sens()
    moy <- lapply(setNames(input$s_pays, input$s_pays), function(p)
      colMeans(d[d$pays == p, cols, drop = FALSE], na.rm = TRUE))
    v <- unlist(moy); v <- v[is.finite(v)]
    req(length(v) > 0)
    list(moy = moy, labs = unname(ATTR_LAB[cols]),
         axe = c(floor(min(v) * 2) / 2 - 0.5, ceiling(max(v) * 2) / 2 + 0.25),
         ref = colMeans(d[cols], na.rm = TRUE))
  })
  
  radar_trace <- function(p, m, labs, nom, couleur = NULL)
    add_trace(p, r = c(m, m[1]), theta = c(labs, labs[1]), name = nom,
              line = if (!is.null(couleur)) list(color = couleur))
  
  output$radar <- renderPlotly({
    r <- radar_data()
    p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
    for (k in seq_along(r$moy)) p <- radar_trace(p, r$moy[[k]], r$labs, names(r$moy)[k])
    layout(p, polar = list(radialaxis = list(range = r$axe)),
           legend = list(orientation = "h")) |> theme_cafe()
  })
  
  # un radar par pays, deux par ligne, sur la même échelle
  COUL_PAYS <- c("#6f4e37", "#c27c3c", "#7d8a5b", "#a8503f")
  output$radars_pays <- renderUI({
    n <- length(input$s_pays); req(n > 0)
    lignes <- unname(split(seq_len(n), ceiling(seq_len(n) / 2)))
    tagList(lapply(lignes, function(ids)
      fluidRow(lapply(ids, function(k)
        column(6, plotlyOutput(paste0("radar_", k), height = 380))))))
  })
  for (k in 1:4) local({
    kk <- k
    output[[paste0("radar_", kk)]] <- renderPlotly({
      r <- radar_data(); req(length(r$moy) >= kk)
      nom <- names(r$moy)[kk]
      plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself") |>
        radar_trace(r$moy[[kk]], r$labs, nom, COUL_PAYS[kk]) |>
        layout(title = list(text = nom, x = 0.5), margin = list(t = 60), showlegend = FALSE,
               polar = list(radialaxis = list(range = r$axe))) |>
        theme_cafe()
    })
  })
  
  output$mots <- renderUI({
    r <- radar_data()
    tagList(lapply(names(r$moy), function(p)
      tags$p(strong(p), ":", phrase_gout(r$moy[[p]], r$ref),
             "(par rapport à l'ensemble des lots filtrés)")))
  })
  
  