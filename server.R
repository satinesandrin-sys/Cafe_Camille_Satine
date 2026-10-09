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
  # ---- CHOIX DE CAFÉ ---------------------------------------------------------
  
  classement <- reactive({
    
    for (a in KEY)
      req(input[[paste0("c_", a)]])
    
    req(!is.null(input$c_meth))
    
    avec_meth <- nzchar(input$c_meth)
    
    d <- cafe[
      !is.na(cafe$pays) &
        !is.na(cafe$score),
    ]
    
    if (avec_meth)
      d <- d[d$methode == input$c_meth, ]
    
    n <- table(d$pays)
    
    d <- d[
      d$pays %in%
        names(n)[
          n >= if (avec_meth) 5 else 10
        ],
    ]
    
    validate(
      need(
        nrow(d) > 0,
        "Pas assez de lots avec ces critères."
      )
    )
    
    qual <- isTRUE(input$c_qual)
    
    dims <- c(
      KEY,
      if (qual) "score"
    )
    
    moy <- aggregate(
      d[c(KEY, "score")],
      by = list(pays = d$pays),
      FUN = mean,
      na.rm = TRUE
    )
    
    nb <- as.integer(
      table(d$pays)[moy$pays]
    )
    
    Z <- scale(moy[dims])
    Z[is.na(Z)] <- 0
    
    cible <- c(
      vapply(
        KEY,
        function(a)
          (input[[paste0("c_", a)]] - 3) * 0.75,
        numeric(1)
      ),
      if (qual) 1.5
    )
    
    dist <- sqrt(
      rowSums(
        sweep(Z, 2, cible)^2
      )
    )
    
    out <- data.frame(
      pays = moy$pays,
      compat = round(
        100 * (1 - dist / max(dist))
      ),
      n = nb,
      score = moy$score
    )
    
    out[order(-out$compat), ]
  })
  
  output$c_classement <- renderTable({
    
    cl <- head(
      classement(),
      5
    )
    
    data.frame(
      Pays = cl$pays,
      `Compatibilité (%)` = cl$compat,
      `Score CQI moyen` = round(cl$score, 1),
      check.names = FALSE
    )
    
  }, striped = TRUE, spacing = "xs")
  
  observe({
    
    cl <- tryCatch(
      classement(),
      error = function(e) NULL
    )
    
    if (!is.null(cl))
      updateSelectInput(
        session,
        "c_pays",
        choices = head(cl$pays, 5),
        selected = cl$pays[1]
      )
  })
  
  output$c_statut <- renderText({
    
    invalidateLater(15000)
    
    paste0(
      "NaileR : ",
      if (NAILER_OK)
        "installé"
      else
        "non installé",
      " | Ollama : ",
      if (ollama_actif())
        "détecté"
      else
        "non détecté (lancez l'application Ollama)"
    )
  })
  
  output$c_profil <- renderUI({
    
    req(input$c_pays)
    
    m <- colMeans(
      cafe[cafe$pays == input$c_pays, KEY],
      na.rm = TRUE
    )
    
    ref <- colMeans(
      cafe[KEY],
      na.rm = TRUE
    )
    
    tags$p(
      strong(input$c_pays),
      ":",
      phrase_gout(m, ref),
      "(par rapport à tous les cafés ; calcul statistique, sans IA)"
    )
  })
  
  msg_interp <- reactiveVal(NULL)
  
  observeEvent(input$c_interp, {
    
    txt <- trimws(
      paste(input$c_texte, collapse = " ")
    )
    
    if (!nzchar(txt))
      return(
        msg_interp(
          list(
            erreur = "Écrivez d'abord quelques mots sur vos envies."
          )
        )
      )
    
    if (!requireNamespace("ollamar", quietly = TRUE))
      return(
        msg_interp(
          list(
            erreur = "le package ollamar n'est pas installé (install.packages('ollamar'))."
          )
        )
      )
    
    meths <- setdiff(
      methodes,
      "Non renseigné"
    )
    
    gabarit <- paste0(
      "{",
      paste0(
        '"',
        KEY,
        '":3',
        collapse = ","
      ),
      ',"methode":"","qualite":false,"explication":"..."}'
    )
    
    prompt <- paste0(
      "Tu aides un consommateur à choisir un café. ",
      "Il décrit ses envies : « ",
      txt,
      " ».\n",
      "Traduis ces envies en niveaux de 1 (pas du tout) à 5 (beaucoup), ",
      "3 = indifférent, pour : ",
      paste0(
        '"',
        KEY,
        '" (',
        ADJ[KEY],
        ")",
        collapse = ", "
      ),
      ".\n",
      "Indique aussi \"methode\" parmi [",
      paste(meths, collapse = ", "),
      "] ou \"\" si rien n'est précisé, ",
      "\"qualite\" à true s'il veut le meilleur café possible, ",
      "et \"explication\" : une phrase en français qui reformule ce que tu as compris. ",
      "Ne traduis en niveaux que les critères listés ci-dessus ; si la personne évoque autre chose, ",
      "ignore-le et dis-le dans l'explication.\n",
      "Réponds uniquement par un objet JSON valide, sans aucun texte autour, de la forme ",
      gabarit
    )
    
    sortie <- withProgress(
      message = "Je lis vos envies...",
      value = 0.5,
      
      tryCatch(
        ollamar::generate(
          model = input$c_model,
          prompt = prompt,
          output = "text",
          stream = FALSE
        ),
        error = function(e) e
      )
    )
    
    if (inherits(sortie, "error"))
      return(
        msg_interp(
          list(
            erreur = conditionMessage(sortie)
          )
        )
      )
    
    json <- regmatches(
      sortie,
      regexpr(
        "\\{[^{}]*\\}",
        sortie
      )
    )
    
    pr <- if (length(json))
      tryCatch(
        jsonlite::fromJSON(json),
        error = function(e) NULL
      )
    else
      NULL
    
    if (is.null(pr))
      return(
        msg_interp(
          list(
            erreur = "réponse du LLM illisible, reformulez ou relancez."
          )
        )
      )
    
    for (a in KEY) {
      
      v <- suppressWarnings(
        as.integer(
          round(
            as.numeric(pr[[a]])
          )
        )
      )
      
      if (
        length(v) == 1 &&
        !is.na(v)
      ) {
        updateSliderInput(
          session,
          paste0("c_", a),
          value = max(
            1,
            min(5, v)
          )
        )
      }
    }
    
    m <- pr[["methode"]]
    
    updateSelectInput(
      session,
      "c_meth",
      selected =
        if (
          is.character(m) &&
          length(m) == 1 &&
          m %in% meths
        )
          m
      else
        ""
    )
    
    updateCheckboxInput(
      session,
      "c_qual",
      value = isTRUE(pr[["qualite"]])
    )
    
    msg_interp(
      list(
        texte =
          if (
            is.character(pr[["explication"]])
          )
            pr[["explication"]]
        else
          "Réglages mis à jour."
      )
    )
  })
  
  output$c_interp_res <- renderUI({
    
    r <- msg_interp()
    
    req(r)
    
    if (!is.null(r$erreur))
      return(
        div(
          class = "alert alert-danger",
          strong("Interprétation impossible : "),
          r$erreur,
          br(),
          "Vérifiez qu'Ollama est lancé et que le modèle est téléchargé."
        )
      )
    
    div(
      class = "alert alert-success",
      strong("J'ai compris : "),
      r$texte,
      " Les curseurs ont été réglés ; vous pouvez les modifier."
    )
  })
  # ---- CONSEILLER ------------------------------------------------------------
  
  reponse <- eventReactive(input$c_go, {
    
    req(input$c_pays)
    
    if (!NAILER_OK)
      return(
        list(
          erreur = "le package NaileR n'est pas installé (devtools::install_github('Sebastien-Le/NaileR'))."
        )
      )
    
    cible <- input$c_pays
    apercu <- isTRUE(input$c_apercu)
    
    txt_libre <- trimws(
      paste(input$c_texte, collapse = " ")
    )
    
    prefs <- paste(
      vapply(
        KEY,
        function(a)
          paste(
            NIVEAUX[
              input[[paste0("c_", a)]]
            ],
            ADJ[[a]]
          ),
        character(1)
      ),
      collapse = ", "
    )
    
    intro <- paste0(
      "Contexte : application d'aide au choix de café pour un consommateur de supermarché. ",
      "Les données sont des lots de café évalués par des dégustateurs (notes de 0 à 10 par critère ; ",
      "altitude de culture en classes ; méthode de production). Le groupe « ",
      cible,
      " » est comparé au groupe « Autres pays ». ",
      "Le consommateur recherche un café : ",
      prefs,
      ".",
      if (nzchar(txt_libre))
        paste0(
          " Il l'exprime ainsi : « ",
          txt_libre,
          " »."
        )
      else
        ""
    )
    
    demande <- paste0(
      "À partir des seuls éléments statistiques fournis, rédige en français, pour un consommateur ",
      "non spécialiste, un conseil de 5 à 8 lignes : décris le goût typique du café du groupe « ",
      cible,
      " », ce qui le distingue des autres pays, et dis s'il correspond aux goûts recherchés, ",
      "en nuançant. Ne cite pas de chiffres techniques ni de p-values. ",
      "N'invente rien qui ne soit pas dans les données."
    )
    
    res <- withProgress(
      message = "Le conseiller rédige sa réponse (une à deux minutes possibles)...",
      value = 0.5,
      
      tryCatch(
        appelle_nailer(
          donnees_llm(cible),
          intro,
          demande,
          input$c_model,
          generate = !apercu
        ),
        error = function(e) e
      )
    )
    
    if (inherits(res, "error"))
      return(
        list(
          erreur = conditionMessage(res)
        )
      )
    
    list(
      texte = texte_nailer(
        res,
        if (apercu)
          "prompt"
        else
          "response"
      ),
      apercu = apercu,
      cible = cible
    )
  })
  
  output$c_reponse <- renderUI({
    
    r <- reponse()
    
    if (!is.null(r$erreur))
      return(
        div(
          class = "alert alert-danger",
          strong("Le conseiller n'a pas pu répondre : "),
          r$erreur,
          br(),
          "Vérifiez qu'Ollama est lancé et que le modèle est téléchargé (ollama pull <modèle>)."
        )
      )
    
    if (r$apercu)
      return(
        tagList(
          h5("Prompt envoyé au LLM (aucun appel effectué)"),
          tags$pre(r$texte)
        )
      )
    
    tagList(
      h5(
        paste(
          "L'avis du conseiller sur :",
          r$cible
        )
      ),
      div(
        class = "avis-conseiller",
        r$texte
      )
    )
  })
  # ---- AFDM ---------------------------------------------------------------------
  afdm <- reactive({
    validate(need(FACTOMINER_OK, "Installez le package FactoMineR : install.packages('FactoMineR')"))
    d <- cafe[!is.na(cafe$pays) & !is.na(cafe$score) & cafe$methode != "Non renseigné", ]
    d$pays_detail <- d$pays
    d$pays <- regroupe(d$pays, input$af_nmin, "Autres pays")
    d$methode <- regroupe(d$methode, input$af_nmin, "Autre")
    quanti <- c(attr_cols, if (isTRUE(input$af_alt)) "altitude")
    d <- d[complete.cases(d[c(quanti, "score", "pays", "methode", "espece")]), ]
    validate(need(nrow(d) > 50, "Trop peu de lots complets : décochez l'altitude."))
    quanti_ini <- quanti
    quanti <- quanti[vapply(quanti, function(a) isTRUE(sd(d[[a]]) > 0), logical(1))]
    quali <- c("pays", "methode", "espece")
    quali <- quali[vapply(quali, function(v) length(unique(d[[v]])) > 1, logical(1))]
    base <- d[c(quanti, quali, "score")]
    for (v in quali) base[[v]] <- factor(base[[v]])
    names(base) <- unname(LAB_AF[names(base)])
    res <- FactoMineR::FAMD(base, ncp = 5, sup.var = ncol(base), graph = FALSE)
    list(res = res, base = base, quali = unname(LAB_AF[quali]), quanti = unname(LAB_AF[quanti]),
         exclues = unname(LAB_AF[setdiff(quanti_ini, quanti)]),
         pays_detail = d$pays_detail)
  })
  
  dims <- reactive({ req(input$af_x, input$af_y); c(as.integer(input$af_x), as.integer(input$af_y)) })
  lab_dim <- function(res, k) sprintf("Dimension %d (%.1f %%)", k, res$eig[k, 2])
  
  output$af_resume <- renderUI({
    a <- afdm()
    tags$p(strong(nrow(a$base)), " lots complets utilisés. Variables numériques : ",
           paste(a$quanti, collapse = ", "), ". Variables qualitatives : ", paste(a$quali, collapse = ", "),
           ". Variable supplémentaire : Score total.",
           if (length(a$exclues)) paste0(" Exclues car constantes : ", paste(a$exclues, collapse = ", "), "."))
  })
  
  # cercle des corrélations : grand, étiquettes écartées des flèches
  output$af_cercle <- renderPlotly({
    res <- afdm()$res; ax <- dims(); req(max(ax) <= ncol(res$ind$coord))
    q <- as.data.frame(res$quanti.var$coord[, ax, drop = FALSE]); names(q) <- c("x", "y")
    q$nom <- rownames(q); q$sup <- FALSE
    v <- q
    sup <- res$quanti.sup$coord
    if (!is.null(sup)) {
      s <- as.data.frame(sup[, ax, drop = FALSE]); names(s) <- c("x", "y")
      s$nom <- rownames(s); s$sup <- TRUE
      v <- rbind(q, s)
    }
    couleur <- ifelse(v$sup, "#d9480f", "#3e2415")
    pos <- ifelse(abs(v$x) >= abs(v$y),
                  ifelse(v$x >= 0, "middle right", "middle left"),
                  ifelse(v$y >= 0, "top center", "bottom center"))
    fleches <- lapply(seq_len(nrow(v)), function(i)
      list(x = v$x[i], y = v$y[i], ax = 0, ay = 0, xref = "x", yref = "y", axref = "x", ayref = "y",
           showarrow = TRUE, arrowhead = 2, arrowsize = 1.2, arrowwidth = 2.5, arrowcolor = couleur[i]))
    plot_ly(v, x = ~x, y = ~y, type = "scatter", mode = "text",
            text = paste0("<b>", v$nom, "</b>"), textposition = pos,
            textfont = list(size = 15, color = couleur),
            hovertext = sprintf("%s : %.2f ; %.2f", v$nom, v$x, v$y), hoverinfo = "text") |>
      layout(shapes = list(
        list(type = "circle", xref = "x", yref = "y", x0 = -1, y0 = -1, x1 = 1, y1 = 1,
             line = list(color = "#b9a58c", width = 1.5)),
        list(type = "line", x0 = -1.35, x1 = 1.35, y0 = 0, y1 = 0,
             line = list(color = "#d9c6ad", dash = "dot")),
        list(type = "line", x0 = 0, x1 = 0, y0 = -1.35, y1 = 1.35,
             line = list(color = "#d9c6ad", dash = "dot"))),
        annotations = fleches, showlegend = FALSE,
        xaxis = list(range = c(-1.35, 1.35), title = lab_dim(res, ax[1]),
                     zeroline = FALSE, showgrid = FALSE, tickvals = seq(-1, 1, 0.5)),
        yaxis = list(range = c(-1.35, 1.35), title = lab_dim(res, ax[2]), scaleanchor = "x",
                     zeroline = FALSE, showgrid = FALSE, tickvals = seq(-1, 1, 0.5)),
        margin = list(l = 60, r = 30, t = 20, b = 60)) |>
      theme_cafe() |> config(displayModeBar = FALSE)
  })
  
  # lots dans le plan : couleur selon le vrai pays (les pays très rares passent en gris)
  output$af_ind <- renderPlotly({
    a <- afdm(); res <- a$res; ax <- dims(); req(max(ax) <= ncol(res$ind$coord))
    pays <- a$pays_detail
    eff <- table(pays)
    g <- ifelse(pays %in% names(eff)[eff >= input$af_naff], pays, "Autres pays")
    ord <- names(sort(table(g[g != "Autres pays"]), decreasing = TRUE))
    n_c <- length(ord)
    pal <- if (n_c <= length(COULEURS_POP)) COULEURS_POP[seq_len(n_c)] else
      c(COULEURS_POP, grDevices::hcl.colors(n_c - length(COULEURS_POP), "Dark 3"))
    niveaux <- c(intersect("Autres pays", g), ord)          # le gris est tracé en premier, dessous
    cols <- setNames(c(rep("#c9c2b8", length(niveaux) - n_c), pal), niveaux)
    df <- data.frame(x = res$ind$coord[, ax[1]], y = res$ind$coord[, ax[2]],
                     groupe = factor(g, levels = niveaux))
    # étiquette au centre des nuages des pays d'au moins 10 lots
    cen <- aggregate(cbind(x, y) ~ groupe, data = df, FUN = mean)
    cen <- cen[cen$groupe %in% names(eff)[eff >= 10] & cen$groupe != "Autres pays", ]
    p <- plot_ly(df, x = ~x, y = ~y, color = ~groupe, colors = cols, type = "scatter", mode = "markers",
                 marker = list(size = 8, opacity = 0.8, line = list(color = "white", width = 0.6)),
                 hovertemplate = "%{fullData.name}<extra></extra>")
    if (nrow(cen) > 0)
      p <- add_trace(p, data = cen, x = ~x, y = ~y, text = ~as.character(groupe), type = "scatter",
                     mode = "text", textfont = list(size = 13, color = "#2b1a12"),
                     showlegend = FALSE, hoverinfo = "skip", inherit = FALSE)
    p |>
      layout(xaxis = list(title = lab_dim(res, ax[1])), yaxis = list(title = lab_dim(res, ax[2])),
             legend = list(font = list(size = 11))) |>
      theme_cafe() |> config(displayModeBar = FALSE)
  })
  
  # contributions : une barre par variable, foncée si au-dessus de la contribution moyenne
  contrib_plot <- function(res, k, xmax) {
    ctr <- res$var$contrib[, k]
    df <- data.frame(v = names(ctr), c = as.numeric(ctr)); df <- df[order(df$c), ]
    df$v <- factor(df$v, levels = df$v)
    seuil <- 100 / nrow(df)
    plot_ly(df, x = ~c, y = ~v, type = "bar", orientation = "h",
            marker = list(color = ifelse(df$c >= seuil, "#8c5a2b", "#d9c6ad")),
            hovertemplate = "%{y} : %{x:.1f} %<extra></extra>") |>
      layout(title = list(text = lab_dim(res, k), x = 0.5, font = list(size = 14)),
             xaxis = list(title = "Contribution (%)", range = c(0, xmax)),
             yaxis = list(title = ""), margin = list(t = 50),
             shapes = list(list(type = "line", x0 = seuil, x1 = seuil, y0 = 0, y1 = 1, yref = "paper",
                                line = list(dash = "dot", color = "#555")))) |>
      theme_cafe() |> config(displayModeBar = FALSE)
  }
  xmax_contrib <- reactive({ max(afdm()$res$var$contrib[, dims()]) * 1.1 })
  output$af_contrib1 <- renderPlotly({
    res <- afdm()$res; k <- dims()[1]; req(k <= ncol(res$ind$coord))
    contrib_plot(res, k, max(20, max(res$var$contrib[, k]) * 1.05))
  })
  output$af_contrib2 <- renderPlotly({
    res <- afdm()$res; k <- dims()[2]; req(k <= ncol(res$ind$coord))
    contrib_plot(res, k, xmax_contrib())
  })
  
  output$af_texte <- renderUI({
    res <- afdm()$res; ax <- dims(); req(max(ax) <= ncol(res$ind$coord))
    phrase <- function(k) {
      ctr <- sort(res$var$contrib[, k], decreasing = TRUE)
      tags$p(strong(lab_dim(res, k)), " : surtout portée par ", paste(names(head(ctr, 3)), collapse = ", "), ".")
    }
    div(class = "encart", strong("Lecture rapide"), phrase(ax[1]), phrase(ax[2]))
  })
}