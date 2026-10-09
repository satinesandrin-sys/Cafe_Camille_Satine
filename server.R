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
  