fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@500;600&family=Source+Sans+3:wght@400;600&display=swap"
    ),
    tags$style(HTML(CSS_CAFE))
  ),
  
  div(
    class = "bandeau-equipe",
    
    logo,
    
    div(
      class = "noms-equipe",
      NOMS_EQUIPE
    )
  ),
  
  titlePanel(
    "Choisir son café",
    windowTitle = "Choisir son café"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      conditionalPanel(
        "input.onglet == 'accueil'",
        h4("En bref"),
        CHIFFRES
      ),
      
      conditionalPanel(
        "input.onglet == 'carte' || input.onglet == 'sens'",
        h4("Filtres"),
        
        selectizeInput(
          "f_pays",
          "Pays (vide = tous)",
          pays_liste,
          multiple = TRUE,
          options = list(placeholder = "Tous les pays")
        ),
        
        sliderInput(
          "f_alt",
          "Altitude (m)",
          0,
          alt_max,
          c(0, alt_max),
          step = 100
        ),
        
        checkboxInput(
          "f_alt_na",
          "Inclure altitude inconnue",
          TRUE
        ),
        
        sliderInput(
          "f_an",
          "Année de récolte",
          an_range[1],
          an_range[2],
          an_range,
          step = 1,
          sep = ""
        ),
        
        checkboxInput(
          "f_an_na",
          "Inclure année inconnue",
          TRUE
        ),
        
        checkboxGroupInput(
          "f_meth",
          "Méthode de production",
          methodes,
          methodes
        ),
        
        checkboxGroupInput(
          "f_esp",
          "Espèce",
          unique(cafe$espece),
          unique(cafe$espece)
        ),
        
        textOutput("n_lots")
      ),
      
      conditionalPanel(
        "input.onglet == 'carte'",
        actionButton(
          "reset_filtres",
          "Réinitialiser les filtres",
          icon = icon("refresh"),
          class = "btn-default"
        )
      ),
      
      conditionalPanel(
        "input.onglet == 'afdm'",
        h4("Réglages de l'AFDM"),
        checkboxInput("af_alt", "Inclure l'altitude (réduit le nombre de lots)", TRUE),
        sliderInput("af_nmin", "Construction des axes : lots minimum par pays (sinon « Autres pays »)", 5, 40, 15),
        sliderInput("af_naff", "Nuage de lots : afficher les pays ayant au moins N lots", 1, 20, 3),
        selectInput("af_x", "Axe horizontal", setNames(1:5, paste("Dimension", 1:5)), selected = 1),
        selectInput("af_y", "Axe vertical", setNames(1:5, paste("Dimension", 1:5)), selected = 2),
        helpText("Le score total est une variable supplémentaire : projetée sur les axes, sans les construire.")
      ),
      conditionalPanel(
        "input.onglet == 'choix'",
        
        h4("Mes goûts"),
        
        lapply(
          KEY,
          function(a)
            sliderInput(
              paste0("c_", a),
              ATTR_LAB[[a]],
              1,
              5,
              3,
              step = 1,
              ticks = FALSE
            )
        ),
        
        helpText("1 = pas du tout, 5 = beaucoup"),
        
        selectInput(
          "c_meth",
          "Méthode de production (facultatif)",
          c(
            "Peu importe" = "",
            setdiff(methodes, "Non renseigné")
          )
        ),
        
        checkboxInput(
          "c_qual",
          "Privilégier les cafés les mieux notés",
          FALSE
        )
      )
    ),
    