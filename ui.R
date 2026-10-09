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
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "onglet",
        
        tabPanel("Accueil", value = "accueil",
                 br(),
                 div(class = "page-accueil",
                     h3("Le projet"),
                     p("Au supermarché, le choix d'un café repose sur peu d'indications : un pays, parfois",
                       "la mention « arabica ». Cette application traduit les notes de dégustateurs",
                       "professionnels en repères simples (sucré, acide, aromatique, corsé) pour vous aider",
                       "à choisir : explorez la carte, comparez les profils de goût, puis décrivez vos envies."),
                     
                     h3("Le jeu de données"),
                     p(sprintf("%s lots de café évalués par des dégustateurs certifiés du Coffee Quality Institute",
                               format(nrow(cafe), big.mark = " ")),
                       sprintf("(version de mai 2023, Kaggle), issus de %d pays et récoltés entre %s.",
                               length(pays_liste), txt_periode),
                       "Chaque ligne du jeu est un lot de café, décrit par sa provenance, son mode de production",
                       "et les notes que lui ont données les dégustateurs."),
                     
                     h3("Les variables utilisées"),
                     tableOutput("acc_variables"),
                     
                     h3("À garder en tête"),
                     tags$ul(
                       tags$li("Ce sont surtout des cafés de spécialité : les scores sont des repères relatifs",
                               "entre pays, pas la note d'un paquet de supermarché."),
                       tags$li(sprintf("Le pays est une échelle large, et les données sont incomplètes (altitude inconnue pour %s des lots, méthode pour %s).",
                                       pct(is.na(cafe$altitude)), pct(cafe$methode == "Non renseigné"))),
                       tags$li("Le texte du conseiller IA est une interprétation des résultats, pas un résultat.")))),
        tabPanel(
          "Carte",
          value = "carte",
          br(),
          
          fluidRow(
            column(
              8,
              selectInput(
                "indicateur",
                "Colorer les pays selon",
                IND
              )
            ),
            
            column(
              4,
              checkboxInput(
                "fond_osm",
                "Afficher le fond OpenStreetMap",
                FALSE
              )
            )
          ),
          
          leafletOutput("carte", height = 480),
          h4("Données utilisées par la carte"),
          helpText("Le tableau suit les filtres de gauche et est trié selon l'indicateur choisi."),
          div(style = "max-height: 320px; overflow-y: auto;", tableOutput("table_carte")),
          br(),
          uiOutput("detail_pays")),
        tabPanel(
          "Profil sensoriel",
          value = "sens",
          br(),
          
          selectizeInput("s_pays",
                         "Choisissez de 1 à 4 pays pour comparer leur profil de goût :",
                         pays_liste, multiple = TRUE,
                         options = list(maxItems = 4,
                                        placeholder = "Cliquez ici, puis tapez un nom de pays")),
          
          checkboxInput(
            "s_tous",
            "Afficher tous les critères (sinon : sucré, acide, arôme, corps)",
            FALSE
          ),
          
          fluidRow(
            column(
              7,
              plotlyOutput("radar", height = 420)
            ),
            
            column(
              5,
              h4("En quelques mots"),
              uiOutput("mots")
            )
          ),
          h4("Un profil par pays"),
          uiOutput("radars_pays"),
          
          helpText(
            "Le profil est calculé sur les lots filtrés à gauche.",
            "Les notes vont de 0 à 10 ; l'axe du radar est zoomé",
            "pour faire ressortir les différences."
          )
        ),