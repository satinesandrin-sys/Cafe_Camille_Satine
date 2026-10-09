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
        tabPanel(
          "Analyse factorielle (AFDM)",
          value = "afdm",
          br(),
          div(
            class = "page-accueil",
            p("L'analyse factorielle de données mixtes (AFDM) résume en quelques axes les ressemblances",
              "entre lots décrits à la fois par des variables numériques (critères sensoriels, altitude)",
              "et qualitatives (pays, méthode, espèce)."),
            uiOutput("af_resume"),
            
            h4("Cercle des corrélations"),
            div(style = "max-width: 760px; margin: 0 auto;",
                plotlyOutput("af_cercle", height = 640)),
            helpText("Plus une flèche est longue et proche du cercle, mieux la variable est représentée sur ce plan.",
                     "Deux flèches voisines vont de pair. En rouge : le score total, variable supplémentaire."),
            
            h4("Les lots dans le plan factoriel : une couleur par pays"),
            plotlyOutput("af_ind", height = 560),
            
            h4("Contribution des variables aux deux axes"),
            fluidRow(
              column(6, plotlyOutput("af_contrib1", height = 360)),
              column(6, plotlyOutput("af_contrib2", height = 360))
            ),
            helpText("Barres foncées : variables qui contribuent plus que la moyenne (trait pointillé)."),
            
            uiOutput("af_texte"),
            helpText("Le premier curseur règle le regroupement utilisé pour construire les axes ; le second règle les",
                     "pays colorés sur le nuage, les lots des autres pays restant affichés en gris. Les lots sans",
                     "méthode sont exclus. L'AFDM décrit des ressemblances : elle ne prouve aucune cause.")
          )
        ),
        tabPanel(
          "Choix de café (conseiller IA)",
          value = "choix",
          br(),
          
          wellPanel(
            h4("Dites-moi ce que vous aimez"),
            
            selectizeInput("c_texte", NULL, choices = PHRASES_CAFE, width = "100%",
                           options = list(
                             create = TRUE, createOnBlur = TRUE, persist = FALSE, maxOptions = 8,
                             placeholder = "Tapez vos envies ou choisissez une suggestion...",
                             onInitialize = I('function() { this.setValue(""); }'),
                             render = I('{ option_create: function(data, escape) { return "<div>Utiliser : <strong>" + escape(data.input) + "</strong></div>"; } }'))),
            
            fluidRow(
              column(
                5,
                selectizeInput(
                  "c_model",
                  "Modèle Ollama",
                  modeles_ollama,
                  selected = modeles_ollama[1],
                  options = list(create = TRUE)
                )
              ),
              
              column(
                7,
                br(),
                actionButton(
                  "c_interp",
                  "Interpréter mes envies",
                  class = "btn-primary"
                )
              )
            ),
            
            uiOutput("c_interp_res"),
            
            helpText(
              "L'IA règle les curseurs de gauche à partir de votre texte ;",
              "vous pouvez ensuite les ajuster à la main."
            )
          ),
          
          h4("Les pays qui correspondent à vos goûts"),
          
          tableOutput("c_classement"),
          
          helpText(
            "Compatibilité relative : 100 = pays le plus proche de vos goûts, 0 = le plus éloigné."
          ),
          
          hr(),
          
          h4("L'avis du conseiller"),
          
          fluidRow(
            column(
              4,
              selectInput(
                "c_pays",
                "Pays à expliquer",
                NULL
              )
            ),
            
            column(
              4,
              br(),
              actionButton(
                "c_go",
                "Demander au conseiller"
              )
            ),
            
            column(
              4,
              br(),
              checkboxInput(
                "c_apercu",
                "Voir seulement le prompt (sans LLM)",
                FALSE
              )
            )
          ),
          
          uiOutput("c_profil"),
          uiOutput("c_reponse"),
          
          helpText(
            textOutput("c_statut", inline = TRUE)
          ),
          
          helpText(
            "Le LLM ne reçoit que des résultats statistiques calculés dans R",
            "(NaileR) ; son texte est une interprétation, pas un résultat.",
            "Les lots CQI sont surtout des cafés de spécialité : les conseils",
            "valent comme repères relatifs entre pays."
          )
        )
      )
    )
  )
)