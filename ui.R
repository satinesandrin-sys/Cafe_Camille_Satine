# =============================================================================
# ui.R : STRUCTURE DE L'INTERFACE
#
# Lien avec server.R :
#   - un contrôle porte un identifiant (ex. "f_pays") que server.R lit avec input$f_pays ;
#   - une zone d'affichage porte un identifiant (ex. "carte") que server.R remplit avec output$carte.
#   - convention de noms : f_ = filtres, s_ = profil sensoriel, af_ = AFDM, c_ = choix de café.
#
# Organisation de la page :
#   bandeau (logo + équipe) / titre / [ panneau latéral | zone principale à 5 onglets ]
#   Onglets : Accueil, Carte, Profil sensoriel, Analyse factorielle (AFDM), Choix de café (conseiller IA).
#   Le panneau latéral change selon l'onglet actif (conditionalPanel).
# =============================================================================
fluidPage(
  # En-tête HTML de la page (non visible) : polices et feuille de style.
  tags$head(
    # Polices Playfair Display (titres) et Source Sans 3 (texte), chargées depuis Google Fonts.
    # Nécessite Internet ; sans connexion, le navigateur utilise les polices de secours prévues dans le CSS.
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@500;600&family=Source+Sans+3:wght@400;600&display=swap"
    ),
    tags$style(HTML(CSS_CAFE)),
    tags$script(src = "cafe_graphiques.js")
  ),
  # Bandeau supérieur : logo à gauche, noms de l'équipe à droite (mise en forme par les classes CSS
    div(
    class = "bandeau-equipe",
    
    logo,
    
    div(
      class = "noms-equipe",
      NOMS_EQUIPE
    )
  ),
  
  # Titre de la page ; windowTitle = texte de l'onglet du navigateur.
  titlePanel(
    "Choisir son café",
    windowTitle = "Choisir son café"
  ),
  
  # ============ PANNEAU LATÉRAL ============
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      # Onglet Accueil : encadré « En bref » (liste CHIFFRES calculée dans global.R).
      conditionalPanel(
        "input.onglet == 'accueil'",
        h4("En bref"),
        CHIFFRES
      ),
      
      # Onglets Carte et Profil sensoriel : filtres communs. Les valeurs sont utilisées par filtre() dans server.R.
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
          "f_an_na", #Identifiant du contrôle
          "Inclure année inconnue", #Libellé affiché
          TRUE #Valeur par défaut (coché)
        ),
        
        # f_meth : cases à cocher (arguments : identifiant, libellé, choix possibles, choix cochés au départ).
        checkboxGroupInput(
          "f_meth", #Identifiant du contrôle
          "Méthode de production", #Libellé affiché
          methodes, #Choix possibles (liste des méthodes de production)
          methodes #Choix cochés au départ (toutes les méthodes)
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
      
      # Onglet AFDM : réglages de l'analyse (lus par la fonction afdm() de server.R).
      conditionalPanel(
        "input.onglet == 'afdm'",
        h4("Réglages de l'AFDM"),
        checkboxInput("af_alt", "Inclure l'altitude (réduit le nombre de lots)", TRUE),
        sliderInput("af_nmin", "Construction des axes : lots minimum par pays (sinon « Autres pays »)", 5, 40, 15),
        # af_col : variable qualitative utilisée pour colorer les lots (et dessiner les ellipses)
        selectInput("af_col", "Colorer les lots selon", c("Pays", "Méthode", "Espèce")),
        # af_naff : concerne seulement le pays (les pays très rares passent en gris « Autres pays »)
        conditionalPanel(
          "input.af_col == 'Pays'",
          sliderInput("af_naff", "Pays colorés : au moins N lots", 1, 20, 3)
        ),
        # af_ell : ellipses pour les groupes d'au moins 10 lots ; af_ell_type : ce que l'ellipse représente
        checkboxInput("af_ell", "Afficher des ellipses (groupes d'au moins 10 lots)", FALSE),
        conditionalPanel(
          "input.af_ell",
          radioButtons("af_ell_type", NULL,
                       c("Confiance de la moyenne (95 %)" = "bary",
                         "Dispersion des lots (95 %)" = "disp"))
        ),        
        selectInput("af_x", "Axe horizontal", setNames(1:5, paste("Dimension", 1:5)), selected = 1),
        selectInput("af_y", "Axe vertical", setNames(1:5, paste("Dimension", 1:5)), selected = 2),
        checkboxInput("af_ell", "Ellipses de confiance (95 %) des pays d'au moins 10 lots", FALSE),
        helpText("Le score total est une variable supplémentaire : projetée sur les axes, sans les construire.")
      ),
      
      # Onglet Choix de café : « Mes goûts ».
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
    # ============ ZONE PRINCIPALE ============
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "onglet",
        
        # ---- ONGLET ACCUEIL : texte statique + quelques chiffres calculés sur les données ----
        tabPanel("Accueil", value = "accueil",
                 br(),
                 div(class = "page-accueil",
                     # Présentation du projet.
                     h3("Le projet"),
                     p("Au supermarché, le choix d'un café repose sur peu d'indications : un pays, parfois",
                       "la mention « arabica ». Cette application traduit les notes de dégustateurs",
                       "professionnels en repères simples (sucré, acide, aromatique, corsé) pour vous aider",
                       "à choisir : explorez la carte, comparez les profils de goût, puis décrivez vos envies."),
                     
                     # Présentation du jeu de données : sprintf() insère les chiffres réels (nombre de lots, de pays, période).
                     h3("Le jeu de données"),
                     p(sprintf("%s lots de café évalués par des dégustateurs certifiés du Coffee Quality Institute",
                               format(nrow(cafe), big.mark = " ")),
                       sprintf("(version de mai 2023, Kaggle), issus de %d pays et récoltés entre %s.",
                               length(pays_liste), txt_periode),
                       "Chaque ligne du jeu est un lot de café, décrit par sa provenance, son mode de production",
                       "et les notes que lui ont données les dégustateurs."),
                     
                     # Tableau des variables, rempli par output$acc_variables (server.R).
                     h3("Les variables utilisées"),
                     tableOutput("acc_variables"),
                     
                     # Limites principales ; pct() (global.R) calcule la part de données manquantes (altitude, méthode).
                     h3("À garder en tête"),
                     tags$ul(
                       tags$li("Ce sont surtout des cafés de spécialité : les scores sont des repères relatifs",
                               "entre pays, pas la note d'un paquet de supermarché."),
                       tags$li(sprintf("Le pays est une échelle large, et les données sont incomplètes (altitude inconnue pour %s des lots, méthode pour %s).",
                                       pct(is.na(cafe$altitude)), pct(cafe$methode == "Non renseigné"))),
                       tags$li("Le texte du conseiller IA est une interprétation des résultats, pas un résultat.")))),
        # ---- ONGLET CARTE : carte interactive, tableau des données, détail du pays cliqué ----
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
          
          # Carte Leaflet : créée par output$carte, pays dessinés par l'observe de server.R.
          leafletOutput("carte", height = 480),
          # Tableau des données réellement utilisées par la carte, dans un cadre défilant (hauteur maximale 320 px).
          h4("Données utilisées par la carte"),
          helpText("Le tableau suit les filtres de gauche et est trié selon l'indicateur choisi."),
          div(style = "max-height: 320px; overflow-y: auto;", tableOutput("table_carte")),
          br(),
          uiOutput("detail_pays")), #Détails sur les meilleurs lots du pays selectionné
        # ---- ONGLET PROFIL SENSORIEL : comparer jusqu'à 4 pays par radar et par phrases ----
        tabPanel(
          "Profil sensoriel",
          value = "sens",
          br(),
          
          # s_pays : pays à comparer (de 1 à 4) ; la phrase du libellé guide l'utilisateur.
          selectizeInput("s_pays",
                         "Choisissez de 1 à 4 pays pour comparer leur profil de goût :",
                         pays_liste, multiple = TRUE,
                         options = list(maxItems = 4,
                                        placeholder = "Cliquez ici, puis tapez un nom de pays")),
          
          # s_tous : afficher les 7 critères au lieu des 4 axes principaux (sucré, acide, arôme, corps).
          checkboxInput(
            "s_tous",
            "Afficher tous les critères (sinon : acide, arôme, corps)",
            FALSE
          ),
          
          # Ligne : radar combiné à gauche, phrases « En quelques mots » à droite.
          fluidRow(
            column(7, plotlyOutput("radar", height = 420)),
            column(5, h4("En quelques mots"), uiOutput("mots"))
          ),
          # Un radar par pays (deux par ligne), créés dynamiquement par server.R selon les pays choisis.
          h4("Un profil par pays"),
          uiOutput("radars_pays"),
          
          helpText(
            "Le profil est calculé sur les lots filtrés à gauche.",
            "Les notes vont de 0 à 10 ; l'axe du radar est zoomé",
            "pour faire ressortir les différences."
          )
        ),
        # ---- ONGLET AFDM : analyse factorielle de données mixtes ----
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
            
            # Cercle des corrélations : variables numériques et score total (variable supplémentaire).
            h4("Cercle des corrélations"),
            div(style = "max-width: 760px; margin: 0 auto;",
                plotlyOutput("af_cercle", height = 640)),
            helpText("Plus une flèche est longue et proche du cercle, mieux la variable est représentée sur ce plan.",
                     "Deux flèches voisines vont de pair. En rouge : le score total, variable supplémentaire."),
            
            # Nuage des lots dans le plan des deux axes choisis, une couleur par pays.
            h4("Les lots dans le plan factoriel"),
            plotlyOutput("af_ind", height = 560),
            
            # Contribution des variables aux deux axes, côte à côte.
            h4("Contribution des variables aux deux axes"),
            fluidRow(
              column(6, plotlyOutput("af_contrib1", height = 360)),
              column(6, plotlyOutput("af_contrib2", height = 360))
            ),
            helpText("Barres foncées : variables qui contribuent plus que la moyenne (trait pointillé)."),
            
            # Phrase de lecture rapide : variables qui portent chaque axe, calculée par server.R.
            uiOutput("af_texte"),
            helpText("Le premier curseur règle le regroupement utilisé pour construire les axes ; le second règle les",
                     "pays colorés sur le nuage, les lots des autres pays restant affichés en gris. Les lots sans",
                     "méthode sont exclus. L'AFDM décrit des ressemblances : elle ne prouve aucune cause.")
          )
        ),
        # ---- ONGLET CHOIX DE CAFÉ : décrire ses envies -> classement des pays -> avis du conseiller IA ----
        tabPanel(
          "Choix de café (conseiller IA)",
          value = "choix",
          br(),
          
          # Bloc 1 : l'utilisateur décrit ses envies ; l'IA règle les curseurs du panneau latéral.
          wellPanel(
            h4("Dites-moi ce que vous aimez"),
            
            # c_texte : liste déroulante façon moteur de recherche (suggestions PHRASES_CAFE de global.R).
            selectizeInput("c_texte", NULL, choices = PHRASES_CAFE, width = "100%",
                           options = list(
                             create = TRUE, createOnBlur = TRUE, persist = FALSE, maxOptions = 8,
                             placeholder = "Tapez vos envies ou choisissez une suggestion...",
                             onInitialize = I('function() { this.setValue(""); }'),
                             render = I('{ option_create: function(data, escape) { return "<div>Utiliser : <strong>" + escape(data.input) + "</strong></div>"; } }'))),
            
            # Ligne : choix du modèle de langage et bouton d'interprétation.
            fluidRow(
              column(5, selectizeInput("c_model", "Modèle Ollama", modeles_ollama,
                  selected = modeles_ollama[1],
                  options = list(create = TRUE))),
              
              column(7, br(), actionButton("c_interp", "Interpréter mes envies", class = "btn-primary"))
            ),
            
            # Résultat de l'interprétation : message de réussite ou d'erreur (output$c_interp_res).
            uiOutput("c_interp_res"),
            
            # Rappel : on peut ajuster les curseurs à la main après l'interprétation.
            helpText(
              "L'IA règle les curseurs de gauche à partir de votre texte ;",
              "vous pouvez ensuite les ajuster à la main."
            )
          ),
          
          # Bloc 2 : classement des pays selon les goûts (curseurs, texte ou les deux).
          h4("Les pays qui correspondent à vos goûts"),
          
          # Tableau du classement (output$c_classement) : pays, compatibilité, score moyen.
          tableOutput("c_classement"),
          
          # Lecture de la compatibilité (mesure relative).
          helpText(
            "Compatibilité relative : 100 = pays le plus proche de vos goûts, 0 = le plus éloigné."
          ),
          
          hr(),
          
          # Bloc 3 : avis rédigé par le conseiller IA (NaileR) sur un pays du classement.
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
          
          # Profil statistique du pays choisi, calculé sans IA (output$c_profil).
          uiOutput("c_profil"),
          # Réponse du conseiller IA, ou message d'erreur (output$c_reponse).
          uiOutput("c_reponse"),
          
          # Statut des outils : NaileR installé ? Ollama détecté ? (rafraîchi toutes les 15 secondes par server.R).
          helpText(
            textOutput("c_statut", inline = TRUE)
          ),
          
          # Rappel de prudence sur la nature du texte généré et sur la portée des données.
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