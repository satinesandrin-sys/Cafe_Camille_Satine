fluidPage(
  tags$head(tags$style(HTML("
    .value-box { background:#f6efe9; border-left:5px solid #8B5A2B;
                 border-radius:6px; padding:10px 14px; margin-bottom:15px; }
    .value-box .num { font-size:26px; font-weight:bold; color:#5a3a1a; }
    .value-box .lab { font-size:13px; color:#777; }
  "))),
  
  titlePanel("Data Café"),
  
  tabsetPanel(
    tabPanel("Accueil",
             h3("Bienvenue"),
             p("Ceci est le contenu du premier onglet.")
    ),
    
    tabPanel("Explorer les cafés",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 selectInput("pays", "Pays :",
                             choices = c("Tous", sort(unique(na.omit(cafe$Country.of.Origin))))),
                 selectInput("variete", "Variété :",
                             choices = c("Tous", choix_variete)),
                 selectInput("methode", "Méthode de traitement :",
                             choices = c("Tous", choix_methode)),
                 sliderInput("altitude_range", "Altitude (m) :",
                             min = floor(alt_bornes[1]),
                             max = ceiling(alt_bornes[2]),
                             value = c(floor(alt_bornes[1]), ceiling(alt_bornes[2]))),
                 selectInput("annee", "Année de récolte :",
                             choices = c("Tous", choix_annee)),
                 
                 sliderInput("f_cup", "🏆 Total Cup Points",
                             min = cup_min, max = cup_max,
                             value = c(cup_min, cup_max), step = 0.5),
                 
                 actionButton("reset_filters", "↺ Reinitialiser les filtres",
                              class = "btn-sm"),
                 br(), br(),
                 uiOutput("map_selection_info")
               ),
               
               mainPanel(
                 width = 9,
                 
                 fluidRow(
                   column(3, div(class = "value-box",
                                 div(class = "num", textOutput("kpi_n")),
                                 div(class = "lab", "Cafés sélectionnés"))),
                   column(3, div(class = "value-box",
                                 div(class = "num", textOutput("kpi_pays")),
                                 div(class = "lab", "Pays représentés"))),
                   column(3, div(class = "value-box",
                                 div(class = "num", textOutput("kpi_score")),
                                 div(class = "lab", "Score moyen"))),
                   column(3, div(class = "value-box",
                                 div(class = "num", textOutput("kpi_alt")),
                                 div(class = "lab", "Altitude moyenne (m)")))
                 ),
                 
                 # Carte : taille des bulles = nombre de cafés, couleur = score moyen
                 
                 leafletOutput("world_map", height = "500px"),
                 p("Taille des bulles = nombre de cafes ; couleur = score moyen. ",
                   "Cliquez sur un pays pour filtrer le reste de l'application."),
                 br(),
                 div(style = "overflow-x: auto; max-height: 500px; overflow-y: auto;",
                     tableOutput("table")),
                 DTOutput("coffee_table")
                 
               )
             )
    ),
    
    tabPanel("Analyses",
             sidebarLayout(
               sidebarPanel(
                 selectInput("var", "Variable :", choices = vars_num),
                 sliderInput("bins", "Nombre de classes :", min = 5, max = 50, value = 20),
                 colourInput("col", "Couleur :", value = "#8B5A2B")
               ),
               mainPanel(plotOutput("plot"))
             )
    )
  )
)