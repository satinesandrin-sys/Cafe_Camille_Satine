fluidPage(
  titlePanel("Data Café"),
  
  tabsetPanel(
    tabPanel("Accueil",
             h3("Bienvenue"),
             p("Ceci est le contenu du premier onglet.")
    ),
    
    # Ordre des onglets : exploration puis analyse
    tabPanel("Explorer les cafés",
             sidebarLayout(
               sidebarPanel(
                 selectInput("pays", "Pays :",
                             choices = c("Tous", sort(unique(cafe$Country.of.Origin)))),
                 selectInput("variete", "Variété :",
                             choices = c("Tous", choix_variete)),
                 selectInput("methode", "Méthode de traitement :",
                             choices = c("Tous", choix_methode)),
                 sliderInput("altitude_range", "Altitude (m) :",
                             min = floor(min(cafe$Altitude_num, na.rm = TRUE)),
                             max = ceiling(max(cafe$Altitude_num, na.rm = TRUE)),
                             value = range(cafe$Altitude_num, na.rm = TRUE)),
                 selectInput("annee", "Année de récolte :",
                             choices = c("Tous", choix_annee))
               ),
               mainPanel(
                 # Carte : taille des bulles = nombre de cafés, couleur = score moyen
                 leafletOutput(outputId = "world_map", height = "600px"),
                 div(style = "overflow-x: auto; max-height: 500px; overflow-y: auto;",
                     tableOutput("table"))
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
