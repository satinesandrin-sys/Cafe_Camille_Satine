fluidPage(
  titlePanel("Data Café"),
  
  tabsetPanel(
    tabPanel("Accueil",
             h3("Bienvenue"),
             p("Ceci est le contenu du premier onglet.")
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
    ),
    
    tabPanel("Explorer les cafés",
             sidebarLayout(
               sidebarPanel(
                 selectInput("pays", "Pays :",
                             choices = c("Tous", sort(unique(cafe$Country.of.Origin)))),
                 selectInput("variete", "Variété :",
                             choices = c("Tous", sort(unique(cafe$Variety)))),
                 selectInput("methode", "Méthode de traitement :",
                             choices = c("Tous", sort(unique(cafe$Processing.Method)))),
                 selectInput("altitude_num", "Altitude :",
                             choices = c("Tous", sort(unique(cafe$Altitude))))
               ),
               mainPanel(
                 div(style = "overflow-x: auto; max-height: 500px; overflow-y: auto;",
                     tableOutput("table"))
               )
             )
    )
  )
)