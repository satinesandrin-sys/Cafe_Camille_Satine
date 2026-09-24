#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#
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
    
    tabPanel("Données",
             selectInput("pays", "Pays :",
                         choices = c("Tous", sort(unique(cafe$Country.of.Origin)))),
             div(style = "overflow-x: auto; max-height: 500px; overflow-y: auto;",
                 tableOutput("table"))
    )
  )
)