#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("Data Café"),
    
    tabsetPanel(
      tabPanel("Accueil", 
               h3("Bienvenue sur l'onglet Accueil"),
               p("Ceci est le contenu du premier onglet."), 
      ),
      tabPanel("Analyses", 
               h3("Onglet Analyses"),
               plotOutput("plot")
      ),
      tabPanel("Données", 
               h3("Onglet Données"),
               tableOutput("table")

        )
    )
)





