library(shiny)
library(colourpicker)

cafe <- read.csv("df_arabica_clean.csv", header = TRUE, fileEncoding = "UTF-8")

#Nettoyage des données car certaines valeurs sont manquantes ou incorrectes
choix_variete <- sort(unique(cafe$Variety[cafe$Variety != ""]))
choix_methode <- sort(unique(cafe$Processing.Method[cafe$Processing.Method != ""]))

#On mets les valeurs d'Altitude en valeur numérique pour pouvoir les utiliser dans les graphiques
extraire_altitude <- function(x) {
  x <- gsub(" ", "", x)
  bornes <- strsplit(x, "-")
  sapply(bornes, function(b) {
    b <- suppressWarnings(as.numeric(b))
    if (all(is.na(b))) NA else mean(b, na.rm = TRUE)
  })
}
cafe$Altitude_num <- extraire_altitude(cafe$Altitude)

vars_num <- names(cafe)[sapply(cafe, is.numeric)]
