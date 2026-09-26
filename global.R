library(shiny)
library(colourpicker)

cafe <- read.csv("df_arabica_clean.csv", header = TRUE, fileEncoding = "UTF-8")


# NETTOYAGE

#Variety : on distingue variété unique vs mélange, on harmonise la casse
# Nombre de variétés listées dans le champ (en comptant les séparateurs , - + &)
cafe$Variety_clean <- sapply(cafe$Variety, function(x) {
  x <- trimws(x)
  if (x == "" || tolower(x) %in% c("unknow", "unknown")) return(NA_character_)
  
  cas_forces_melange <- c("Typica Bourbon Caturra Catimor", "Typica Gesha")
  est_melange <- x %in% cas_forces_melange ||
    grepl(",|\\+|&|-|\\band\\b|\\bblend\\b", x, ignore.case = TRUE)
  if (est_melange) return("Mélange / Blend")
  
  mots <- strsplit(x, " ")[[1]]
  mots <- sapply(mots, function(m) {
    if (grepl("^[A-Z0-9]+$", m)) m else tools::toTitleCase(tolower(m))
  })
  paste(mots, collapse = " ")
})

#Processing Method : fusion des doublons, mélanges regroupés
cafe$Processing_clean <- sapply(cafe$Processing.Method, function(x) {
  x <- trimws(x)
  if (x == "") return(NA_character_)
  x_low <- tolower(x)
  if (grepl(",", x)) return("Mélange / Autre")
  if (x_low %in% c("semi washed", "semi-lavado")) return("Semi Washed")
  if (x_low == "washed / wet") return("Washed / Wet")
  if (x_low == "natural / dry") return("Natural / Dry")
  if (x_low == "wet hulling") return("Wet Hulling")
  if (x_low == "double anaerobic washed") return("Double Anaerobic Washed")
  if (x_low == "anaerobico 1000h") return("Anaerobic (autre)")
  if (x_low == "double carbonic maceration / natural") return("Carbonic Maceration / Natural")
  if (x_low == "pulped natural / honey") return("Pulped Natural / Honey")
  x
})

##Color : fusion des variantes d'orthographe
cafe$Color_clean <- sapply(cafe$Color, function(x) {
  x <- trimws(tolower(x))
  if (x == "") return(NA_character_)
  if (x %in% c("yellow-green", "yellow- green", "yello-green", "yellow green")) return("Yellow-green")
  if (x %in% c("bluish-green", "blue-green")) return("Blue-green")
  if (x %in% c("browish-green", "brownish")) return("Brownish")
  if (x == "green") return("Green")
  if (x == "greenish") return("Greenish")
  if (x == "pale yellow") return("Pale yellow")
  if (x == "yellowish") return("Yellowish")
  x
})

##Harvest Year : on garde la première année de chaque campagne
cafe$Harvest_year_clean <- as.integer(sub(" */.*", "", trimws(cafe$Harvest.Year)))

##Altitude numérique 
extraire_altitude <- function(x) {
  x <- trimws(x)
  x <- gsub("~", "-", x)
  x <- gsub("(?i)\\s*A\\s*", "-", x, perl = TRUE)
  x <- gsub("\\s+", "", x)
  bornes <- strsplit(x, "-")
  sapply(bornes, function(b) {
    b <- suppressWarnings(as.numeric(b))
    if (all(is.na(b))) return(NA_real_)
    val <- mean(b, na.rm = TRUE)
    if (val > 3000) return(NA_real_)
    val
  })
}
cafe$Altitude_num <- extraire_altitude(cafe$Altitude)

##Status : colonne à valeur unique, inutile
cafe$Status <- NULL

##Region : garde le 1er élément, casse harmonisée (sauf caractères chinois)
cafe$Region_clean <- sapply(cafe$Region, function(x) {
  x <- trimws(x)
  if (x == "") return(NA_character_)
  x <- trimws(strsplit(x, ",")[[1]][1])
  if (grepl("[\u4e00-\u9fff]", x)) return(x)
  tools::toTitleCase(tolower(x))
})

#Listes de choix propres pour les filtres 
choix_variete  <- sort(unique(na.omit(cafe$Variety_clean)))
choix_methode  <- sort(unique(na.omit(cafe$Processing_clean)))
choix_couleur  <- sort(unique(na.omit(cafe$Color_clean)))

choix_annee <- sort(unique(na.omit(cafe$Harvest_year_clean)))

vars_num <- names(cafe)[sapply(cafe, is.numeric)]


