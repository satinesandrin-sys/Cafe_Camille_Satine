################################################################################
# global.R : packages, lecture et nettoyage des données
# (exécuté une seule fois au lancement de l'app, avant ui.R et server.R)
################################################################################

# ---- 1. Packages ------------------------------------------------------------
library(shiny)
library(colourpicker)
library(leaflet)
library(dplyr)
library(tidyr)
library(tibble)
library(stringr)
library(plotly)
library(DT)
library(scales)
library(ggplot2)

# ---- 2. Lecture -------------------------------------------------------------
cafe <- read.csv("df_arabica_clean.csv", header = TRUE, fileEncoding = "UTF-8")

# On vérifie tout de suite que les colonnes utilisées plus bas existent
# (read.csv remplace les espaces par des points : "Country of Origin" devient
# "Country.of.Origin"). Sinon, message clair plutôt qu'une erreur obscure.
colonnes_attendues <- c("Country.of.Origin", "Total.Cup.Points", "Variety",
                        "Processing.Method", "Color", "Harvest.Year", "Altitude",
                        "Region", "Aroma", "Flavor", "Acidity")
colonnes_manquantes <- setdiff(colonnes_attendues, names(cafe))
if (length(colonnes_manquantes) > 0) {
  stop("Colonnes manquantes dans le CSV : ",
       paste(colonnes_manquantes, collapse = ", "),
       "\nColonnes disponibles : ", paste(names(cafe), collapse = ", "))
}

# ---- 3. Nettoyage -----------------------------------------------------------

## Outil : retire les espaces autour du texte et transforme "" en NA.
## Ça évite de tester à chaque fois x == "" (qui plante sur les NA).
vide_en_na <- function(x) {
  x <- trimws(as.character(x))
  x[!is.na(x) & x == ""] <- NA_character_
  x
}

## Pays : espaces parasites retirés (sinon la jointure avec les coordonnées
## de la carte rate pour ces pays)
cafe$Country.of.Origin <- vide_en_na(cafe$Country.of.Origin)

## Libellé lisible du pays pour l'affichage (utilisé par la carte)
clean_country_name <- function(x) {
  ifelse(x == "Tanzania, United Republic Of", "Tanzania", x)
}

## Variété : variété unique vs mélange, casse harmonisée
## (mélange = présence d'un séparateur , + & - ou des mots "and" / "blend")
nettoyer_variete <- function(x) {
  if (is.na(x) || tolower(x) %in% c("unknow", "unknown")) return(NA_character_)
  
  cas_forces_melange <- c("Typica Bourbon Caturra Catimor", "Typica Gesha")
  est_melange <- x %in% cas_forces_melange ||
    grepl(",|\\+|&|-|\\band\\b|\\bblend\\b", x, ignore.case = TRUE)
  if (est_melange) return("Mélange / Blend")
  
  mots <- strsplit(x, " ")[[1]]
  mots <- vapply(mots, function(m) {
    if (grepl("^[A-Z0-9]+$", m)) m else tools::toTitleCase(tolower(m))
  }, character(1), USE.NAMES = FALSE)
  paste(mots, collapse = " ")
}
# vapply(..., USE.NAMES = FALSE) et non sapply() : sapply() renvoie un vecteur
# NOMMÉ, ce qui déclenche l'avertissement jsonlite "named vector" avec leaflet.
cafe$Variety_clean <- vapply(vide_en_na(cafe$Variety), nettoyer_variete,
                             character(1), USE.NAMES = FALSE)

## Méthode de traitement : doublons fusionnés, mélanges regroupés
methode <- vide_en_na(cafe$Processing.Method)
methode_low <- tolower(methode)
cafe$Processing_clean <- case_when(
  is.na(methode)                                    ~ NA_character_,
  grepl(",", methode)                               ~ "Mélange / Autre",
  methode_low %in% c("semi washed", "semi-lavado")  ~ "Semi Washed",
  methode_low == "washed / wet"                     ~ "Washed / Wet",
  methode_low == "natural / dry"                    ~ "Natural / Dry",
  methode_low == "wet hulling"                      ~ "Wet Hulling",
  methode_low == "double anaerobic washed"          ~ "Double Anaerobic Washed",
  methode_low == "anaerobico 1000h"                 ~ "Anaerobic (autre)",
  methode_low == "double carbonic maceration / natural" ~ "Carbonic Maceration / Natural",
  methode_low == "pulped natural / honey"           ~ "Pulped Natural / Honey",
  TRUE                                              ~ methode
)

## Couleur : variantes d'orthographe fusionnées
couleur <- tolower(vide_en_na(cafe$Color))
cafe$Color_clean <- case_when(
  is.na(couleur)                                                          ~ NA_character_,
  couleur %in% c("yellow-green", "yellow- green", "yello-green", "yellow green") ~ "Yellow-green",
  couleur %in% c("bluish-green", "blue-green")                            ~ "Blue-green",
  couleur %in% c("browish-green", "brownish")                             ~ "Brownish",
  couleur == "green"                                                      ~ "Green",
  couleur == "greenish"                                                   ~ "Greenish",
  couleur == "pale yellow"                                                ~ "Pale yellow",
  couleur == "yellowish"                                                  ~ "Yellowish",
  TRUE                                                                    ~ couleur
)

## Année de récolte : on garde la première année de la campagne
## (extraction du 1er nombre à 4 chiffres : gère "2017 / 2018", "2017-2018",
## "Spring 2016"... qui donnaient NA avec as.integer() direct)
cafe$Harvest_year_clean <- as.integer(
  str_extract(as.character(cafe$Harvest.Year), "[0-9]{4}")
)

## Altitude numérique
## On retire les virgules de milliers ("1,800"), puis on extrait tous les
## nombres du texte et on prend la moyenne : gère "1700-1930", "1700 a 1930",
## "1200m", "~1500". Les valeurs <= 0 ou > 3000 m sont des erreurs de saisie -> NA.
extraire_altitude <- function(x) {
  x <- gsub(",", "", as.character(x))
  nombres <- str_extract_all(x, "[0-9]+\\.?[0-9]*")
  vapply(nombres, function(b) {
    b <- as.numeric(b)
    b <- b[!is.na(b)]
    if (length(b) == 0) return(NA_real_)
    val <- mean(b)
    if (val <= 0 || val > 3000) return(NA_real_)
    val
  }, numeric(1), USE.NAMES = FALSE)
}
cafe$Altitude_num <- extraire_altitude(cafe$Altitude)

## Région : on garde le 1er élément, casse harmonisée (sauf caractères chinois)
nettoyer_region <- function(x) {
  if (is.na(x)) return(NA_character_)
  x <- trimws(strsplit(x, ",")[[1]][1])
  if (is.na(x) || x == "") return(NA_character_)
  if (grepl("[\u4e00-\u9fff]", x)) return(x)
  tools::toTitleCase(tolower(x))
}
cafe$Region_clean <- vapply(vide_en_na(cafe$Region), nettoyer_region,
                            character(1), USE.NAMES = FALSE)

## Status : colonne à valeur unique, inutile
cafe$Status <- NULL

# ---- 4. Coordonnées pour la carte ------------------------------------------
# Les noms de pays doivent être IDENTIQUES à ceux de Country.of.Origin.
country_centroids <- tribble(
  ~Country,                        ~lat,     ~lng,
  "Colombia",                       4.5709,  -74.2973,
  "Taiwan",                        23.6978,  120.9605,
  "Laos",                          19.8563,  102.4955,
  "Costa Rica",                     9.7489,  -83.7534,
  "Guatemala",                     15.7835,  -90.2308,
  "Tanzania, United Republic Of",  -6.3690,   34.8888,
  "Ethiopia",                       9.1450,   40.4897,
  "Thailand",                      15.8700,  100.9925,
  "Brazil",                       -14.2350,  -51.9253,
  "United States (Hawaii)",        20.7984, -156.3319,
  "Kenya",                         -0.0236,   37.9062,
  "Uganda",                         1.3733,   32.2903,
  "Indonesia",                     -0.7893,  113.9213,
  "Peru",                          -9.1900,  -75.0152,
  "Panama",                         8.5380,  -80.7821,
  "Nicaragua",                     12.8654,  -85.2072,
  "Vietnam",                       14.0583,  108.2772,
  "Honduras",                      15.2000,  -86.2419,
  "El Salvador",                   13.7942,  -88.8965,
  "Madagascar",                   -18.7669,   46.8691,
  "Mexico",                        23.6345, -102.5528,
  "Myanmar",                       21.9162,   95.9560
)

# Un pays du CSV absent de ce tableau ne peut pas s'afficher sur la carte.
# On le signale dans la console R au lancement : il suffit de l'ajouter ci-dessus.
pays_sans_coord <- setdiff(unique(na.omit(cafe$Country.of.Origin)),
                           country_centroids$Country)
if (length(pays_sans_coord) > 0) {
  warning("Pays sans coordonnées (absents de la carte) : ",
          paste(pays_sans_coord, collapse = ", "))
}

# ---- 5. Éléments pour l'interface ------------------------------------------

## Listes de choix propres pour les filtres
choix_variete <- sort(unique(na.omit(cafe$Variety_clean)))
choix_methode <- sort(unique(na.omit(cafe$Processing_clean)))
choix_couleur <- sort(unique(na.omit(cafe$Color_clean)))
choix_annee   <- sort(unique(na.omit(cafe$Harvest_year_clean)))

vars_num <- names(cafe)[sapply(cafe, is.numeric)]

## Paramètres de la carte
# Bornes de l'altitude : sert à savoir si le slider est "tout sélectionné"
alt_bornes <- range(cafe$Altitude_num, na.rm = TRUE)


# Domaine fixe de la palette : étendue des scores moyens par pays sur tout le
# jeu de données. Les couleurs restent stables quand les filtres changent, et
# la palette ne casse pas quand il ne reste qu'un seul pays.
domaine_score <- range(tapply(cafe$Total.Cup.Points, cafe$Country.of.Origin,
                              mean, na.rm = TRUE))
## Bornes du curseur Total Cup Points
cup_min <- floor(min(cafe$Total.Cup.Points, na.rm = TRUE))
cup_max <- ceiling(max(cafe$Total.Cup.Points, na.rm = TRUE))
