# =============================================================================
# Choisir son café : carte, profil sensoriel, AFDM, conseiller IA
# Données : Coffee Quality Data (CQI, mai 2023) - Kaggle fatihb/coffee-quality-data-cqi
# Fond de carte : Natural Earth 110m (shapefile fourni)
#
# Arborescence attendue :
#   app.R
#   data/            <- les CSV café
#   www/             <- logo PNG
#
# Packages : shiny, leaflet, sf, plotly, dplyr
# Conseiller IA : NaileR + ollamar + Ollama local
# =============================================================================

library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)

# ---- 1. LECTURE ET NETTOYAGE DES DONNÉES ------------------------------------

fichiers_cafe <- list.files("data", pattern = "\\.csv$", full.names = TRUE,
                            ignore.case = TRUE)
if (!length(fichiers_cafe)) {
  stop("Aucun CSV trouvé dans data/ : déposez-y les fichiers Kaggle CQI.")
}

lire <- function(f) {
  d <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
                encoding = "UTF-8")
  names(d) <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(names(d))))
  d$espece <- if (grepl("robusta", f, ignore.case = TRUE)) "Robusta" else "Arabica"
  d
}

brut <- bind_rows(lapply(fichiers_cafe, lire))
message("Colonnes détectées : ", paste(names(brut), collapse = ", "))

trouve <- function(candidats) {
  x <- intersect(candidats, names(brut))
  if (length(x)) brut[[x[1]]] else rep(NA, nrow(brut))
}

num <- function(x)
  suppressWarnings(as.numeric(sub(",", ".", as.character(x))))

parse_altitude <- function(x) {
  vapply(tolower(as.character(x)), function(s) {
    if (is.na(s)) return(NA_real_)
    n <- as.numeric(unlist(regmatches(
      s,
      gregexpr("[0-9]+\\.?[0-9]*", gsub(",", "", s))
    )))
    if (!length(n)) return(NA_real_)
    v <- mean(n[seq_len(min(2, length(n)))])
    if (grepl("ft|feet|pies", s)) v <- v * 0.3048
    if (v > 5000) NA_real_ else v
  }, numeric(1), USE.NAMES = FALSE)
}

parse_annee <- function(x) {
  x <- as.character(x)
  m <- regexpr("(19|20)[0-9]{2}", x)
  out <- rep(NA_integer_, length(x))
  out[m > 0] <- as.integer(regmatches(x, m))
  out
}

simplifie_methode <- function(x) {
  x <- tolower(trimws(as.character(x)))
  case_when(
    is.na(x) | x == "" ~ "Non renseigné",
    grepl("semi", x) ~ "Semi-lavé",
    grepl("honey|pulp", x) ~ "Honey / pulped natural",
    grepl("wash|wet", x) ~ "Lavé",
    grepl("natur|dry", x) ~ "Nature (sec)",
    TRUE ~ "Autre"
  )
}

ATTR_CAND <- list(
  aroma = "aroma",
  flavor = "flavor",
  aftertaste = "aftertaste",
  acidity = c("acidity", "salt_acid"),
  body = c("body", "mouthfeel"),
  balance = "balance",
  sweetness = "sweetness"
)

ATTR_LAB <- c(
  aroma = "Arôme",
  flavor = "Saveur",
  aftertaste = "Longueur en bouche",
  acidity = "Acidité",
  body = "Corps",
  balance = "Équilibre",
  sweetness = "Douceur (sucré)"
)

cafe <- data.frame(
  pays_brut = trimws(as.character(trouve(c("country_of_origin", "country")))),
  altitude = parse_altitude(trouve(c("altitude_mean_meters", "altitude"))),
  annee = parse_annee(trouve(c("harvest_year", "harvest"))),
  methode = simplifie_methode(trouve(c("processing_method", "process"))),
  variete = as.character(trouve("variety")),
  espece = brut$espece,
  score = num(trouve(c("total_cup_points", "total_score"))),
  stringsAsFactors = FALSE
)

for (a in names(ATTR_CAND)) {
  v <- num(trouve(ATTR_CAND[[a]]))
  v[!is.na(v) & v <= 0] <- NA
  cafe[[a]] <- v
}

cafe$score[!is.na(cafe$score) & cafe$score < 40] <- NA
cafe <- cafe[!is.na(cafe$pays_brut) & cafe$pays_brut != "", ]

attr_cols <- names(ATTR_CAND)[vapply(
  names(ATTR_CAND),
  function(a) any(!is.na(cafe[[a]])),
  logical(1)
)]

message("Attributs sensoriels retenus : ", paste(attr_cols, collapse = ", "))
# ---- 2. FOND DE CARTE + JOINTURE DES NOMS DE PAYS ----------------------------

shp <- list.files(
  c(".", "data"),
  pattern = "^NaturalEarth_Map\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)[1]

if (is.na(shp)) {
  stop(
    "Shapefile introuvable (dossier de travail : ", getwd(),
    "). Placez NaturalEarth_Map.shp, .shx, .dbf, .prj et .cpg à côté de app.R ou dans data/."
  )
}

monde <- st_transform(st_read(shp, quiet = TRUE), 4326)

cle <- function(x)
  gsub("[^a-z]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))

recode_pays <- c(
  "United States (Hawaii)" = "United States",
  "Tanzania, United Republic Of" = "Tanzania",
  "Cote d?Ivoire" = "Cote d'Ivoire"
)

nm <- st_drop_geometry(monde)

colonnes_noms <- intersect(
  c("NAME", "NAME_LONG", "ADMIN", "NAME_EN", "FORMAL_EN"),
  names(nm)
)

dico <- unique(do.call(
  rbind,
  lapply(
    colonnes_noms,
    function(cn)
      data.frame(
        cle = cle(nm[[cn]]),
        iso = nm$ISO_A3_EH,
        stringsAsFactors = FALSE
      )
  )
))

dico <- dico[!duplicated(dico$cle), ]

cafe$pays <- ifelse(
  cafe$pays_brut %in% names(recode_pays),
  recode_pays[cafe$pays_brut],
  cafe$pays_brut
)

cafe$iso <- dico$iso[match(cle(cafe$pays), dico$cle)]

non_apparies <- unique(cafe$pays_brut[is.na(cafe$iso)])

if (length(non_apparies)) {
  message(
    "Pays SANS correspondance sur la carte (à ajouter dans recode_pays) : ",
    paste(non_apparies, collapse = " | ")
  )
}
