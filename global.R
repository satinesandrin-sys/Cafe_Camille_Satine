# =============================================================================
# CHOISIR SON CAFÉ  -  global.R
# Application R Shiny : carte interactive, profil sensoriel, AFDM, conseiller IA.
#
# SOURCES
#   Données  : Coffee Quality Data (CQI, mai 2023) - Kaggle fatihb/coffee-quality-data-cqi
#   Carte    : Natural Earth, échelle 1:110 millions (shapefile NaturalEarth_Map.)
#
# ARBORESCENCE ATTENDUE
#   global.R, ui.R, server.R
#   data/  <- CSV du café + shapefile du fond de carte
#   www/   <- logo (PNG) et, en option, cafe.jpg (image de la fenêtre d'accueil)
#
#
# PLAN DU FICHIER
#   1. Lecture et nettoyage du CSV         
#   2. Fond de carte et jointure des noms de pays
#   3. Paramètres de l'interface           
#   4. Conseiller IA (NaileR / Ollama)
#   5. Habillage (CSS, image, graphiques)  
#   6. Éléments de la page d'accueil
#   7. Outils de l'AFDM                    
#   8. Logo
# ==============================================================================  

# Chargement des packages  :
#   shiny   = framework de l'application (entrées, sorties, réactivité, balises HTML)
#   leaflet = carte interactive      
#   sf    = lecture et manipulation du fond de carte
#   plotly  = graphiques interactifs (radars, nuage de lots, contributions ; fournit aussi layout())
#   dplyr   = bind_rows() et case_when() pour le nettoyage
#
#   Optionnels   : FactoMineR (onglet AFDM) ; NaileR + ollamar + serveur Ollama local
#                  (conseiller IA). Leur absence désactive seulement la fonction concernée.
library(shiny)
library(leaflet)
library(sf)
library(plotly)
library(dplyr)
library(FactoMineR) 
library(NaileR)
library(ollamar)

# ---- 1. LECTURE ET NETTOYAGE DES DONNÉES ------------------------------------

# Liste des CSV du dossier data pour pouvoir étendre l'application si de nouvelles données sont trouvées.
fichiers_cafe <- list.files("data", pattern = "\\.csv$", full.names = TRUE,
                            ignore.case = TRUE)
if (!length(fichiers_cafe)) {
  stop("Aucun CSV trouvé dans data/ : déposez-y les fichiers Kaggle CQI.")
} # Sans données, inutile de continuer : arrêt avec un message qui dit quoi faire.

lire <- function(f) { #f = chemin du fichier
  d <- read.csv(f, check.names = FALSE, stringsAsFactors = FALSE,
                encoding = "UTF-8")
  # Normalisation des noms
  names(d) <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(names(d))))
  d$espece <- if (grepl("robusta", f, ignore.case = TRUE)) "Robusta" else "Arabica"
  d #data.frame aux noms de colonnes normalisés, avec une colonne `espece` en plus.
} #lit UN fichier CSV et le met au format commun.

brut <- bind_rows(lapply(fichiers_cafe, lire))
# Trace en console : permet de vérifier quelles colonnes ont réellement été lues.
message("Colonnes détectées : ", paste(names(brut), collapse = ", "))

trouve <- function(candidats) {
  x <- intersect(candidats, names(brut))
  if (length(x)) brut[[x[1]]] else rep(NA, nrow(brut))
}

num <- function(x)
  suppressWarnings(as.numeric(sub(",", ".", as.character(x))))

parse_altitude <- function(x) { # transforme une altitude saisie librement en moyenne numérique en mètres (ou NA si impossible).
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

parse_annee <- function(x) { #extrait l'année de récolte d'un texte libre
  x <- as.character(x)
  m <- regexpr("(19|20)[0-9]{2}", x)
  out <- rep(NA_integer_, length(x))
  out[m > 0] <- as.integer(regmatches(x, m))
  out
}

simplifie_methode <- function(x) { #regroupe les méthodes de production en 5 catégories + « Non renseigné »
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

ATTR_CAND <- list( #pour chaque critère sensoriel, noms de colonnes possibles dans les CSV
  aroma = "aroma",
  flavor = "flavor",
  aftertaste = "aftertaste",
  acidity = c("acidity", "salt_acid"),
  body = c("body", "mouthfeel"),
  balance = "balance",
  sweetness = "sweetness"
)

ATTR_LAB <- c( #libellé français de chaque critère, utilisé dans les graphiques et tableaux.
  aroma = "Arôme",
  flavor = "Saveur",
  aftertaste = "Longueur en bouche",
  acidity = "Acidité",
  body = "Corps",
  balance = "Équilibre",
  sweetness = "Douceur"
)

cafe <- data.frame( # data.frame final, nettoyé et prêt à l'emploi pour l'application, une ligne = un lot de café
  pays_brut = trimws(as.character(trouve(c("country_of_origin", "country")))),
  altitude = parse_altitude(trouve(c("altitude_mean_meters", "altitude"))),
  annee = parse_annee(trouve(c("harvest_year", "harvest"))),
  methode = simplifie_methode(trouve(c("processing_method", "process"))),
  variete = as.character(trouve("variety")),
  espece = brut$espece,
  score = num(trouve(c("total_cup_points", "total_score"))),
  stringsAsFactors = FALSE
)

# Ajout des 7 critères sensoriels : pour chacun, lecture de la 1re colonne disponible puis nettoyage.
for (a in names(ATTR_CAND)) {
  v <- num(trouve(ATTR_CAND[[a]]))
  v[!is.na(v) & v <= 0] <- NA
  cafe[[a]] <- v
}

# Un score total inférieur à 40 est jugé aberrant (ligne mal remplie) -> NA.
cafe$score[!is.na(cafe$score) & cafe$score < 40] <- NA
# Les lots sans pays sont retirés : impossibles à placer sur la carte.
cafe <- cafe[!is.na(cafe$pays_brut) & cafe$pays_brut != "", ]

attr_cols <- names(ATTR_CAND)[vapply(
  names(ATTR_CAND),
  function(a) any(!is.na(cafe[[a]])),
  logical(1)
)] #liste des critères d'évaluation de référence utilisée ensuite partout dans l'application.

message("Attributs sensoriels retenus : ", paste(attr_cols, collapse = ", "))

# ---- 2. FOND DE CARTE + JOINTURE DES NOMS DE PAYS ----------------------------

shp <- list.files(
  c(".", "data"),
  pattern = "^NaturalEarth_Map\\.shp$",
  recursive = TRUE,
  full.names = TRUE,
  ignore.case = TRUE
)[1] #récupération du shapefile 

if (is.na(shp)) {
  stop(
    "Shapefile introuvable (dossier de travail : ", getwd(),
    "). Placez NaturalEarth_Map.shp, .shx, .dbf, .prj et .cpg à côté de app.R ou dans data/."
  )
}

monde <- st_transform(st_read(shp, quiet = TRUE), 4326)

cle <- function(x)
  gsub("[^a-z]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))

recode_pays <- c( #corrections manuelles des noms du CSV qui ne correspondent à aucun nom de la carte.
  "United States (Hawaii)" = "United States",
  "Tanzania, United Republic Of" = "Tanzania",
  "Cote d?Ivoire" = "Cote d'Ivoire"
)

nm <- st_drop_geometry(monde)

colonnes_noms <- intersect(
  c("NAME", "NAME_LONG", "ADMIN", "NAME_EN", "FORMAL_EN"),
  names(nm)
)

dico <- unique(do.call( # dictionnaire « clé de nom -> code ISO du pays »
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

dico <- dico[!duplicated(dico$cle), ] # Une clé ne doit désigner qu'un seul pays : on garde la première occurrence.

# `pays` = nom harmonisé : corrigé par recode_pays si nécessaire, sinon nom d'origine.
cafe$pays <- ifelse(
  cafe$pays_brut %in% names(recode_pays),
  recode_pays[cafe$pays_brut],
  cafe$pays_brut
)

cafe$iso <- dico$iso[match(cle(cafe$pays), dico$cle)] # colonne qui relie un lot à son polygone sur la carte.

non_apparies <- unique(cafe$pays_brut[is.na(cafe$iso)])

if (length(non_apparies)) {
  message(
    "Pays SANS correspondance sur la carte (à ajouter dans recode_pays) : ",
    paste(non_apparies, collapse = " | ")
  )
}

# ---- 3. PARAMÈTRES D'INTERFACE ------------------------------------------------
# alt_max : borne haute du curseur d'altitude = altitude maximale arrondie aux 100 m, plafonnée à 4000 m (3000 si aucune donnée).
alt_max <- if (any(!is.na(cafe$altitude))) min(4000, ceiling(max(cafe$altitude, na.rm = TRUE) / 100) * 100) else 3000

# an_range : bornes du curseur d'année de récolte (années min et max observées).
an_range <- if (any(!is.na(cafe$annee))) range(cafe$annee, na.rm = TRUE) else c(2010, 2023)

# Listes servant aux filtres : méthodes de production et pays présents dans les données.
methodes <- sort(unique(cafe$methode))
pays_liste <- sort(unique(cafe$pays))

# IND : indicateurs proposés pour colorer la carte (étiquette affichée = colonne ou calcul associé).
IND <- c(
  "Score total moyen" = "score",
  "Nombre de lots" = "n",
  setNames(attr_cols, paste("Note moyenne :", ATTR_LAB[attr_cols]))
)

# MOTS : expression grand public associée à chaque critère quand un pays le dépasse nettement (voir phrase_gout).
MOTS <- c(
  aroma = "très aromatique",
  flavor = "riche en saveurs",
  aftertaste = "longue en bouche",
  acidity = "vive et acidulée",
  body = "ronde et corsée",
  balance = "équilibrée",
  sweetness = "douce et sucrée"
)

# KEY : les 4 axes « grand public » du profil sensoriel (douceur, acidité, arôme, corps), s'ils existent dans les données.
KEY <- intersect(
  c("sweetness", "acidity", "aroma", "body"),
  attr_cols
)

# phrase_gout(m, ref) : décrit en mots ce qui distingue un pays de la référence, pour le profil sensoriel.
phrase_gout <- function(m, ref) {
  ecart <- sort(m - ref, decreasing = TRUE)
  ecart <- ecart[is.finite(ecart) & ecart > 0]
  
  if (length(ecart))
    paste(
      "plutôt",
      paste(MOTS[names(head(ecart, 2))], collapse = ", ")
    )
  else
    "dans la moyenne sur tous les critères"
}

# ---- CONSEILLER IA ------------------------------------------------------------
#Préparation des fonctions pour l'onglet « Conseiller IA » : NaileR + Ollama (si disponibles).
NAILER_OK <- requireNamespace("NaileR", quietly = TRUE)

ollama_actif <- function() {
  isTRUE(tryCatch({
    con <- url("http://localhost:11434")
    on.exit(close(con))
    readLines(con, n = 1, warn = FALSE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE))
}

modeles_ollama <- if (requireNamespace("ollamar", quietly = TRUE) && ollama_actif()) tryCatch(ollamar::list_models()$name, error = function(e) character()) else character()

modeles_ollama <- unique(
  c(modeles_ollama, "llama3.1", "mistral-small3.2")
)

# NIVEAUX : traduction des niveaux 1 à 5 des curseurs « Mes goûts » en mots, pour le prompt envoyé au LLM.
NIVEAUX <- c(
  "pas du tout",
  "peu",
  "moyennement",
  "assez",
  "très"
)

# ADJ : adjectif décrivant chaque critère, utilisé dans les phrases adressées au LLM.
ADJ <- c(
  sweetness = "sucré",
  acidity = "acide",
  aroma = "aromatique",
  body = "corsé",
  flavor = "savoureux",
  aftertaste = "long en bouche",
  balance = "équilibré"
)

# NOMS_LLM : noms de colonnes sans accents ni espaces, envoyés à NaileR (plus sûrs pour le package et le LLM).
NOMS_LLM <- c(
  aroma = "Arome",
  flavor = "Saveur",
  aftertaste = "Longueur_en_bouche",
  acidity = "Acidite",
  body = "Corps",
  balance = "Equilibre",
  sweetness = "Douceur"
)

# donnees_llm(cible) : prépare le tableau donné à NaileR.
donnees_llm <- function(cible) {
  d <- cafe[!is.na(cafe$pays), ]
  d <- d[complete.cases(d[attr_cols]), ]
  
  df <- d[attr_cols]
  names(df) <- unname(NOMS_LLM[attr_cols])
  
  alt <- cut(
    d$altitude,
    c(-Inf, 1000, 1500, Inf),
    labels = c(
      "basse (<1000 m)",
      "moyenne (1000-1500 m)",
      "haute (>1500 m)"
    )
  )
  
  df$Altitude <- factor(
    ifelse(is.na(alt), "inconnue", as.character(alt))
  )
  
  df$Methode <- factor(d$methode)
  
  df$Groupe <- factor(
    ifelse(d$pays == cible, cible, "Autres pays"),
    levels = c(cible, "Autres pays")
  )
  
  df
}

# appelle_nailer() : statistiques + rédaction par le LLM.
appelle_nailer <- function(
    df,
    introduction,
    request,
    model,
    generate = TRUE
) {
  f <- names(formals(NaileR::nail_catdes))
  
  args <- list(
    dataset = df,
    num.var = ncol(df),
    introduction = introduction,
    request = request,
    model = model
  )
  
  if ("generate" %in% f)
    args$generate <- generate
  
  if ("provider" %in% f)
    args$provider <- "ollama"
  
  if ("isolate.groups" %in% f)
    args$isolate.groups <- FALSE
  
  if ("interpretation_mode" %in% f)
    args$interpretation_mode <- "standard"
  
  do.call(NaileR::nail_catdes, args)
}

# texte_nailer(res, type) : extrait le texte d'un résultat NaileR.
texte_nailer <- function(
    res,
    type = c("response", "prompt")
) {
  type <- match.arg(type)
  
  ns <- asNamespace("NaileR")
  fn <- paste0("nail_", type)
  
  if (exists(fn, envir = ns, inherits = FALSE)) {
    out <- tryCatch(
      get(fn, envir = ns)(res),
      error = function(e) NULL
    )
    
    if (!is.null(out))
      return(paste(unlist(out), collapse = "\n\n"))
  }
  
  if (is.list(res) && !is.null(res[[type]]))
    return(paste(unlist(res[[type]]), collapse = "\n\n"))
  
  paste(capture.output(print(res)), collapse = "\n")
}

# ---- HABILLAGE ----------------------------------------------------------------
# CSS_CAFE : feuille de style de toute l'application (thème café), injectée dans l'en-tête par ui.R.
CSS_CAFE <- "
body {
  background: #f6efe6;
  color: #3b2a20;
  font-family: 'Source Sans 3', 'Segoe UI', Helvetica, Arial, sans-serif;
  font-size: 15px;
  line-height: 1.5;
}
 
/* Titres : police à empattements (Playfair Display), brun foncé */
h1, h2, h3, h4, h5 {
  font-family: 'Playfair Display', Georgia, 'Times New Roman', serif;
  color: #3e2415;
  font-weight: 600;
}
 
/* Titre principal de la page : soulignement caramel */
.container-fluid > .row:first-child h2 {
  margin: 18px 0 16px;
  padding-bottom: 10px;
  border-bottom: 2px solid #b9814a;
  letter-spacing: 0.3px;
}
 
/* Panneau latéral (« well ») : fond beige, bord fin */
.well {
  background: #efe3d3;
  border: 1px solid #e0cfb8;
  border-radius: 6px;
  box-shadow: none;
}
 
/* Onglets : soulignement caramel sous l'onglet actif */
.nav-tabs {
  border-bottom: 1px solid #d9c6ad;
}
 
.nav-tabs > li > a {
  color: #6f4e37;
  border: none;
  border-bottom: 3px solid transparent;
  border-radius: 0;
  font-weight: 600;
  margin-right: 6px;
}
 
.nav-tabs > li > a:hover {
  background: transparent;
  border-bottom-color: #d9b88f;
  color: #3e2415;
}
 
.nav-tabs > li.active > a,
.nav-tabs > li.active > a:hover,
.nav-tabs > li.active > a:focus {
  background: transparent;
  color: #3e2415;
  border: none;
  border-bottom: 3px solid #b9814a;
}
 
/* Boutons : principal = brun foncé ; secondaire = contour caramel */
.btn {
  border-radius: 4px;
  font-weight: 600;
}
 
.btn-primary {
  background: #6f4e37;
  border-color: #6f4e37;
  color: #fff;
}
 
.btn-primary:hover,
.btn-primary:focus,
.btn-primary:active {
  background: #4e3626;
  border-color: #4e3626;
}
 
.btn-default {
  background: #fbf7f1;
  border-color: #b9814a;
  color: #6f4e37;
}
 
.btn-default:hover,
.btn-default:focus {
  background: #efe3d3;
  border-color: #8c5a2b;
  color: #3e2415;
}
 
.form-control,
/* Champs de saisie et listes déroulantes */
.selectize-input {
  background: #fffdf9;
  border-color: #d9c6ad;
  border-radius: 4px;
  box-shadow: none;
}
 
.form-control:focus,
.selectize-input.focus {
  border-color: #b9814a;
  box-shadow: 0 0 0 2px rgba(185, 129, 74, 0.2);
}
 
.selectize-control.multi .selectize-input > div {
  background: #ead9c1;
  color: #3e2415;
  border-radius: 3px;
}
 
.selectize-dropdown .active {
  background: #efe3d3;
  color: #3e2415;
}
 
input[type=checkbox],
input[type=radio] {
  accent-color: #8c5a2b;
}
 
.irs-bar,
/* Curseurs (barre, poignée, bulles de valeur) aux couleurs du thème */
.irs--shiny .irs-bar {
  background: #b9814a;
  border-top-color: #b9814a;
  border-bottom-color: #b9814a;
}
 
.irs-from,
.irs-to,
.irs-single,
.irs--shiny .irs-from,
.irs--shiny .irs-to,
.irs--shiny .irs-single {
  background: #6f4e37;
}
 
.irs--shiny .irs-line {
  background: #e6d6bf;
  border-color: #d9c6ad;
}
 
.irs--shiny .irs-handle {
  border-color: #6f4e37;
}
 
/* Messages (succès / erreur) */
.alert {
  border-radius: 6px;
}
 
.alert-success {
  background: #f1e4d0;
  border-color: #d9b88f;
  color: #3e2415;
}
 
.alert-danger {
  background: #f5e0da;
  border-color: #d8a89c;
  color: #5a1f14;
}
 
/* Tableaux : en-tête souligné, lignes alternées */
.table > thead > tr > th {
  border-bottom: 2px solid #b9814a;
  color: #3e2415;
  font-weight: 600;
}
 
.table-striped > tbody > tr:nth-of-type(odd) {
  background: #f1e7d9;
}
 
.table > tbody > tr > td {
  border-top: 1px solid #e6d6bf;
}
 
/* Textes d'aide, séparateurs, blocs de code */
.help-block {
  color: #8a7565;
  font-size: 13px;
}
 
hr {
  border-top: 1px solid #d9c6ad;
}
 
pre {
  background: #f1e7d9;
  border: 1px solid #e0cfb8;
  color: #3b2a20;
}
 
/* Encadré de l'avis du conseiller IA */
.avis-conseiller {
  white-space: pre-wrap;
  background: #fbf7f1;
  border-left: 4px solid #b9814a;
  padding: 14px 16px;
  border-radius: 4px;
}
 
/* Carte Leaflet : fond bleu océan, bord arrondi */
.leaflet-container {
  background: #a6cee3;
  border: 1px solid #d9c6ad;
  border-radius: 6px;
  font-family: inherit;
}
 
/* Fenêtre d'accueil (modale) : ombre, apparition en zoom */
.modal-content {
  background: #f6efe6;
  border: none;
  border-radius: 10px;
  box-shadow: 0 18px 50px rgba(43, 26, 18, 0.35);
}
 
.modal-footer {
  border-top: none;
  text-align: center;
}
 
.modal.fade .modal-dialog {
  transform: scale(0.85);
  opacity: 0;
  transition: transform 0.35s ease-out, opacity 0.35s ease-out;
}
 
.modal.in .modal-dialog {
  transform: scale(1);
  opacity: 1;
}
 
/* Bandeau supérieur : fond brun, logo à gauche, noms de l'équipe à droite ; version mobile en bas */
.bandeau-equipe {
  background: #3b2a20;
  min-height: 82px;
  display: flex;
  align-items: center;
  justify-content: space-between;
  gap: 30px;
  padding: 10px 30px;
  margin: -15px -15px 18px -15px;
  box-shadow: 0 2px 6px rgba(0,0,0,0.15);
}
 
.logo-institut {
  max-height: 58px;
  max-width: 260px;
  width: auto;
  height: auto;
  object-fit: contain;
}
 
.noms-equipe {
  color: #f6efe6;
  font-family: 'Source Sans 3', 'Segoe UI', Helvetica, Arial, sans-serif;
  font-size: 18px;
  font-weight: 600;
  text-align: right;
  line-height: 1.35;
}
 
@media (max-width: 700px) {
  .bandeau-equipe {
    padding: 10px 18px;
    gap: 15px;
  }
 
  .logo-institut {
    max-height: 48px;
    max-width: 190px;
  }
 
  .noms-equipe {
    font-size: 14px;
  }
}
 
/* Contenu de la fenêtre d'accueil (image, titre, texte) */
.accueil {
  text-align: center;
  padding: 6px 10px 0;
}
 
.accueil svg,
.accueil-img {
  max-width: 100%;
  height: auto;
  max-height: 260px;
  border-radius: 8px;
}
 
.accueil h2 {
  margin: 14px 0 6px;
}
 
.accueil p {
  color: #6f4e37;
  font-size: 16px;
  max-width: 420px;
  margin: 0 auto;
}
 
/* Page d'accueil : largeur limitée, titres de section, listes */
.page-accueil {
  max-width: 980px;
}
 
.page-accueil h3 {
  margin-top: 30px;
  padding-bottom: 6px;
  border-bottom: 1px solid #d9c6ad;
}
 
.page-accueil li {
  margin-bottom: 6px;
}
 
/* Encadré mis en avant */
.encart {
  background: #fbf7f1;
  border-left: 4px solid #b9814a;
  padding: 12px 16px;
  border-radius: 4px;
  margin: 14px 0;
}
 
"
# SVG_TASSE : illustration vectorielle (code SVG en texte) d'une tasse de café, affichée dans la fenêtre
# d'accueil quand aucune photo cafe.* n'est fournie. Aucun fichier ni connexion nécessaire.
SVG_TASSE <- '<svg viewBox="0 0 400 280" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Tasse de café">
<ellipse cx="200" cy="236" rx="152" ry="26" fill="#d9c4a8"/>
<ellipse cx="200" cy="229" rx="140" ry="22" fill="#efe3d3"/>
<path d="M290 126 C338 118 340 190 282 188" fill="none" stroke="#6f4e37" stroke-width="14" stroke-linecap="round"/>
<path d="M290 126 C338 118 340 190 282 188" fill="none" stroke="#f6efe6" stroke-width="5" stroke-linecap="round"/>
<path d="M110 112 H290 V172 C290 210 255 234 200 234 C145 234 110 210 110 172 Z" fill="#fbf7f1" stroke="#6f4e37" stroke-width="4"/>
<ellipse cx="200" cy="112" rx="90" ry="18" fill="#3e2415" stroke="#6f4e37" stroke-width="4"/>
<ellipse cx="200" cy="110" rx="62" ry="9" fill="#8c5a2b" opacity="0.6"/>
<g transform="translate(52 246) rotate(-25)"><ellipse rx="21" ry="13" fill="#6f4e37"/><path d="M-19 0 C-7 -6 7 6 19 0" fill="none" stroke="#3e2415" stroke-width="3"/></g>
<g transform="translate(348 250) rotate(20)"><ellipse rx="21" ry="13" fill="#6f4e37"/><path d="M-19 0 C-7 -6 7 6 19 0" fill="none" stroke="#3e2415" stroke-width="3"/></g>
</svg>'

# photo_accueil : nom d'une photo cafe.jpg / .png / .webp du dossier www/ (NA si absente).
photo_accueil <- list.files(
  "www",
  pattern = "^cafe\\.(jpe?g|png|webp)$",
  ignore.case = TRUE
)[1]

# image_accueil : la photo si elle existe, sinon l'illustration SVG. Utilisée par server.R dans la fenêtre d'accueil.
image_accueil <- if (!is.na(photo_accueil)) tags$img(src = photo_accueil, class = "accueil-img", alt = "Café") else HTML(SVG_TASSE)

# theme_cafe(p) : applique la même police, la même palette et un fond transparent à un graphique plotly.
theme_cafe <- function(p) {
  layout(
    p,
    font = list(
      family = "Source Sans 3, Segoe UI, Arial, sans-serif",
      color = "#3b2a20"
    ),
    colorway = c(
      "#6f4e37",
      "#c27c3c",
      "#7d8a5b",
      "#a8503f"
    ),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor = "rgba(0,0,0,0)"
  )
}

# ---- PAGE D'ACCUEIL -----------------------------------------------------------

# pct(x) : pourcentage de VRAI dans un vecteur logique, arrondi.
pct <- function(x) paste0(round(100 * mean(x)), " %")

# Répartition des lots par espèce, mise en phrase (ex. « Arabica (1300), Robusta (28) »).
n_esp <- table(cafe$espece)
txt_especes <- paste(sprintf("%s (%d)", names(n_esp), as.integer(n_esp)), collapse = ", ")

# Période de récolte affichée sur l'accueil (années min et max).
txt_periode <- if (any(!is.na(cafe$annee))) sprintf("%d à %d", min(cafe$annee, na.rm = TRUE), max(cafe$annee, na.rm = TRUE)) else "non renseignée"

# CHIFFRES : liste à puces « En bref » du panneau latéral de l'accueil, calculée sur les vraies données.
CHIFFRES <- tags$ul(
  tags$li(
    strong(format(nrow(cafe), big.mark = " ")),
    " lots de café"
  ),
  tags$li(
    strong(length(pays_liste)),
    " pays d'origine"
  ),
  tags$li(
    "Récoltes de ",
    strong(txt_periode)
  ),
  tags$li(
    "Espèces : ",
    txt_especes
  ),
  tags$li(
    "Source : Coffee Quality Institute, via Kaggle (mai 2023)"
  )
)

# PHRASES_CAFE : suggestions proposées dans la zone « Dites-moi ce que vous aimez » (onglet Choix de café).
PHRASES_CAFE <- c(
  "Un café doux et sucré pour le matin",
  "Un café fruité et acidulé, à boire sans lait",
  "Un café corsé et rond pour l'après-repas",
  "Un café très aromatique pour le week-end",
  "Un café équilibré, facile à boire tous les jours",
  "Un café peu acide, tout en rondeur",
  "Un café d'exception, très bien noté",
  "Un café lavé, net et frais",
  "Un café nature, fruité et sucré",
  "Un café léger et délicat, pas trop corsé")

# ---- AFDM : préparation ---------------------------------------------------------
# Sans FactoMineR, l'onglet AFDM affichera un message au lieu de planter.
FACTOMINER_OK <- requireNamespace("FactoMineR", quietly = TRUE)
if (!FACTOMINER_OK) message("FactoMineR absent : l'onglet AFDM sera inactif (install.packages('FactoMineR')).")

# regroupe(x, n_min, autre) : remplace les modalités rares par une modalité commune.
regroupe <- function(x, n_min, autre) {
  x <- as.character(x); n <- table(x)
  x[x %in% names(n)[n < n_min]] <- autre
  x
}

# LAB_AF : libellés français des variables de l'AFDM (critères, altitude, score, pays, méthode, espèce).
LAB_AF <- c(ATTR_LAB, altitude = "Altitude", score = "Score total",
            pays = "Pays", methode = "Méthode", espece = "Espèce")
# COULEURS_POP : 24 couleurs vives et distinctes, une par pays dans le nuage de lots de l'AFDM.
COULEURS_POP <- c("#E6194B", "#3CB44B", "#4363D8", "#F58231", "#911EB4", "#00A6C8",
                  "#F032E6", "#7CB518", "#008080", "#FF1493", "#9A6324", "#FFB000",
                  "#1F3A93", "#00C781", "#C2185B", "#6A1B9A", "#FF6F00", "#2E7D32",
                  "#D500F9", "#00B0FF", "#FF5252", "#795548", "#37474F", "#AEEA00")

# ---- 4. INTERFACE -------------------------------------------------------------

# Logo : fichier de www/ dont le nom est donné ci-dessous ; à défaut, première image de www/ autre que cafe.*.
LOGO_INSTITUT <- "Logo_Institut_Agro_Rennes_Angers.png"
if (!file.exists(file.path("www", LOGO_INSTITUT))) {
  autres <- list.files("www", pattern = "\\.(png|jpe?g|svg|webp)$", ignore.case = TRUE)
  autres <- autres[!grepl("^cafe\\.", autres, ignore.case = TRUE)]
  LOGO_INSTITUT <- if (length(autres)) autres[1] else NA
}
logo <- if (!is.na(LOGO_INSTITUT)) {
  tags$img(src = LOGO_INSTITUT, class = "logo-institut", alt = "Institut Agro Rennes-Angers")
} else {
  message("Logo introuvable dans www/ : déposez-y le PNG et indiquez son nom dans LOGO_INSTITUT.")
  NULL
}

NOMS_EQUIPE <- "Camille Delage • Satine Sandrin"