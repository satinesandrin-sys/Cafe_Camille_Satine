# =============================================================================
# EBAUCHE v0 - Choisir son café : carte, profil sensoriel, prédiction de qualité
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

# ---- 3. PARAMÈTRES D'INTERFACE ------------------------------------------------

alt_max <- if (any(!is.na(cafe$altitude)))
  min(4000, ceiling(max(cafe$altitude, na.rm = TRUE) / 100) * 100)
else
  3000

an_range <- if (any(!is.na(cafe$annee)))
  range(cafe$annee, na.rm = TRUE)
else
  c(2010, 2023)

methodes <- sort(unique(cafe$methode))
pays_liste <- sort(unique(cafe$pays))

IND <- c(
  "Score total moyen" = "score",
  "Nombre de lots" = "n",
  setNames(attr_cols, paste("Note moyenne :", ATTR_LAB[attr_cols]))
)

MOTS <- c(
  aroma = "très aromatique",
  flavor = "riche en saveurs",
  aftertaste = "longue en bouche",
  acidity = "vive et acidulée",
  body = "ronde et corsée",
  balance = "équilibrée",
  sweetness = "douce et sucrée"
)

KEY <- intersect(
  c("sweetness", "acidity", "aroma", "body"),
  attr_cols
)

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

NAILER_OK <- requireNamespace("NaileR", quietly = TRUE)

ollama_actif <- function() {
  isTRUE(tryCatch({
    con <- url("http://localhost:11434")
    on.exit(close(con))
    readLines(con, n = 1, warn = FALSE)
    TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE))
}

modeles_ollama <-
  if (requireNamespace("ollamar", quietly = TRUE) && ollama_actif())
    tryCatch(
      ollamar::list_models()$name,
      error = function(e) character()
    )
else
  character()

modeles_ollama <- unique(
  c(modeles_ollama, "llama3.1", "mistral-small3.2")
)

NIVEAUX <- c(
  "pas du tout",
  "peu",
  "moyennement",
  "assez",
  "très"
)

ADJ <- c(
  sweetness = "sucré",
  acidity = "acide",
  aroma = "aromatique",
  body = "corsé",
  flavor = "savoureux",
  aftertaste = "long en bouche",
  balance = "équilibré"
)

NOMS_LLM <- c(
  aroma = "Arome",
  flavor = "Saveur",
  aftertaste = "Longueur_en_bouche",
  acidity = "Acidite",
  body = "Corps",
  balance = "Equilibre",
  sweetness = "Douceur_sucre"
)

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

CSS_CAFE <- "
body {
  background: #f6efe6;
  color: #3b2a20;
  font-family: 'Source Sans 3', 'Segoe UI', Helvetica, Arial, sans-serif;
  font-size: 15px;
  line-height: 1.5;
}

h1, h2, h3, h4, h5 {
  font-family: 'Playfair Display', Georgia, 'Times New Roman', serif;
  color: #3e2415;
  font-weight: 600;
}

.container-fluid > .row:first-child h2 {
  margin: 18px 0 16px;
  padding-bottom: 10px;
  border-bottom: 2px solid #b9814a;
  letter-spacing: 0.3px;
}

.well {
  background: #efe3d3;
  border: 1px solid #e0cfb8;
  border-radius: 6px;
  box-shadow: none;
}

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

.avis-conseiller {
  white-space: pre-wrap;
  background: #fbf7f1;
  border-left: 4px solid #b9814a;
  padding: 14px 16px;
  border-radius: 4px;
}

.leaflet-container {
  background: #a6cee3;
  border: 1px solid #d9c6ad;
  border-radius: 6px;
  font-family: inherit;
}

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

.encart {
  background: #fbf7f1;
  border-left: 4px solid #b9814a;
  padding: 12px 16px;
  border-radius: 4px;
  margin: 14px 0;
}

.vapeur {
  animation: vapeur 3.4s ease-in-out infinite;
}

@keyframes vapeur {
  0% {
    opacity: 0;
    transform: translateY(10px);
  }

  40% {
    opacity: 0.85;
  }

  100% {
    opacity: 0;
    transform: translateY(-12px);
  }
}
"

SVG_TASSE <- '<svg viewBox="0 0 400 280" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Tasse de café fumante">
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

photo_accueil <- list.files(
  "www",
  pattern = "^cafe\\.(jpe?g|png|webp)$",
  ignore.case = TRUE
)[1]

image_accueil <-
  if (!is.na(photo_accueil))
    tags$img(
      src = photo_accueil,
      class = "accueil-img",
      alt = "Café"
    )
else
  HTML(SVG_TASSE)

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

src_col <- function(cands) {
  x <- intersect(cands, names(brut))
  if (length(x)) x[1] else "non trouvée"
}

pct <- function(x)
  paste0(round(100 * mean(x)), " %")

n_esp <- table(cafe$espece)

txt_especes <- paste(
  sprintf("%s (%d)", names(n_esp), as.integer(n_esp)),
  collapse = ", "
)

txt_periode <-
  if (any(!is.na(cafe$annee)))
    sprintf(
      "%d à %d",
      min(cafe$annee, na.rm = TRUE),
      max(cafe$annee, na.rm = TRUE)
    )
else
  "non renseignée"

q_att <-
  if (length(attr_cols))
    quantile(
      unlist(cafe[attr_cols]),
      c(0.05, 0.95),
      na.rm = TRUE
    )
else
  c(NA, NA)

q_sc <- quantile(
  cafe$score,
  c(0.05, 0.95),
  na.rm = TRUE
)

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

# ---- 4. INTERFACE -------------------------------------------------------------

# NOM EXACT DU FICHIER PNG DANS www/
LOGO_INSTITUT <- "NOM_EXACT_DE_TON_PNG.png"

NOMS_EQUIPE <- "Camille Delage • Satine Sandrin"

ui <- fluidPage(
  tags$head(
    tags$link(
      rel = "stylesheet",
      href = "https://fonts.googleapis.com/css2?family=Playfair+Display:wght@500;600&family=Source+Sans+3:wght@400;600&display=swap"
    ),
    tags$style(HTML(CSS_CAFE))
  ),
  
  div(
    class = "bandeau-equipe",
    
    tags$img(
      src = LOGO_INSTITUT,
      class = "logo-institut",
      alt = "Institut Agro Rennes-Angers"
    ),
    
    div(
      class = "noms-equipe",
      NOMS_EQUIPE
    )
  ),
  
  titlePanel(
    "Choisir son café",
    windowTitle = "Choisir son café"
  ),
  
  sidebarLayout(
    
    sidebarPanel(
      width = 3,
      
      conditionalPanel(
        "input.onglet == 'accueil'",
        h4("En bref"),
        CHIFFRES
      ),
      
      conditionalPanel(
        "input.onglet == 'carte' || input.onglet == 'sens'",
        h4("Filtres"),
        
        selectizeInput(
          "f_pays",
          "Pays (vide = tous)",
          pays_liste,
          multiple = TRUE,
          options = list(placeholder = "Tous les pays")
        ),
        
        sliderInput(
          "f_alt",
          "Altitude (m)",
          0,
          alt_max,
          c(0, alt_max),
          step = 100
        ),
        
        checkboxInput(
          "f_alt_na",
          "Inclure altitude inconnue",
          TRUE
        ),
        
        sliderInput(
          "f_an",
          "Année de récolte",
          an_range[1],
          an_range[2],
          an_range,
          step = 1,
          sep = ""
        ),
        
        checkboxInput(
          "f_an_na",
          "Inclure année inconnue",
          TRUE
        ),
        
        checkboxGroupInput(
          "f_meth",
          "Méthode de production",
          methodes,
          methodes
        ),
        
        checkboxGroupInput(
          "f_esp",
          "Espèce",
          unique(cafe$espece),
          unique(cafe$espece)
        ),
        
        textOutput("n_lots")
      ),
      
      conditionalPanel(
        "input.onglet == 'carte'",
        actionButton(
          "reset_filtres",
          "Réinitialiser les filtres",
          icon = icon("refresh"),
          class = "btn-default"
        )
      ),
      
      conditionalPanel(
        "input.onglet == 'choix'",
        
        h4("Mes goûts"),
        
        lapply(
          KEY,
          function(a)
            sliderInput(
              paste0("c_", a),
              ATTR_LAB[[a]],
              1,
              5,
              3,
              step = 1,
              ticks = FALSE
            )
        ),
        
        helpText("1 = pas du tout, 5 = beaucoup"),
        
        selectInput(
          "c_meth",
          "Méthode de production (facultatif)",
          c(
            "Peu importe" = "",
            setdiff(methodes, "Non renseigné")
          )
        ),
        
        checkboxInput(
          "c_qual",
          "Privilégier les cafés les mieux notés",
          FALSE
        )
      )
    ),
    
    mainPanel(
      width = 9,
      
      tabsetPanel(
        id = "onglet",
        
        tabPanel("Accueil", value = "accueil",
                 br(),
                 div(class = "page-accueil",
                     h3("Le projet"),
                     p("Au supermarché, le choix d'un café repose sur peu d'indications : un pays, parfois",
                       "la mention « arabica ». Cette application traduit les notes de dégustateurs",
                       "professionnels en repères simples (sucré, acide, aromatique, corsé) pour vous aider",
                       "à choisir : explorez la carte, comparez les profils de goût, puis décrivez vos envies."),
                     
                     h3("Le jeu de données"),
                     p(sprintf("%s lots de café évalués par des dégustateurs certifiés du Coffee Quality Institute",
                               format(nrow(cafe), big.mark = " ")),
                       sprintf("(version de mai 2023, Kaggle), issus de %d pays et récoltés entre %s.",
                               length(pays_liste), txt_periode),
                       "Chaque ligne du jeu est un lot de café, décrit par sa provenance, son mode de production",
                       "et les notes que lui ont données les dégustateurs."),
                     
                     h3("Les variables utilisées"),
                     tableOutput("acc_variables"),
                     
                     h3("À garder en tête"),
                     tags$ul(
                       tags$li("Ce sont surtout des cafés de spécialité : les scores sont des repères relatifs",
                               "entre pays, pas la note d'un paquet de supermarché."),
                       tags$li(sprintf("Le pays est une échelle large, et les données sont incomplètes (altitude inconnue pour %s des lots, méthode pour %s).",
                                       pct(is.na(cafe$altitude)), pct(cafe$methode == "Non renseigné"))),
                       tags$li("Le texte du conseiller IA est une interprétation des résultats, pas un résultat.")))),
        
        tabPanel(
          "Carte",
          value = "carte",
          br(),
          
          fluidRow(
            column(
              8,
              selectInput(
                "indicateur",
                "Colorer les pays selon",
                IND
              )
            ),
            
            column(
              4,
              checkboxInput(
                "fond_osm",
                "Afficher le fond OpenStreetMap",
                FALSE
              )
            )
          ),
          
          leafletOutput("carte", height = 480),
          h4("Données utilisées par la carte"),
          helpText("Le tableau suit les filtres de gauche et est trié selon l'indicateur choisi."),
          div(style = "max-height: 320px; overflow-y: auto;", tableOutput("table_carte")),
          br(),
          uiOutput("detail_pays")),
        
        tabPanel(
          "Profil sensoriel",
          value = "sens",
          br(),
          
          selectizeInput("s_pays",
                         "Choisissez de 1 à 4 pays pour comparer leur profil de goût :",
                         pays_liste, multiple = TRUE,
                         options = list(maxItems = 4,
                                        placeholder = "Cliquez ici, puis tapez un nom de pays")),
          
          checkboxInput(
            "s_tous",
            "Afficher tous les critères (sinon : sucré, acide, arôme, corps)",
            FALSE
          ),
          
          fluidRow(
            column(
              7,
              plotlyOutput("radar", height = 420)
            ),
            
            column(
              5,
              h4("En quelques mots"),
              uiOutput("mots")
            )
          ),
          h4("Un profil par pays"),
          uiOutput("radars_pays"),
          
          helpText(
            "Le profil est calculé sur les lots filtrés à gauche.",
            "Les notes vont de 0 à 10 ; l'axe du radar est zoomé",
            "pour faire ressortir les différences."
          )
        ),
        
        tabPanel(
          "Choix de café (conseiller IA)",
          value = "choix",
          br(),
          
          wellPanel(
            h4("Dites-moi ce que vous aimez"),
            
            selectizeInput("c_texte", NULL, choices = PHRASES_CAFE, width = "100%",
                           options = list(
                             create = TRUE, createOnBlur = TRUE, persist = FALSE, maxOptions = 8,
                             placeholder = "Tapez vos envies ou choisissez une suggestion...",
                             onInitialize = I('function() { this.setValue(""); }'),
                             render = I('{ option_create: function(data, escape) { return "<div>Utiliser : <strong>" + escape(data.input) + "</strong></div>"; } }'))),
            
            fluidRow(
              column(
                5,
                selectizeInput(
                  "c_model",
                  "Modèle Ollama",
                  modeles_ollama,
                  selected = modeles_ollama[1],
                  options = list(create = TRUE)
                )
              ),
              
              column(
                7,
                br(),
                actionButton(
                  "c_interp",
                  "Interpréter mes envies",
                  class = "btn-primary"
                )
              )
            ),
            
            uiOutput("c_interp_res"),
            
            helpText(
              "L'IA règle les curseurs de gauche à partir de votre texte ;",
              "vous pouvez ensuite les ajuster à la main."
            )
          ),
          
          h4("Les pays qui correspondent à vos goûts"),
          
          tableOutput("c_classement"),
          
          helpText(
            "Compatibilité relative : 100 = pays le plus proche de vos goûts, 0 = le plus éloigné."
          ),
          
          hr(),
          
          h4("L'avis du conseiller"),
          
          fluidRow(
            column(
              4,
              selectInput(
                "c_pays",
                "Pays à expliquer",
                NULL
              )
            ),
            
            column(
              4,
              br(),
              actionButton(
                "c_go",
                "Demander au conseiller"
              )
            ),
            
            column(
              4,
              br(),
              checkboxInput(
                "c_apercu",
                "Voir seulement le prompt (sans LLM)",
                FALSE
              )
            )
          ),
          
          uiOutput("c_profil"),
          uiOutput("c_reponse"),
          
          helpText(
            textOutput("c_statut", inline = TRUE)
          ),
          
          helpText(
            "Le LLM ne reçoit que des résultats statistiques calculés dans R",
            "(NaileR) ; son texte est une interprétation, pas un résultat.",
            "Les lots CQI sont surtout des cafés de spécialité : les conseils",
            "valent comme repères relatifs entre pays."
          )
        )
      )
    )
  )
)

# ---- 5. SERVEUR ---------------------------------------------------------------

server <- function(input, output, session) {
  
  output$acc_variables <- renderTable({
    data.frame(
      Variable = c("Score total (Total Cup Points)", "Critères sensoriels", "Pays d'origine",
                   "Altitude", "Année de récolte", "Méthode de production", "Compatibilité"),
      Définition = c(
        "La note globale d'un lot : la somme des notes des dégustateurs.",
        paste0("Les notes de goût du lot : ", paste(unname(ATTR_LAB[attr_cols]), collapse = ", "),
               ". Nous mettons en avant quatre axes : sucré, acide, arôme et corps."),
        "Le pays où le café a été cultivé.",
        "L'altitude de la plantation (moyenne quand elle est donnée en fourchette).",
        "L'année de la récolte.",
        "Le traitement des cerises après la récolte : lavé, nature (séchage de la cerise entière), semi-lavé, honey / pulped natural.",
        "Calculée par l'application : proximité entre vos goûts et le profil moyen d'un pays."),
      Échelle = c("environ 0 à 100 ; 80 et plus : café de spécialité", "0 à 10 par critère",
                  "texte", "mètres", "année", "catégories",
                  "0 à 100, relative : 100 = pays le plus proche de vos goûts"),
      check.names = FALSE)
  }, striped = TRUE, spacing = "s")
  
  showModal(
    modalDialog(
      div(
        class = "accueil",
        image_accueil,
        h2("Choisir son café"),
        p(
          "Parcourez la carte des cafés du monde, découvrez leurs profils de goût",
          "et laissez le conseiller vous orienter selon vos envies."
        )
      ),
      footer = modalButton("Découvrir"),
      easyClose = TRUE,
      size = "m",
      fade = TRUE
    )
  )
  
  filtre <- reactive({
    d <- cafe
    
    if (length(input$f_pays))
      d <- d[d$pays %in% input$f_pays, ]
    
    ok <- !is.na(d$altitude) &
      d$altitude >= input$f_alt[1] &
      d$altitude <= input$f_alt[2]
    
    d <- d[
      ok | (is.na(d$altitude) & input$f_alt_na),
    ]
    
    ok <- !is.na(d$annee) &
      d$annee >= input$f_an[1] &
      d$annee <= input$f_an[2]
    
    d <- d[
      ok | (is.na(d$annee) & input$f_an_na),
    ]
    
    d[
      d$methode %in% input$f_meth &
        d$espece %in% input$f_esp,
    ]
  })
  
  # critères utilisables pour la corrélation : ceux qui ne sont pas constants
  cols_corr <- attr_cols[vapply(attr_cols, function(a)
    isTRUE(sd(cafe[[a]], na.rm = TRUE) > 0), logical(1))]
  if (length(cols_corr) < length(attr_cols))
    message("Critères exclus de la matrice (constants) : ",
            paste(setdiff(attr_cols, cols_corr), collapse = ", "))
  
  output$n_lots <- renderText(
    sprintf(
      "%d lots correspondent aux filtres.",
      nrow(filtre())
    )
  )
  
  observeEvent(input$reset_filtres, {
    
    updateSelectizeInput(
      session,
      "f_pays",
      selected = character(0)
    )
    
    updateSliderInput(
      session,
      "f_alt",
      value = c(0, alt_max)
    )
    
    updateCheckboxInput(
      session,
      "f_alt_na",
      value = TRUE
    )
    
    updateSliderInput(
      session,
      "f_an",
      value = an_range
    )
    
    updateCheckboxInput(
      session,
      "f_an_na",
      value = TRUE
    )
    
    updateCheckboxGroupInput(
      session,
      "f_meth",
      selected = methodes
    )
    
    updateCheckboxGroupInput(
      session,
      "f_esp",
      selected = unique(cafe$espece)
    )
  })
  
  stats_pays <- reactive({
    
    d <- filtre()
    d <- d[!is.na(d$iso), ]
    
    if (!nrow(d))
      return(NULL)
    
    do.call(
      rbind,
      lapply(
        split(d, d$iso),
        function(x)
          data.frame(
            iso = x$iso[1],
            pays = x$pays[1],
            n = nrow(x),
            score = mean(x$score, na.rm = TRUE),
            as.list(colMeans(x[attr_cols], na.rm = TRUE))
          )
      )
    )
  })
  
  # ---- CARTE -----------------------------------------------------------------
  
  output$carte <- renderLeaflet({
    
    leaflet(
      options = leafletOptions(minZoom = 2)
    ) |>
      setView(
        lng = 10,
        lat = 15,
        zoom = 2
      )
  })
  
  observeEvent(input$fond_osm, {
    
    proxy <- leafletProxy("carte") |>
      clearTiles()
    
    if (isTRUE(input$fond_osm))
      proxy |> addTiles()
  })
  
  carte_prete <- reactiveVal(FALSE)
  observeEvent(input$carte_bounds, carte_prete(TRUE), once = TRUE)
  
  observe({
    req(carte_prete())
    st <- stats_pays()
    
    proxy <- leafletProxy("carte") |>
      clearShapes() |>
      clearControls()
    
    if (is.null(st))
      return()
    
    v <- st[[input$indicateur]][
      match(monde$ISO_A3_EH, st$iso)
    ]
    
    rng <- range(v, na.rm = TRUE)
    
    if (!is.finite(rng[1]))
      return()
    
    if (diff(rng) == 0)
      rng <- rng + c(-0.5, 0.5)
    
    pal <- colorNumeric(
      c(
        "#f2e4cf",
        "#d2a572",
        "#a56a35",
        "#6b3f1f",
        "#33190d"
      ),
      rng,
      na.color = "#e9dfd0"
    )
    
    i <- match(
      monde$ISO_A3_EH,
      st$iso
    )
    
    etiq <- ifelse(
      is.na(i),
      sprintf(
        "<b>%s</b><br>pas de lot",
        monde$NAME
      ),
      sprintf(
        "<b>%s</b><br>%d lots<br>Score moyen : %.1f",
        st$pays[i],
        st$n[i],
        st$score[i]
      )
    )
    
    nom_ind <- names(IND)[IND == input$indicateur]
    
    proxy |>
      addPolygons(
        data = monde,
        layerId = ~ISO_A3_EH,
        fillColor = pal(v),
        fillOpacity = 0.85,
        color = "white",
        weight = 0.7,
        label = lapply(etiq, HTML),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#333"
        )
      ) |>
      addLegend(
        "bottomright",
        pal = pal,
        values = v,
        title = nom_ind,
        opacity = 0.9
      )
  })
  
  output$table_carte <- renderTable({
    st <- stats_pays(); req(st)
    st <- st[order(-st[[input$indicateur]]), ]
    out <- data.frame(Pays = st$pays, Lots = st$n,
                      `Score total moyen` = round(st$score, 2), check.names = FALSE)
    for (a in attr_cols) out[[ATTR_LAB[[a]]]] <- round(st[[a]], 2)
    out
  }, striped = TRUE, spacing = "xs")
  
  pays_clic <- reactiveVal(NULL)
  
  observeEvent(
    input$carte_shape_click,
    pays_clic(input$carte_shape_click$id)
  )
  
  output$detail_pays <- renderUI({
    
    iso <- pays_clic()
    
    if (is.null(iso))
      return(
        helpText(
          "Cliquez sur un pays pour voir le détail de ses cafés."
        )
      )
    
    d <- filtre()
    d <- d[!is.na(d$iso) & d$iso == iso, ]
    
    if (!nrow(d))
      return(
        helpText(
          "Aucun lot pour ce pays avec les filtres actuels."
        )
      )
    
    tagList(
      
      h4(d$pays[1]),
      
      p(
        sprintf(
          "%d lots - score moyen %.1f - altitude médiane %s m",
          nrow(d),
          mean(d$score, na.rm = TRUE),
          ifelse(
            all(is.na(d$altitude)),
            "n.d.",
            round(median(d$altitude, na.rm = TRUE))
          )
        )
      ),
      
      fluidRow(
        column(
          6,
          plotlyOutput(
            "hist_pays",
            height = 260
          )
        ),
        
        column(
          6,
          h5("Les 10 meilleurs lots"),
          tableOutput("top_pays")
        )
      )
    )
  })
  
  d_pays <- reactive({
    d <- filtre()
    d[
      !is.na(d$iso) &
        d$iso == pays_clic(),
    ]
  })
  
  output$hist_pays <- renderPlotly({
    
    plot_ly(
      d_pays(),
      x = ~score,
      type = "histogram",
      marker = list(color = "#8c5a2b")
    ) |>
      layout(
        xaxis = list(title = "Score total"),
        yaxis = list(title = "Lots")
      ) |>
      theme_cafe() |>
      config(displayModeBar = FALSE)
  })
  
  output$top_pays <- renderTable({
    
    d <- head(
      d_pays()[order(-d_pays()$score), ],
      10
    )
    
    data.frame(
      Score = round(d$score, 1),
      Méthode = d$methode,
      `Altitude (m)` = round(d$altitude),
      Année = d$annee,
      Variété = d$variete,
      check.names = FALSE
    )
    
  }, na = "n.d.", striped = TRUE, spacing = "xs")
  
  # ---- PROFIL SENSORIEL ------------------------------------------------------
  
  output$radar <- renderPlotly({
    
    req(length(input$s_pays) > 0)
    
    d <- filtre()
    
    cols <- if (input$s_tous)
      attr_cols
    else
      KEY
    
    labs <- unname(ATTR_LAB[cols])
    
    moy <- lapply(
      input$s_pays,
      function(p)
        colMeans(
          d[d$pays == p, cols, drop = FALSE],
          na.rm = TRUE
        )
    )
    
    toutes <- unlist(moy)
    toutes <- toutes[is.finite(toutes)]
    
    req(length(toutes) > 0)
    
    p <- plot_ly(
      type = "scatterpolar",
      mode = "lines+markers",
      fill = "toself"
    )
    
    for (k in seq_along(moy)) {
      
      m <- moy[[k]]
      
      p <- add_trace(
        p,
        r = c(m, m[1]),
        theta = c(labs, labs[1]),
        name = input$s_pays[k]
      )
    }
    
    layout(
      p,
      polar = list(
        radialaxis = list(
          range = c(
            floor(min(toutes) * 2) / 2 - 0.5,
            ceiling(max(toutes) * 2) / 2 + 0.25
          )
        )
      ),
      legend = list(orientation = "h")
    ) |>
      theme_cafe()
  })
  
  # un radar par pays sur la même échelle, pour rester comparables
  COUL_PAYS <- c("#6f4e37", "#c27c3c", "#7d8a5b", "#a8503f")
  output$radars_pays <- renderUI({
    n <- length(input$s_pays); req(n > 0)
    # on découpe les pays par paires : une ligne = 2 radars au maximum
    lignes <- unname(split(seq_len(n), ceiling(seq_len(n) / 2)))
    tagList(lapply(lignes, function(ids)
      fluidRow(lapply(ids, function(k)
        column(6, plotlyOutput(paste0("radar_", k), height = 380))))))
  })
  for (k in 1:4) local({
    kk <- k
    output[[paste0("radar_", kk)]] <- renderPlotly({
      pays <- input$s_pays; req(length(pays) >= kk)
      d <- filtre(); cols <- if (input$s_tous) attr_cols else KEY
      labs <- unname(ATTR_LAB[cols])
      moy <- lapply(pays, function(p) colMeans(d[d$pays == p, cols, drop = FALSE], na.rm = TRUE))
      toutes <- unlist(moy); toutes <- toutes[is.finite(toutes)]
      req(length(toutes) > 0)
      m <- moy[[kk]]
      plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself",
              r = c(m, m[1]), theta = c(labs, labs[1]), name = pays[kk],
              line = list(color = COUL_PAYS[kk])) |>
        layout(title = list(text = pays[kk], x = 0.5), margin = list(t = 60),
               showlegend = FALSE,
               polar = list(radialaxis = list(range = c(floor(min(toutes) * 2) / 2 - 0.5,
                                                        ceiling(max(toutes) * 2) / 2 + 0.25)))) |>
        theme_cafe()
    })
  })
  
  output$mots <- renderUI({
    
    req(length(input$s_pays) > 0)
    
    d <- filtre()
    
    cols <- if (input$s_tous)
      attr_cols
    else
      KEY
    
    ref <- colMeans(
      d[cols],
      na.rm = TRUE
    )
    
    tagList(
      lapply(
        input$s_pays,
        function(pp) {
          
          m <- colMeans(
            d[d$pays == pp, cols, drop = FALSE],
            na.rm = TRUE
          )
          
          tags$p(
            strong(pp),
            ":",
            phrase_gout(m, ref),
            "(par rapport à l'ensemble des lots filtrés)"
          )
        }
      )
    )
  })
  
  # ---- CHOIX DE CAFÉ ---------------------------------------------------------
  
  classement <- reactive({
    
    for (a in KEY)
      req(input[[paste0("c_", a)]])
    
    req(!is.null(input$c_meth))
    
    avec_meth <- nzchar(input$c_meth)
    
    d <- cafe[
      !is.na(cafe$pays) &
        !is.na(cafe$score),
    ]
    
    if (avec_meth)
      d <- d[d$methode == input$c_meth, ]
    
    n <- table(d$pays)
    
    d <- d[
      d$pays %in%
        names(n)[
          n >= if (avec_meth) 5 else 10
        ],
    ]
    
    validate(
      need(
        nrow(d) > 0,
        "Pas assez de lots avec ces critères."
      )
    )
    
    qual <- isTRUE(input$c_qual)
    
    dims <- c(
      KEY,
      if (qual) "score"
    )
    
    moy <- aggregate(
      d[c(KEY, "score")],
      by = list(pays = d$pays),
      FUN = mean,
      na.rm = TRUE
    )
    
    nb <- as.integer(
      table(d$pays)[moy$pays]
    )
    
    Z <- scale(moy[dims])
    Z[is.na(Z)] <- 0
    
    cible <- c(
      vapply(
        KEY,
        function(a)
          (input[[paste0("c_", a)]] - 3) * 0.75,
        numeric(1)
      ),
      if (qual) 1.5
    )
    
    dist <- sqrt(
      rowSums(
        sweep(Z, 2, cible)^2
      )
    )
    
    out <- data.frame(
      pays = moy$pays,
      compat = round(
        100 * (1 - dist / max(dist))
      ),
      n = nb,
      score = moy$score
    )
    
    out[order(-out$compat), ]
  })
  
  output$c_classement <- renderTable({
    
    cl <- head(
      classement(),
      5
    )
    
    data.frame(
      Pays = cl$pays,
      `Compatibilité (%)` = cl$compat,
      `Score CQI moyen` = round(cl$score, 1),
      check.names = FALSE
    )
    
  }, striped = TRUE, spacing = "xs")
  
  observe({
    
    cl <- tryCatch(
      classement(),
      error = function(e) NULL
    )
    
    if (!is.null(cl))
      updateSelectInput(
        session,
        "c_pays",
        choices = head(cl$pays, 5),
        selected = cl$pays[1]
      )
  })
  
  output$c_statut <- renderText({
    
    invalidateLater(15000)
    
    paste0(
      "NaileR : ",
      if (NAILER_OK)
        "installé"
      else
        "non installé",
      " | Ollama : ",
      if (ollama_actif())
        "détecté"
      else
        "non détecté (lancez l'application Ollama)"
    )
  })
  
  output$c_profil <- renderUI({
    
    req(input$c_pays)
    
    m <- colMeans(
      cafe[cafe$pays == input$c_pays, KEY],
      na.rm = TRUE
    )
    
    ref <- colMeans(
      cafe[KEY],
      na.rm = TRUE
    )
    
    tags$p(
      strong(input$c_pays),
      ":",
      phrase_gout(m, ref),
      "(par rapport à tous les cafés ; calcul statistique, sans IA)"
    )
  })
  
  msg_interp <- reactiveVal(NULL)
  
  observeEvent(input$c_interp, {
    
    txt <- trimws(
      paste(input$c_texte, collapse = " ")
    )
    
    if (!nzchar(txt))
      return(
        msg_interp(
          list(
            erreur = "Écrivez d'abord quelques mots sur vos envies."
          )
        )
      )
    
    if (!requireNamespace("ollamar", quietly = TRUE))
      return(
        msg_interp(
          list(
            erreur = "le package ollamar n'est pas installé (install.packages('ollamar'))."
          )
        )
      )
    
    meths <- setdiff(
      methodes,
      "Non renseigné"
    )
    
    gabarit <- paste0(
      "{",
      paste0(
        '"',
        KEY,
        '":3',
        collapse = ","
      ),
      ',"methode":"","qualite":false,"explication":"..."}'
    )
    
    prompt <- paste0(
      "Tu aides un consommateur à choisir un café. ",
      "Il décrit ses envies : « ",
      txt,
      " ».\n",
      "Traduis ces envies en niveaux de 1 (pas du tout) à 5 (beaucoup), ",
      "3 = indifférent, pour : ",
      paste0(
        '"',
        KEY,
        '" (',
        ADJ[KEY],
        ")",
        collapse = ", "
      ),
      ".\n",
      "Indique aussi \"methode\" parmi [",
      paste(meths, collapse = ", "),
      "] ou \"\" si rien n'est précisé, ",
      "\"qualite\" à true s'il veut le meilleur café possible, ",
      "et \"explication\" : une phrase en français qui reformule ce que tu as compris. ",
      "Ne traduis en niveaux que les critères listés ci-dessus ; si la personne évoque autre chose, ",
      "ignore-le et dis-le dans l'explication.\n",
      "Réponds uniquement par un objet JSON valide, sans aucun texte autour, de la forme ",
      gabarit
    )
    
    sortie <- withProgress(
      message = "Je lis vos envies...",
      value = 0.5,
      
      tryCatch(
        ollamar::generate(
          model = input$c_model,
          prompt = prompt,
          output = "text",
          stream = FALSE
        ),
        error = function(e) e
      )
    )
    
    if (inherits(sortie, "error"))
      return(
        msg_interp(
          list(
            erreur = conditionMessage(sortie)
          )
        )
      )
    
    json <- regmatches(
      sortie,
      regexpr(
        "\\{[^{}]*\\}",
        sortie
      )
    )
    
    pr <- if (length(json))
      tryCatch(
        jsonlite::fromJSON(json),
        error = function(e) NULL
      )
    else
      NULL
    
    if (is.null(pr))
      return(
        msg_interp(
          list(
            erreur = "réponse du LLM illisible, reformulez ou relancez."
          )
        )
      )
    
    for (a in KEY) {
      
      v <- suppressWarnings(
        as.integer(
          round(
            as.numeric(pr[[a]])
          )
        )
      )
      
      if (
        length(v) == 1 &&
        !is.na(v)
      ) {
        updateSliderInput(
          session,
          paste0("c_", a),
          value = max(
            1,
            min(5, v)
          )
        )
      }
    }
    
    m <- pr[["methode"]]
    
    updateSelectInput(
      session,
      "c_meth",
      selected =
        if (
          is.character(m) &&
          length(m) == 1 &&
          m %in% meths
        )
          m
      else
        ""
    )
    
    updateCheckboxInput(
      session,
      "c_qual",
      value = isTRUE(pr[["qualite"]])
    )
    
    msg_interp(
      list(
        texte =
          if (
            is.character(pr[["explication"]])
          )
            pr[["explication"]]
        else
          "Réglages mis à jour."
      )
    )
  })
  
  output$c_interp_res <- renderUI({
    
    r <- msg_interp()
    
    req(r)
    
    if (!is.null(r$erreur))
      return(
        div(
          class = "alert alert-danger",
          strong("Interprétation impossible : "),
          r$erreur,
          br(),
          "Vérifiez qu'Ollama est lancé et que le modèle est téléchargé."
        )
      )
    
    div(
      class = "alert alert-success",
      strong("J'ai compris : "),
      r$texte,
      " Les curseurs ont été réglés ; vous pouvez les modifier."
    )
  })
  
  # ---- CONSEILLER ------------------------------------------------------------
  
  reponse <- eventReactive(input$c_go, {
    
    req(input$c_pays)
    
    if (!NAILER_OK)
      return(
        list(
          erreur = "le package NaileR n'est pas installé (devtools::install_github('Sebastien-Le/NaileR'))."
        )
      )
    
    cible <- input$c_pays
    apercu <- isTRUE(input$c_apercu)
    
    txt_libre <- trimws(
      paste(input$c_texte, collapse = " ")
    )
    
    prefs <- paste(
      vapply(
        KEY,
        function(a)
          paste(
            NIVEAUX[
              input[[paste0("c_", a)]]
            ],
            ADJ[[a]]
          ),
        character(1)
      ),
      collapse = ", "
    )
    
    intro <- paste0(
      "Contexte : application d'aide au choix de café pour un consommateur de supermarché. ",
      "Les données sont des lots de café évalués par des dégustateurs (notes de 0 à 10 par critère ; ",
      "altitude de culture en classes ; méthode de production). Le groupe « ",
      cible,
      " » est comparé au groupe « Autres pays ». ",
      "Le consommateur recherche un café : ",
      prefs,
      ".",
      if (nzchar(txt_libre))
        paste0(
          " Il l'exprime ainsi : « ",
          txt_libre,
          " »."
        )
      else
        ""
    )
    
    demande <- paste0(
      "À partir des seuls éléments statistiques fournis, rédige en français, pour un consommateur ",
      "non spécialiste, un conseil de 5 à 8 lignes : décris le goût typique du café du groupe « ",
      cible,
      " », ce qui le distingue des autres pays, et dis s'il correspond aux goûts recherchés, ",
      "en nuançant. Ne cite pas de chiffres techniques ni de p-values. ",
      "N'invente rien qui ne soit pas dans les données."
    )
    
    res <- withProgress(
      message = "Le conseiller rédige sa réponse (une à deux minutes possibles)...",
      value = 0.5,
      
      tryCatch(
        appelle_nailer(
          donnees_llm(cible),
          intro,
          demande,
          input$c_model,
          generate = !apercu
        ),
        error = function(e) e
      )
    )
    
    if (inherits(res, "error"))
      return(
        list(
          erreur = conditionMessage(res)
        )
      )
    
    list(
      texte = texte_nailer(
        res,
        if (apercu)
          "prompt"
        else
          "response"
      ),
      apercu = apercu,
      cible = cible
    )
  })
  
  output$c_reponse <- renderUI({
    
    r <- reponse()
    
    if (!is.null(r$erreur))
      return(
        div(
          class = "alert alert-danger",
          strong("Le conseiller n'a pas pu répondre : "),
          r$erreur,
          br(),
          "Vérifiez qu'Ollama est lancé et que le modèle est téléchargé (ollama pull <modèle>)."
        )
      )
    
    if (r$apercu)
      return(
        tagList(
          h5("Prompt envoyé au LLM (aucun appel effectué)"),
          tags$pre(r$texte)
        )
      )
    
    tagList(
      h5(
        paste(
          "L'avis du conseiller sur :",
          r$cible
        )
      ),
      div(
        class = "avis-conseiller",
        r$texte
      )
    )
  })
}

shinyApp(ui, server)