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

# ---- 3. PARAMÈTRES D'INTERFACE ------------------------------------------------

alt_max <- if (any(!is.na(cafe$altitude))) min(4000, ceiling(max(cafe$altitude, na.rm = TRUE) / 100) * 100) else 3000

an_range <- if (any(!is.na(cafe$annee))) range(cafe$annee, na.rm = TRUE) else c(2010, 2023)

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

modeles_ollama <- if (requireNamespace("ollamar", quietly = TRUE) && ollama_actif()) tryCatch(ollamar::list_models()$name, error = function(e) character()) else character()

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
 
"

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

photo_accueil <- list.files(
  "www",
  pattern = "^cafe\\.(jpe?g|png|webp)$",
  ignore.case = TRUE
)[1]

image_accueil <- if (!is.na(photo_accueil)) tags$img(src = photo_accueil, class = "accueil-img", alt = "Café") else HTML(SVG_TASSE)

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

pct <- function(x) paste0(round(100 * mean(x)), " %")

n_esp <- table(cafe$espece)
txt_especes <- paste(sprintf("%s (%d)", names(n_esp), as.integer(n_esp)), collapse = ", ")

txt_periode <- if (any(!is.na(cafe$annee))) sprintf("%d à %d", min(cafe$annee, na.rm = TRUE), max(cafe$annee, na.rm = TRUE)) else "non renseignée"

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

# ---- AFDM : préparation ---------------------------------------------------------
FACTOMINER_OK <- requireNamespace("FactoMineR", quietly = TRUE)
if (!FACTOMINER_OK) message("FactoMineR absent : l'onglet AFDM sera inactif (install.packages('FactoMineR')).")

# regroupe les modalités rares
regroupe <- function(x, n_min, autre) {
  x <- as.character(x); n <- table(x)
  x[x %in% names(n)[n < n_min]] <- autre
  x
}
LAB_AF <- c(ATTR_LAB, altitude = "Altitude", score = "Score total",
            pays = "Pays", methode = "Méthode", espece = "Espèce")
# couleurs vives, une par pays (le gris est réservé à « Autres pays »)
COULEURS_POP <- c("#E6194B", "#3CB44B", "#4363D8", "#F58231", "#911EB4", "#00A6C8",
                  "#F032E6", "#7CB518", "#008080", "#FF1493", "#9A6324", "#FFB000",
                  "#1F3A93", "#00C781", "#C2185B", "#6A1B9A", "#FF6F00", "#2E7D32",
                  "#D500F9", "#00B0FF", "#FF5252", "#795548", "#37474F", "#AEEA00")
