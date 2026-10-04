# =============================================================================
# EBAUCHE v0 - Choisir son café : carte, profil sensoriel, prédiction de qualité
# Données : Coffee Quality Data (CQI, mai 2023) - Kaggle fatihb/coffee-quality-data-cqi
# Fond de carte : Natural Earth 110m (shapefile fourni)
#
# Arborescence attendue :
#   app.R
#   data/            <- les CSV café (ex. df_arabica_clean.csv, df_robusta_clean.csv)
#   data/ne_110m_admin_0_countries.shp (+ .dbf .shx .prj .cpg)   (ou à la racine)
#
# Packages : shiny, leaflet, sf, plotly, dplyr (aucune clé API requise)
# Conseiller IA (onglet "Choix de café") : NaileR + ollamar + un serveur Ollama local
#   devtools::install_github("Sebastien-Le/NaileR")   # ou install.packages("NaileR")
#   puis installer Ollama (https://ollama.com) et télécharger un modèle : ollama pull llama3.1
#   (tout tourne en local : pas de clé API)
# STATUT : non testé (pas de R ni du CSV café à disposition à l'écriture).
#          Les messages de la console au lancement indiquent ce qui ne colle pas.
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
  # noms normalisés : "Country of Origin" -> "country_of_origin"
  names(d) <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(names(d))))
  d$espece <- if (grepl("robusta", f, ignore.case = TRUE)) "Robusta" else "Arabica"
  d
}
brut <- bind_rows(lapply(fichiers_cafe, lire))
message("Colonnes détectées : ", paste(names(brut), collapse = ", "))

# renvoie la 1re colonne trouvée parmi les candidats (sinon NA)
trouve <- function(candidats) {
  x <- intersect(candidats, names(brut))
  if (length(x)) brut[[x[1]]] else rep(NA, nrow(brut))
}
num <- function(x) suppressWarnings(as.numeric(sub(",", ".", as.character(x))))

parse_altitude <- function(x) {
  # gère "1200", "1100-1300", "1,200 m", "4000 ft"... -> mètres (moyenne)
  vapply(tolower(as.character(x)), function(s) {
    if (is.na(s)) return(NA_real_)
    n <- as.numeric(unlist(regmatches(s, gregexpr("[0-9]+\\.?[0-9]*", gsub(",", "", s)))))
    if (!length(n)) return(NA_real_)
    v <- mean(n[seq_len(min(2, length(n)))])
    if (grepl("ft|feet|pies", s)) v <- v * 0.3048
    if (v > 5000) NA_real_ else v        # valeurs aberrantes écartées
  }, numeric(1), USE.NAMES = FALSE)
}
parse_annee <- function(x) {
  x <- as.character(x); m <- regexpr("(19|20)[0-9]{2}", x)
  out <- rep(NA_integer_, length(x)); out[m > 0] <- as.integer(regmatches(x, m)); out
}
simplifie_methode <- function(x) {
  x <- tolower(trimws(as.character(x)))
  case_when(is.na(x) | x == ""        ~ "Non renseigné",
            grepl("semi", x)          ~ "Semi-lavé",
            grepl("honey|pulp", x)    ~ "Honey / pulped natural",
            grepl("wash|wet", x)      ~ "Lavé",
            grepl("natur|dry", x)     ~ "Nature (sec)",
            TRUE                      ~ "Autre")
}

# attributs sensoriels (étiquette grand public = colonne normalisée)
ATTR_CAND <- list(
  aroma = "aroma", flavor = "flavor", aftertaste = "aftertaste",
  acidity = c("acidity", "salt_acid"), body = c("body", "mouthfeel"),
  balance = "balance", sweetness = "sweetness")
ATTR_LAB <- c(aroma = "Arôme", flavor = "Saveur", aftertaste = "Longueur en bouche",
              acidity = "Acidité", body = "Corps", balance = "Équilibre",
              sweetness = "Douceur (sucré)")

cafe <- data.frame(
  pays_brut = trimws(as.character(trouve(c("country_of_origin", "country")))),
  altitude  = parse_altitude(trouve(c("altitude_mean_meters", "altitude"))),
  annee     = parse_annee(trouve(c("harvest_year", "harvest"))),
  methode   = simplifie_methode(trouve(c("processing_method", "process"))),
  variete   = as.character(trouve("variety")),
  espece    = brut$espece,
  score     = num(trouve(c("total_cup_points", "total_score"))),
  stringsAsFactors = FALSE)
for (a in names(ATTR_CAND)) {
  v <- num(trouve(ATTR_CAND[[a]])); v[!is.na(v) & v <= 0] <- NA; cafe[[a]] <- v
}
cafe$score[!is.na(cafe$score) & cafe$score < 40] <- NA   # lignes aberrantes (0 point)
cafe <- cafe[!is.na(cafe$pays_brut) & cafe$pays_brut != "", ]

attr_cols <- names(ATTR_CAND)[vapply(names(ATTR_CAND),
                                     function(a) any(!is.na(cafe[[a]])), logical(1))]
message("Attributs sensoriels retenus : ", paste(attr_cols, collapse = ", "))

# ---- 2. FOND DE CARTE + JOINTURE DES NOMS DE PAYS -----------------------------

shp <- c("data/ne_110m_admin_0_countries.shp", "ne_110m_admin_0_countries.shp")
shp <- shp[file.exists(shp)][1]
monde <- st_transform(st_read(shp, quiet = TRUE), 4326)

cle <- function(x) gsub("[^a-z]", "", tolower(iconv(x, to = "ASCII//TRANSLIT")))
recode_pays <- c("United States (Hawaii)" = "United States",
                 "Tanzania, United Republic Of" = "Tanzania",
                 "Cote d?Ivoire" = "Cote d'Ivoire")
nm <- st_drop_geometry(monde)
colonnes_noms <- intersect(c("NAME", "NAME_LONG", "ADMIN", "NAME_EN", "FORMAL_EN"), names(nm))
dico <- unique(do.call(rbind, lapply(colonnes_noms, function(cn)
  data.frame(cle = cle(nm[[cn]]), iso = nm$ISO_A3_EH, stringsAsFactors = FALSE))))
dico <- dico[!duplicated(dico$cle), ]

cafe$pays <- ifelse(cafe$pays_brut %in% names(recode_pays),
                    recode_pays[cafe$pays_brut], cafe$pays_brut)
cafe$iso <- dico$iso[match(cle(cafe$pays), dico$cle)]
non_apparies <- unique(cafe$pays_brut[is.na(cafe$iso)])
if (length(non_apparies)) {
  message("Pays SANS correspondance sur la carte (à ajouter dans recode_pays) : ",
          paste(non_apparies, collapse = " | "))
}

# ---- 3. PARAMÈTRES D'INTERFACE ------------------------------------------------

alt_max  <- if (any(!is.na(cafe$altitude))) min(4000, ceiling(max(cafe$altitude, na.rm = TRUE) / 100) * 100) else 3000
an_range <- if (any(!is.na(cafe$annee))) range(cafe$annee, na.rm = TRUE) else c(2010, 2023)
methodes <- sort(unique(cafe$methode))
pays_liste <- sort(unique(cafe$pays))
IND <- c("Score total moyen" = "score", "Nombre de lots" = "n",
         setNames(attr_cols, paste("Note moyenne :", ATTR_LAB[attr_cols])))
MOTS <- c(aroma = "très aromatique", flavor = "riche en saveurs",
          aftertaste = "longue en bouche", acidity = "vive et acidulée",
          body = "ronde et corsée", balance = "équilibrée",
          sweetness = "douce et sucrée")

# Axes "grand public" : l'amertume n'est pas mesurée par le CQI, on la remplace
# par l'arôme -> sucré / acide / aromatique (+ corps)
KEY <- intersect(c("sweetness", "acidity", "aroma", "body"), attr_cols)

phrase_gout <- function(m, ref) {
  ecart <- sort(m - ref, decreasing = TRUE)
  ecart <- ecart[is.finite(ecart) & ecart > 0]
  if (length(ecart)) paste("plutôt", paste(MOTS[names(head(ecart, 2))], collapse = ", "))
  else "dans la moyenne sur tous les critères"
}
# ---- Conseiller IA : NaileR (interprétation de résultats statistiques par un LLM) ----
NAILER_OK <- requireNamespace("NaileR", quietly = TRUE)

ollama_actif <- function() {
  isTRUE(tryCatch({
    con <- url("http://localhost:11434"); on.exit(close(con))
    readLines(con, n = 1, warn = FALSE); TRUE
  }, error = function(e) FALSE, warning = function(w) FALSE))
}
modeles_ollama <- if (requireNamespace("ollamar", quietly = TRUE) && ollama_actif())
  tryCatch(ollamar::list_models()$name, error = function(e) character()) else character()
modeles_ollama <- unique(c(modeles_ollama, "llama3.1", "mistral-small3.2"))

NIVEAUX <- c("pas du tout", "peu", "moyennement", "assez", "très")
ADJ <- c(sweetness = "sucré", acidity = "acide", aroma = "aromatique", body = "corsé",
         flavor = "savoureux", aftertaste = "long en bouche", balance = "équilibré")
NOMS_LLM <- c(aroma = "Arome", flavor = "Saveur", aftertaste = "Longueur_en_bouche",
              acidity = "Acidite", body = "Corps", balance = "Equilibre",
              sweetness = "Douceur_sucre")

# Table donnée à NaileR : notes sensorielles + classes d'altitude + méthode,
# et une variable à 2 modalités (pays choisi / autres pays) à caractériser
donnees_llm <- function(cible) {
  d <- cafe[!is.na(cafe$pays), ]
  d <- d[complete.cases(d[attr_cols]), ]
  df <- d[attr_cols]; names(df) <- unname(NOMS_LLM[attr_cols])
  alt <- cut(d$altitude, c(-Inf, 1000, 1500, Inf),
             labels = c("basse (<1000 m)", "moyenne (1000-1500 m)", "haute (>1500 m)"))
  df$Altitude <- factor(ifelse(is.na(alt), "inconnue", as.character(alt)))
  df$Methode <- factor(d$methode)
  df$Groupe <- factor(ifelse(d$pays == cible, cible, "Autres pays"),
                      levels = c(cible, "Autres pays"))
  df
}

# Appel de nail_catdes() qui s'adapte aux arguments de la version installée
# (CRAN : model / GitHub récent : provider, interpretation_mode)
appelle_nailer <- function(df, introduction, request, model, generate = TRUE) {
  f <- names(formals(NaileR::nail_catdes))
  args <- list(dataset = df, num.var = ncol(df), introduction = introduction,
               request = request, model = model)
  if ("generate" %in% f) args$generate <- generate
  if ("provider" %in% f) args$provider <- "ollama"
  if ("isolate.groups" %in% f) args$isolate.groups <- FALSE
  if ("interpretation_mode" %in% f) args$interpretation_mode <- "standard"
  do.call(NaileR::nail_catdes, args)
}
# Récupère la réponse du LLM ou le prompt envoyé
texte_nailer <- function(res, type = c("response", "prompt")) {
  type <- match.arg(type)
  ns <- asNamespace("NaileR"); fn <- paste0("nail_", type)
  if (exists(fn, envir = ns, inherits = FALSE)) {
    out <- tryCatch(get(fn, envir = ns)(res), error = function(e) NULL)
    if (!is.null(out)) return(paste(unlist(out), collapse = "\n\n"))
  }
  if (is.list(res) && !is.null(res[[type]])) return(paste(unlist(res[[type]]), collapse = "\n\n"))
  paste(capture.output(print(res)), collapse = "\n")
}

# ---- 4. INTERFACE -------------------------------------------------------------

ui <- fluidPage(
  tags$head(tags$style(HTML(".leaflet-container { background: #a6cee3; }"))),
  titlePanel("Choisir son café - ébauche v0"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      conditionalPanel(
        "input.onglet == 'carte' || input.onglet == 'sens'",
        h4("Filtres"),
        selectizeInput("f_pays", "Pays (vide = tous)", pays_liste, multiple = TRUE,
                       options = list(placeholder = "Tous les pays")),
        sliderInput("f_alt", "Altitude (m)", 0, alt_max, c(0, alt_max), step = 100),
        checkboxInput("f_alt_na", "Inclure altitude inconnue", TRUE),
        sliderInput("f_an", "Année de récolte", an_range[1], an_range[2],
                    an_range, step = 1, sep = ""),
        checkboxInput("f_an_na", "Inclure année inconnue", TRUE),
        checkboxGroupInput("f_meth", "Méthode de production", methodes, methodes),
        checkboxGroupInput("f_esp", "Espèce", unique(cafe$espece), unique(cafe$espece)),
        textOutput("n_lots")
      ),
      conditionalPanel(
        "input.onglet == 'choix'",
        h4("Mes goûts"),
        lapply(KEY, function(a)
          sliderInput(paste0("c_", a), ATTR_LAB[[a]], 1, 5, 3, step = 1, ticks = FALSE)),
        helpText("1 = pas du tout, 5 = beaucoup"),
        selectInput("c_meth", "Méthode de production (facultatif)",
                    c("Peu importe" = "", setdiff(methodes, "Non renseigné"))),
        checkboxInput("c_qual", "Privilégier les cafés les mieux notés", FALSE)
      )
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        id = "onglet",
        tabPanel("Carte", value = "carte",
                 br(),
                 fluidRow(column(8, selectInput("indicateur", "Colorer les pays selon", IND)),
                          column(4, checkboxInput("fond_osm", "Afficher le fond OpenStreetMap", FALSE))),
                 leafletOutput("carte", height = 480),
                 br(),
                 uiOutput("detail_pays")),
        tabPanel("Profil sensoriel", value = "sens",
                 br(),
                 selectizeInput("s_pays", "Comparer jusqu'à 4 pays", pays_liste,
                                multiple = TRUE, options = list(maxItems = 4)),
                 checkboxInput("s_tous", "Afficher tous les critères (sinon : sucré, acide, arôme, corps)", FALSE),
                 fluidRow(column(7, plotlyOutput("radar", height = 420)),
                          column(5, h4("En quelques mots"), uiOutput("mots"))),
                 helpText("Le profil est calculé sur les lots filtrés à gauche.",
                          "L'amertume n'est pas mesurée dans les données CQI : on regarde l'arôme à la place.",
                          "Les notes vont de 0 à 10 ; l'axe du radar est zoomé",
                          "pour faire ressortir les différences.")),
        tabPanel("Choix de café (conseiller IA)", value = "choix",
                 br(),
                 h4("Les pays qui correspondent à vos goûts"),
                 tableOutput("c_classement"),
                 helpText("Compatibilité relative : 100 = pays le plus proche de vos goûts parmi",
                          "ceux qui ont assez de lots, 0 = le plus éloigné."),
                 hr(),
                 h4("Demandez conseil à l'IA"),
                 fluidRow(
                   column(4, selectInput("c_pays", "Pays à expliquer", NULL)),
                   column(4, selectizeInput("c_model", "Modèle Ollama", modeles_ollama,
                                            selected = modeles_ollama[1],
                                            options = list(create = TRUE))),
                   column(4, br(), actionButton("c_go", "Demander au conseiller",
                                                class = "btn-primary"))),
                 checkboxInput("c_apercu", "Afficher seulement le prompt (sans appeler le LLM)", FALSE),
                 uiOutput("c_profil"),
                 uiOutput("c_reponse"),
                 helpText(textOutput("c_statut", inline = TRUE)),
                 helpText("Le LLM ne reçoit que des résultats statistiques calculés dans R",
                          "(NaileR) ; son texte est une interprétation, pas un résultat.",
                          "Les lots CQI sont surtout des cafés de spécialité : les conseils",
                          "valent comme repères relatifs entre pays."))
      )
    )
  )
)

# ---- 5. SERVEUR ---------------------------------------------------------------

server <- function(input, output, session) {

  # -- données filtrées (onglets 1 et 2)
  filtre <- reactive({
    d <- cafe
    if (length(input$f_pays)) d <- d[d$pays %in% input$f_pays, ]
    ok <- !is.na(d$altitude) & d$altitude >= input$f_alt[1] & d$altitude <= input$f_alt[2]
    d <- d[ok | (is.na(d$altitude) & input$f_alt_na), ]
    ok <- !is.na(d$annee) & d$annee >= input$f_an[1] & d$annee <= input$f_an[2]
    d <- d[ok | (is.na(d$annee) & input$f_an_na), ]
    d[d$methode %in% input$f_meth & d$espece %in% input$f_esp, ]
  })
  output$n_lots <- renderText(sprintf("%d lots correspondent aux filtres.", nrow(filtre())))

  # -- agrégation par pays
  stats_pays <- reactive({
    d <- filtre(); d <- d[!is.na(d$iso), ]
    if (!nrow(d)) return(NULL)
    do.call(rbind, lapply(split(d, d$iso), function(x)
      data.frame(iso = x$iso[1], pays = x$pays[1], n = nrow(x),
                 score = mean(x$score, na.rm = TRUE),
                 as.list(colMeans(x[attr_cols], na.rm = TRUE)))))
  })

  # -- ONGLET 1 : carte
  output$carte <- renderLeaflet({
    # aucune tuile : la carte est tracée par les polygones Natural Earth (pas de clé API)
    leaflet(options = leafletOptions(minZoom = 2)) |>
      setView(lng = 10, lat = 15, zoom = 2)
  })

  # fond OpenStreetMap facultatif (addTiles() : gratuit, sans clé)
  observeEvent(input$fond_osm, {
    proxy <- leafletProxy("carte") |> clearTiles()
    if (isTRUE(input$fond_osm)) proxy |> addTiles()
  })

  observe({
    st <- stats_pays()
    proxy <- leafletProxy("carte") |> clearShapes() |> clearControls()
    if (is.null(st)) return()
    v <- st[[input$indicateur]][match(monde$ISO_A3_EH, st$iso)]
    rng <- range(v, na.rm = TRUE)
    if (!is.finite(rng[1])) return()
    if (diff(rng) == 0) rng <- rng + c(-0.5, 0.5)
    pal <- colorNumeric("YlOrBr", rng, na.color = "#ececec")
    i <- match(monde$ISO_A3_EH, st$iso)
    etiq <- ifelse(is.na(i), sprintf("<b>%s</b><br>pas de lot", monde$NAME),
                   sprintf("<b>%s</b><br>%d lots<br>Score moyen : %.1f",
                           st$pays[i], st$n[i], st$score[i]))
    nom_ind <- names(IND)[IND == input$indicateur]
    proxy |>
      addPolygons(data = monde, layerId = ~ISO_A3_EH, fillColor = pal(v),
                  fillOpacity = 0.85, color = "white", weight = 0.7,
                  label = lapply(etiq, HTML),
                  highlightOptions = highlightOptions(weight = 2, color = "#333")) |>
      addLegend("bottomright", pal = pal, values = v, title = nom_ind, opacity = 0.9)
  })

  pays_clic <- reactiveVal(NULL)
  observeEvent(input$carte_shape_click, pays_clic(input$carte_shape_click$id))

  output$detail_pays <- renderUI({
    iso <- pays_clic()
    if (is.null(iso)) return(helpText("Cliquez sur un pays pour voir le détail de ses cafés."))
    d <- filtre(); d <- d[!is.na(d$iso) & d$iso == iso, ]
    if (!nrow(d)) return(helpText("Aucun lot pour ce pays avec les filtres actuels."))
    tagList(
      h4(d$pays[1]),
      p(sprintf("%d lots - score moyen %.1f - altitude médiane %s m",
                nrow(d), mean(d$score, na.rm = TRUE),
                ifelse(all(is.na(d$altitude)), "n.d.", round(median(d$altitude, na.rm = TRUE))))),
      fluidRow(column(6, plotlyOutput("hist_pays", height = 260)),
               column(6, h5("Les 10 meilleurs lots"), tableOutput("top_pays"))))
  })
  d_pays <- reactive({
    d <- filtre(); d[!is.na(d$iso) & d$iso == pays_clic(), ]
  })
  output$hist_pays <- renderPlotly({
    plot_ly(d_pays(), x = ~score, type = "histogram", marker = list(color = "#8c5a2b")) |>
      layout(xaxis = list(title = "Score total"), yaxis = list(title = "Lots")) |>
      config(displayModeBar = FALSE)
  })
  output$top_pays <- renderTable({
    d <- head(d_pays()[order(-d_pays()$score), ], 10)
    data.frame(Score = round(d$score, 1), Méthode = d$methode,
               `Altitude (m)` = round(d$altitude), Année = d$annee,
               Variété = d$variete, check.names = FALSE)
  }, na = "n.d.", striped = TRUE, spacing = "xs")

  # -- ONGLET 2 : profil sensoriel
  output$radar <- renderPlotly({
    req(length(input$s_pays) > 0)
    d <- filtre(); cols <- if (input$s_tous) attr_cols else KEY
    labs <- unname(ATTR_LAB[cols])
    moy <- lapply(input$s_pays, function(p) colMeans(d[d$pays == p, cols, drop = FALSE], na.rm = TRUE))
    toutes <- unlist(moy); toutes <- toutes[is.finite(toutes)]
    req(length(toutes) > 0)
    p <- plot_ly(type = "scatterpolar", mode = "lines+markers", fill = "toself")
    for (k in seq_along(moy)) {
      m <- moy[[k]]
      p <- add_trace(p, r = c(m, m[1]), theta = c(labs, labs[1]), name = input$s_pays[k])
    }
    layout(p, polar = list(radialaxis = list(range = c(floor(min(toutes) * 2) / 2 - 0.5,
                                                       ceiling(max(toutes) * 2) / 2 + 0.25))),
           legend = list(orientation = "h"))
  })
  output$mots <- renderUI({
    req(length(input$s_pays) > 0)
    d <- filtre(); cols <- if (input$s_tous) attr_cols else KEY
    ref <- colMeans(d[cols], na.rm = TRUE)
    tagList(lapply(input$s_pays, function(pp) {
      m <- colMeans(d[d$pays == pp, cols, drop = FALSE], na.rm = TRUE)
      tags$p(strong(pp), ":", phrase_gout(m, ref), "(par rapport à l'ensemble des lots filtrés)")
    }))
  })

  # -- ONGLET 3 : choix de café (classement + conseiller IA via NaileR)
  classement <- reactive({
    for (a in KEY) req(input[[paste0("c_", a)]])
    req(!is.null(input$c_meth))
    avec_meth <- nzchar(input$c_meth)
    d <- cafe[!is.na(cafe$pays) & !is.na(cafe$score), ]
    if (avec_meth) d <- d[d$methode == input$c_meth, ]
    n <- table(d$pays); d <- d[d$pays %in% names(n)[n >= (if (avec_meth) 5 else 10)], ]
    validate(need(nrow(d) > 0, "Pas assez de lots avec ces critères."))
    qual <- isTRUE(input$c_qual)
    dims <- c(KEY, if (qual) "score")
    moy <- aggregate(d[c(KEY, "score")], by = list(pays = d$pays), FUN = mean, na.rm = TRUE)
    nb <- as.integer(table(d$pays)[moy$pays])
    Z <- scale(moy[dims]); Z[is.na(Z)] <- 0
    # niveau 1..5 -> cible en écarts-types entre pays (3 = moyenne)
    cible <- c(vapply(KEY, function(a) (input[[paste0("c_", a)]] - 3) * 0.75, numeric(1)),
               if (qual) 1.5)
    dist <- sqrt(rowSums(sweep(Z, 2, cible)^2))
    out <- data.frame(pays = moy$pays, compat = round(100 * (1 - dist / max(dist))),
                      n = nb, score = moy$score)
    out[order(-out$compat), ]
  })

  output$c_classement <- renderTable({
    cl <- head(classement(), 5)
    data.frame(Pays = cl$pays, `Compatibilité (%)` = cl$compat, Lots = cl$n,
               `Score CQI moyen` = round(cl$score, 1), check.names = FALSE)
  }, striped = TRUE, spacing = "xs")

  observe({
    cl <- tryCatch(classement(), error = function(e) NULL)
    if (!is.null(cl)) updateSelectInput(session, "c_pays",
                                        choices = head(cl$pays, 5), selected = cl$pays[1])
  })

  output$c_statut <- renderText({
    invalidateLater(15000)
    paste0("NaileR : ", if (NAILER_OK) "installé" else "non installé",
           " | Ollama : ", if (ollama_actif()) "détecté" else "non détecté (lancez l'application Ollama)")
  })

  # preuve statistique, indépendante du LLM
  output$c_profil <- renderUI({
    req(input$c_pays)
    m <- colMeans(cafe[cafe$pays == input$c_pays, KEY], na.rm = TRUE)
    ref <- colMeans(cafe[KEY], na.rm = TRUE)
    tags$p(strong(input$c_pays), ":", phrase_gout(m, ref),
           "(par rapport à tous les cafés ; calcul statistique, sans IA)")
  })

  reponse <- eventReactive(input$c_go, {
    req(input$c_pays)
    if (!NAILER_OK)
      return(list(erreur = "le package NaileR n'est pas installé (devtools::install_github('Sebastien-Le/NaileR'))."))
    cible <- input$c_pays; apercu <- isTRUE(input$c_apercu)
    prefs <- paste(vapply(KEY, function(a)
      paste(NIVEAUX[input[[paste0("c_", a)]]], ADJ[[a]]), character(1)), collapse = ", ")
    intro <- paste0(
      "Contexte : application d'aide au choix de café pour un consommateur de supermarché. ",
      "Les données sont des lots de café évalués par des dégustateurs (notes de 0 à 10 par critère ; ",
      "altitude de culture en classes ; méthode de production). Le groupe « ", cible,
      " » est comparé au groupe « Autres pays ». Le consommateur recherche un café : ", prefs, ".")
    demande <- paste0(
      "À partir des seuls éléments statistiques fournis, rédige en français, pour un consommateur ",
      "non spécialiste, un conseil de 5 à 8 lignes : décris le goût typique du café du groupe « ",
      cible, " », ce qui le distingue des autres pays, et dis s'il correspond aux goûts recherchés, ",
      "en nuançant. Ne cite pas de chiffres techniques ni de p-values. N'invente rien qui ne soit pas dans les données.")
    res <- withProgress(
      message = "Le conseiller rédige sa réponse (une à deux minutes possibles)...", value = 0.5,
      tryCatch(appelle_nailer(donnees_llm(cible), intro, demande, input$c_model,
                              generate = !apercu), error = function(e) e))
    if (inherits(res, "error")) return(list(erreur = conditionMessage(res)))
    list(texte = texte_nailer(res, if (apercu) "prompt" else "response"),
         apercu = apercu, cible = cible)
  })

  output$c_reponse <- renderUI({
    r <- reponse()
    if (!is.null(r$erreur))
      return(div(class = "alert alert-danger", strong("Le conseiller n'a pas pu répondre : "),
                 r$erreur, br(),
                 "Vérifiez qu'Ollama est lancé et que le modèle est téléchargé (ollama pull <modèle>)."))
    if (r$apercu)
      return(tagList(h5("Prompt envoyé au LLM (aucun appel effectué)"), tags$pre(r$texte)))
    tagList(h5(paste("L'avis du conseiller sur :", r$cible)),
            div(style = "white-space: pre-wrap; background: #f8f4ee; padding: 12px; border-radius: 6px;",
                r$texte))
  })
}

shinyApp(ui, server)
