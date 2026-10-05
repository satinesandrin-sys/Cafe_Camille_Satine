fluidPage(
  
  theme = bs_theme(
    version = 5,
    bg = "#FAF6F1",
    fg = "#3B2A1A",
    primary = "#8B5A2B",
    secondary = "#C9A66B",
    success = "#6E8B3D",
    base_font = font_google("Inter"),
    heading_font = font_google("Poppins"),
    "font-size-base" = "0.95rem"
  ),
  
  tags$head(
    tags$style(HTML("
      /* ---- En-tête ---- */
      .app-header {
        background: linear-gradient(135deg, #8B5A2B 0%, #5A3A1A 100%);
        color: #FFF8F0;
        padding: 22px 30px;
        border-radius: 0 0 18px 18px;
        margin-bottom: 22px;
        box-shadow: 0 4px 14px rgba(90,58,26,0.25);
      }
      .app-header h1 {
        font-family: 'Poppins', sans-serif;
        font-weight: 700;
        margin: 0;
        font-size: 28px;
      }
      .app-header p {
        margin: 4px 0 0 0;
        opacity: 0.85;
        font-size: 14px;
      }

      /* ---- Onglets ---- */
      .nav-tabs {
        border-bottom: none;
        gap: 6px;
        margin-bottom: 18px;
      }
      .nav-tabs .nav-link {
        border: none;
        border-radius: 20px;
        padding: 8px 18px;
        color: #6b4a2d;
        font-weight: 600;
        background: #F1E4D6;
      }
      .nav-tabs .nav-link.active {
        background: #8B5A2B !important;
        color: #fff !important;
      }

      /* ---- Panneaux ---- */
      .well, .card, .sidebar-panel-card {
        border: none;
        border-radius: 16px;
        box-shadow: 0 2px 10px rgba(90,58,26,0.08);
      }
      .well { background: #FFFFFF; padding: 20px; }

      /* ---- Cartes KPI ---- */
      .value-box {
        background: linear-gradient(160deg, #FFFFFF 0%, #F6EFE9 100%);
        border: 1px solid #EFE1D2;
        border-left: 5px solid #8B5A2B;
        border-radius: 14px;
        padding: 14px 16px;
        margin-bottom: 15px;
        box-shadow: 0 2px 8px rgba(90,58,26,0.07);
        display: flex;
        align-items: center;
        gap: 12px;
      }
      .value-box .icon {
        font-size: 22px;
        color: #8B5A2B;
        background: #F1E4D6;
        border-radius: 50%;
        width: 42px; height: 42px;
        display: flex; align-items: center; justify-content: center;
        flex-shrink: 0;
      }
      .value-box .num { font-size: 24px; font-weight: 700; color: #5a3a1a; line-height: 1.1; }
      .value-box .lab { font-size: 12.5px; color: #8a7360; }

      /* ---- Boutons ---- */
      .btn-sm { border-radius: 20px; font-weight: 600; }

      /* ---- Table DT ---- */
      table.dataTable thead th { color: #5a3a1a; border-bottom: 2px solid #C9A66B !important; }
      table.dataTable tbody tr:hover { background-color: #FBF3EA !important; }
    "))
  ),
  
  div(class = "app-header",
      h1("Café Viz"),
      p("Exploration et analyse de la qualité des cafés Arabica")
  ),
  
  tabsetPanel(
    tabPanel(tagList(icon("house"), "Accueil"),
             h3("Bienvenue"),
             p("Ceci est le contenu du premier onglet.")
    ),
    
    tabPanel(tagList(icon("earth-americas"), "Explorer les cafés"),
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 class = "sidebar-panel-card",
                 selectInput("pays", "Pays :",
                             choices = c("Tous", sort(unique(na.omit(cafe$Country.of.Origin))))),
                 selectInput("variete", "Variété :",
                             choices = c("Tous", choix_variete)),
                 selectInput("methode", "Méthode de traitement :",
                             choices = c("Tous", choix_methode)),
                 sliderInput("altitude_range", "Altitude (m) :",
                             min = floor(alt_bornes[1]),
                             max = ceiling(alt_bornes[2]),
                             value = c(floor(alt_bornes[1]), ceiling(alt_bornes[2]))),
                 selectInput("annee", "Année de récolte :",
                             choices = c("Tous", choix_annee)),
                 sliderInput("f_cup", "🏆 Total Cup Points",
                             min = cup_min, max = cup_max,
                             value = c(cup_min, cup_max), step = 0.5),
                 actionButton("reset_filters", "↺ Réinitialiser les filtres",
                              class = "btn-sm btn-outline-secondary w-100"),
                 br(), br(),
                 uiOutput("map_selection_info")
               ),
               
               mainPanel(
                 width = 9,
                 
                 fluidRow(
                   column(3, div(class = "value-box",
                                 div(class = "icon", icon("mug-hot")),
                                 div(div(class = "num", textOutput("kpi_n")),
                                     div(class = "lab", "Cafés sélectionnés")))),
                   column(3, div(class = "value-box",
                                 div(class = "icon", icon("earth-americas")),
                                 div(div(class = "num", textOutput("kpi_pays")),
                                     div(class = "lab", "Pays représentés")))),
                   column(3, div(class = "value-box",
                                 div(class = "icon", icon("star")),
                                 div(div(class = "num", textOutput("kpi_score")),
                                     div(class = "lab", "Score moyen")))),
                   column(3, div(class = "value-box",
                                 div(class = "icon", icon("mountain")),
                                 div(div(class = "num", textOutput("kpi_alt")),
                                     div(class = "lab", "Altitude moyenne (m)"))))
                 ),
                 
                 leafletOutput("world_map", height = "480px"),
                 p(class = "text-muted mt-2",
                   "Taille des bulles = nombre de cafés ; couleur = score moyen. Cliquez sur un pays pour filtrer."),
                 
                 br(),
                 h4("Tableau interactif"),
                 DTOutput("coffee_table"),
                 
                 br(),
                 plotOutput("score_plot", height = "320px")
               )
             )
    ),
    tabPanel(tagList(icon("chart-column"), "Analyses"),
             tabsetPanel(
               tabPanel("Régression",
                        sidebarLayout(
                          sidebarPanel(
                            class = "sidebar-panel-card",
                            helpText("La qualité du café (Total Cup Points) s'explique-t-elle par l'altitude de culture ?"),
                            checkboxInput("reg_par_variete", "Distinguer par variété (couleur)", value = FALSE)
                          ),
                          mainPanel(
                            plotOutput("reg_plot", height = "420px"),
                            br(),
                            h4("Résumé du modèle"),
                            verbatimTextOutput("reg_summary")
                          )
                        )
               ),
               tabPanel("Classification premium",
                        p("À venir.")
               ),
               tabPanel("Random Forest",
                        p("À venir.")
               ),
               tabPanel("ACP / Clustering",
                        p("À venir.")
               )
             )
    ),
    tabPanel(tagList(icon("chart-column"), "Prédictions"),
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar-panel-card",
                 selectInput("var_predictions", "Variable :", choices = vars_num),
                 sliderInput("bins_predictions", "Nombre de classes :", min = 5, max = 50, value = 20),
                 colourInput("col_predictions", "Couleur :", value = "#8B5A2B")
               ),
               mainPanel(plotOutput("plot_predictions"))
             )
    ),
    tabPanel(tagList(icon("magnifying-glass"), "Trouve ton café"),
             sidebarLayout(
               sidebarPanel(
                 class = "sidebar-panel-card",
                 helpText("Décris en une phrase le café que tu recherches."),
                 tags$datalist(id = "suggestions_recherche",
                               lapply(exemples_recherche, function(x) tags$option(value = x))),
                 tags$input(type = "text", id = "recherche_texte", list = "suggestions_recherche",
                            class = "form-control", placeholder = "Ex : un café fruité et léger, pas trop acide...",
                            style = "margin-bottom: 12px;"),
                 actionButton("chercher_cafe", "☕ Trouver mon café", class = "btn-sm w-100")
               ),
               
               mainPanel(
                 uiOutput("resultat_recherche")
               )
             )
    )
  )
)