# Caf-_viz

Projet de visualisation et d'exploration de données réalisé dans le cadre du M2 sur la qualité du café.

L'application **« Choisir son café »** permet d'explorer les caractéristiques de lots de café issus des données du **Coffee Quality Institute (CQI)** et d'obtenir des repères simples pour comparer les profils sensoriels des cafés selon leur pays d'origine.

## Jeu de données

Les données utilisées proviennent du jeu **Coffee Quality Data (CQI)**, publié sur Kaggle par `fatihb` :

[Coffee Quality Data – Kaggle](https://www.kaggle.com/datasets/fatihb/coffee-quality-data-cqi?utm_source=chatgpt.com)

Le jeu contient notamment des informations sur :

| **Type**         | **Exemples**                                                 |
| ---------------- | ------------------------------------------------------------ |
| Géographie       | Country of Origin, Region                                    |
| Production       | Variety, Processing Method, Altitude                         |
| Exploitation     | Farm Name, Producer, Company                                 |
| Récolte          | Harvest Year, Grading Date                                   |
| Quantité         | Number of Bags, Bag Weight                                   |
| Qualité physique | Moisture, Defects, Quakers                                   |
| Sensoriel        | Aroma, Flavor, Aftertaste, Acidity, Body, Balance, Sweetness |
| Score final      | Total Cup Points                                             |

Le **Total Cup Points** correspond au score total attribué au lot à partir des différentes évaluations sensorielles.

## Nettoyage des données

Les fichiers CSV présents dans le dossier `data/` sont automatiquement chargés par l'application.

Avant leur utilisation, plusieurs variables sont nettoyées et harmonisées :

* normalisation des noms de colonnes ;
* identification de l'espèce (`Arabica` ou `Robusta`) à partir du nom du fichier ;
* conversion des variables numériques ;
* conversion des altitudes en mètres, y compris lorsque celles-ci sont exprimées en pieds ;
* traitement des altitudes renseignées sous forme d'intervalles ;
* extraction de l'année de récolte ;
* regroupement des méthodes de production en catégories ;
* harmonisation des noms de pays pour permettre leur association avec la carte ;
* suppression des lignes sans pays d'origine ;
* traitement des valeurs aberrantes du score total.

Les variables sensorielles utilisées par l'application sont :

* Arôme
* Saveur
* Longueur en bouche
* Acidité
* Corps
* Équilibre
* Douceur (sucré)

L'amertume n'est pas disponible dans les données CQI utilisées. L'application utilise donc **l'arôme comme indicateur à la place de l'amertume** dans les repères destinés au grand public.

## Fonctionnalités de l'application

### 1. Accueil

L'onglet d'accueil présente :

* le nombre de lots analysés ;
* le nombre de pays représentés ;
* la période des récoltes ;
* les espèces présentes ;
* les variables utilisées ;
* une description des méthodes de production ;
* les principales limites du jeu de données et de l'analyse.

### 2. Carte interactive

La carte permet de visualiser les données par pays d'origine.

Les filtres permettent de sélectionner :

* un ou plusieurs pays ;
* une plage d'altitude ;
* la prise en compte ou non des altitudes inconnues ;
* une plage d'années de récolte ;
* la prise en compte ou non des années inconnues ;
* une ou plusieurs méthodes de production ;
* une ou plusieurs espèces.

Les pays peuvent être colorés selon :

* le score total moyen ;
* le nombre de lots ;
* une note sensorielle moyenne.

Un clic sur un pays permet également d'afficher :

* le nombre de lots ;
* le score moyen ;
* l'altitude médiane ;
* la distribution des scores ;
* les 10 meilleurs lots du pays.

Lorsque les filtres sélectionnés ne correspondent à aucun lot, l'application affiche un message indiquant que les filtres sont trop restrictifs.

### 3. Profil sensoriel

L'onglet **Profil sensoriel** permet de comparer jusqu'à quatre pays.

Le profil moyen est représenté sous forme de radar à partir :

* de quatre critères simplifiés pour le grand public : sucré, acide, aromatique et corsé ;
* ou de l'ensemble des critères sensoriels disponibles.

L'application fournit également une interprétation textuelle simple du profil de chaque pays par rapport à l'ensemble des lots filtrés.

### 4. Choix de café

L'utilisateur peut décrire ses préférences à l'aide de curseurs :

* sucré ;
* acide ;
* aromatique ;
* corsé.

Il peut également :

* choisir une méthode de production ;
* privilégier les cafés les mieux notés ;
* décrire ses goûts librement.

L'application calcule ensuite un **classement des pays** selon leur proximité avec les préférences indiquées.

La compatibilité est un indicateur relatif : **100 correspond au pays le plus proche des préférences parmi les pays comparés**.

### 5. Conseiller IA

L'application propose également un conseiller utilisant un modèle de langage exécuté localement avec **Ollama** et le package **NaileR**.

Le conseiller peut :

1. interpréter une description libre des goûts de l'utilisateur ;
2. convertir cette description en niveaux de préférence pour les critères sensoriels ;
3. expliquer le profil d'un pays sélectionné ;
4. produire une interprétation en langage naturel des résultats statistiques.

Le modèle ne réalise pas lui-même le classement des pays : **le classement et les calculs sont réalisés dans R, puis transmis au modèle pour interprétation**.

L'utilisation du conseiller IA nécessite :

* le package `NaileR` ;
* le package `ollamar` ;
* une installation locale d'Ollama ;
* un modèle compatible, par exemple `llama3.1`.

## Structure du projet

```text
Caf-_viz/
├── app.R
├── data/
│   ├── *.csv
│   ├── NaturalEarth_Map.shp
│   ├── NaturalEarth_Map.dbf
│   ├── NaturalEarth_Map.shx
│   ├── NaturalEarth_Map.prj
│   └── NaturalEarth_Map.cpg
└── www/
    └── cafe.jpg / cafe.png / cafe.webp   (facultatif)
```

Les fichiers CSV du jeu de données doivent être placés dans le dossier `data/`.

Le fond de carte Natural Earth doit également être placé dans `data/` ou à côté de `app.R`.

## Packages R

Les packages principaux nécessaires au fonctionnement de l'application sont :

```r
install.packages(c(
  "shiny",
  "leaflet",
  "sf",
  "plotly",
  "dplyr"
))
```

Pour le conseiller IA :

```r
install.packages("ollamar")
install.packages("jsonlite")
devtools::install_github("Sebastien-Le/NaileR")
```

Ollama doit ensuite être installé et un modèle téléchargé localement, par exemple :

```bash
ollama pull llama3.1
```

Le conseiller IA est optionnel : les fonctionnalités de visualisation, de filtrage et de classement restent utilisables sans lui.

## Fond de carte

La carte utilise les données géographiques **Natural Earth à l'échelle 1:110 millions**.

Les polygones sont affichés directement dans Leaflet. Un fond OpenStreetMap peut également être activé dans l'application.

## Score et interprétation

Le **Total Cup Points** est utilisé comme indicateur global de qualité du lot.

Cependant, les données du CQI correspondent principalement à des **cafés de spécialité évalués par des dégustateurs**. Les scores sont donc relativement élevés et ne doivent pas être interprétés comme une note permettant de prédire la qualité d'un paquet de café vendu en supermarché.

L'application utilise les scores et profils sensoriels comme **repères relatifs entre pays**, et non comme une prédiction de la qualité d'un café particulier.

## Limites

Plusieurs limites doivent être prises en compte :

* les lots du CQI ne représentent pas l'ensemble des cafés disponibles sur le marché ;
* un lot de café ne correspond pas nécessairement à un paquet commercial ;
* le pays d'origine ne permet pas de représenter les différences entre régions, fermes et variétés ;
* certains pays disposent de peu de lots, ce qui rend leurs moyennes moins robustes ;
* certaines variables sont incomplètes, notamment l'altitude, l'année de récolte et la méthode de production ;
* les variables liées à la torréfaction, à la mouture, à la préparation, à la fraîcheur, à la marque et au prix ne sont pas disponibles ;
* l'amertume n'est pas mesurée dans les données utilisées ;
* les notes sensorielles restent des éva
