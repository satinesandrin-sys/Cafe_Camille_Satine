library(shiny)
library(colourpicker)

cafe <- read.csv("df_arabica_clean.csv", header = TRUE, fileEncoding = "UTF-8")
vars_num <- names(cafe)[sapply(cafe, is.numeric)]