library(shiny)
library(colourpicker)

cafe <- read.csv2("df_arabica_clean.csv", header = TRUE)
vars_num <- names(cafe)[sapply(cafe, is.numeric)]