##Préparation des données 

#Extraire les nombres

analyse_plage_numerique <- function(x, max_value = Inf) {
  x <- gsub(",", "", as.character(x))          # étape 1
  nums <- stringr::str_extract_all(x, "[0-9]+\\.?[0-9]*")   # étape 2 : crée "nums"
  vapply(nums, function(v) {                   # étape 3 : utilise "nums"
    v <- as.numeric(v)
    v <- v[!is.na(v)]
    if (length(v) == 0) return(NA_real_)
    val <- mean(v)
    if (val <= 0 || val > max_value) return(NA_real_) else return(val)
  }, numeric(1), USE.NAMES = FALSE)
}

label_propre <- function(v){
  
}