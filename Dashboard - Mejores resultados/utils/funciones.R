#Utilidades para trabajar con los conjuntos de datos del dashboard

filtrarPorTemporada <-  function(df, temporada) {
  df[year(df$date) == temporada, ]
}

filtrarPorCategoria <- function(df, categ) {
  if (categ == "Todas")
    return(df)
  gender <- ifelse(categ == "Masculina", "men", "women")
  df[df$gender == gender, ]
}

filtrarPorPrueba <- function(df, prueba) {
  df[df$event_name == prueba, ]
}

mejorMarca <- function(df) {
  df_temp <- slice_max(df, df$results_score, n = 1, with_ties = FALSE)
  df_temp$mark
}

mejorAtleta <- function(df) {
  df_temp <- slice_max(df, df$results_score, n = 1, with_ties = FALSE)
  df_temp$competitor
}

mejoresPaises <- function(df) {
  unique(slice_max(df, df$results_score, with_ties = TRUE)$nat)
}

generarTextoPaisSelec <- function(df, id) {
  index_fila_selec <- which(df$ID == id)
  pruebas <- df$pruebas[[index_fila_selec]]
  
  if (length(pruebas) == 0)
    return("El país no es el mejor en ninguna prueba")
  
  pruebas <- lapply(pruebas, function(pr) {
    prueba_df_a_display[[pr]]
  })
  plural <- ifelse(length(pruebas) > 1, "s", "")
  sprintf("El país es el mejor en la%s prueba%s: %s", plural, plural,
          paste(pruebas, collapse = ", "))
}
