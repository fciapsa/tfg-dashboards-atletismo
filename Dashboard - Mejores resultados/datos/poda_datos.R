# Este script se ha utilizado para descartar los datos redundantes para mi proyecto
# que se encontraban en el conjunto de datos original obtenido de
# https://www.kaggle.com/datasets/jeannicolasduval/world-athletics-all-time-rankings

library(readr)
library(dplyr)

df_original <- read_csv("datos/csvs/world-athletics_all-time-top-lists.csv", 
                    col_types = cols(all_time_rank = col_integer(), 
                                     results_score = col_double(),
                                     event_rank = col_integer(), 
                                     date_of_birth = col_date(format = "%Y-%m-%d"), 
                                     pos = col_double(),
                                     date = col_date(format = "%Y-%m-%d"), 
                                     year_of_birth = col_integer()))

# Descartar columnas redundantes
cols_redudantes <- c("all_time_rank", "event_rank", "pos", "wind",
                     "mark_details", "year_of_birth", "event", "category",
                     "date_of_birth", "venue", "age_category")
df_podado <- select(df_original, -all_of(cols_redudantes))

# Descartar observaciones sin valor para results_score
df_podado <- df_podado[!is.na(df_podado$results_score), ]

# Algunas pruebas de marcha tienen múltiples etiquetas para la misma prueba. Las
# unificamos en una sola.
df_podado[df_podado$event_name == "5000-metres-race-walk", ]$event_name = "5-kilometres-race-walk"
df_podado[df_podado$event_name == "20000-metres-race-walk", ]$event_name = "20-kilometres-race-walk"

#Guardar el nuevo conjunto de datos
write_csv(df_podado, "datos/csvs/world-athletics_all-time-top-lists_podado.csv",
          na="", quote="needed")
