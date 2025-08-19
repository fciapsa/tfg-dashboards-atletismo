library(shiny)
library(leaflet)
library(maps)
library(bslib)
library(readr)
library(lubridate) #year()
library(tidyverse) #slice_max()
library(hash) #para definir un diccionario
library(shinydashboard) #box()

#Constantes
ANYO_DATOS_MIN <- 1981
ANYO_DATOS_MAX <- 2023

NOMBRE_CATEG_M <- "Masculina"
NOMBRE_CATEG_F <- "Femenina"
NOMBRE_CATEG_TODAS <- "Todas"

#Diccionario de ids de outputs a nombres de pruebas
output_a_prueba <- hash("m60" = "60-metres", "m100" = "100-metres",
                        "m200" = "200-metres", "m400" = "400-metres",
                        "m60h" = "60-metres-hurdles", "m100h" = "100-metres-hurdles",
                        "m110h" = "110-metres-hurdles", "m400h" = "400-metres-hurdles",
                        "m4x100" = "4x100-metres-relay", "m4x200" = "4x200-metres-relay",
                        "m4x400" = "4x400-metres-relay",
                        "m800" = "800-metres", "m1000" = "1000-metres",
                        "m1500" = "1500-metres", "m1609" = "one-mile",
                        "m3000" = "3000-metres", "m5000" = "5000-metres",
                        "m10000" = "10000-metres", "m2000s" = "2000-metres-steeplechase",
                        "m3000s" = "3000-metres-steeplechase",
                        "longitud" = "long-jump", "triple" = "triple-jump",
                        "altura" = "high-jump", "pertiga" = "pole-vault",
                        "peso" = "shot-put", "disco" = "discus-throw",
                        "martillo" = "hammer-throw", "jabalina" = "javelin-throw",
                        "m1609r" = "1-mile-road", "m5000r" = "5-kilometres",
                        "m10000r" = "10-kilometres", "m20000r" = "20-kilometres",
                        "m21097r" = "half-marathon", "m42195r" = "marathon",
                        "m5000rw" = "5-kilometres-race-walk",
                        "m20000rw" = "20-kilometres-race-walk",
                        "m35000rw" = "35-kilometres-race-walk",
                        "m50000rw" = "50-kilometres-race-walk",
                        "pentatlon" = "pentathlon", "heptatlon" = "heptathlon",
                        "decatlon" = "decathlon")

#Conjunto de datos
df_todo <- read_csv("datos/csvs/world-athletics_all-time-top-lists_podado.csv", 
             col_types = cols(results_score = col_integer(), 
                              date_of_birth = col_date(format = "%Y-%m-%d"), 
                              date = col_date(format = "%Y-%m-%d"), 
                              age = col_double()))

#Utilidades
filtrarPorTemporada <-  function(df, temporada) {
  df[year(df$date) == temporada, ]
}

filtrarPorCategoria <- function(df, categ) {
  gender <- ifelse(categ == NOMBRE_CATEG_M, "men", "women")
  df[df$gender == gender, ]
}

filtrarPorPrueba <- function(df, prueba) {
  df[df$event_name == prueba, ]
}

mejorMarca <- function(df) {
  df_temp <- slice_max(df, df$results_score, n = 1, with_ties = FALSE)
  df_temp$mark
}

ui <- navbarPage(
  "Mejores resultados",
  tabPanel("Por pruebas",
    fluidRow(
      column(3,
        selectInput("temp_pr", "Temporada", choices=ANYO_DATOS_MAX:ANYO_DATOS_MIN)
      ),
      column(3,
        selectInput("categ_pr", "Categoría", choices=c(NOMBRE_CATEG_M, NOMBRE_CATEG_F))
      )
    ),
    tabsetPanel(
      tabPanel("Velocidad",
        value_box(
          title="60 metros",
          value=textOutput("m60")
        ),
        value_box(
          title="100 metros",
          value=textOutput("m100")
        ),
        value_box(
          title="200 metros",
          value=textOutput("m200")
        ),
        value_box(
          title="400 metros",
          value=textOutput("m400")
        )
      ),
      tabPanel("Vallas",
        value_box(
          title="60 metros vallas",
          value=textOutput("m60h")
        ),
        conditionalPanel(
          condition = "input.categ_pr == \"Femenina\"", #No permite usar NOMBRE_CATEG_F aqui
          value_box(
            title="100 metros vallas",
            value=textOutput("m100h")
          )
        ),
        conditionalPanel(
          condition = "input.categ_pr == \"Masculina\"", # No permite usar NOMBRE_CATEG_M aqui
          value_box(
            title="110 metros vallas",
            value=textOutput("m110h")
          )
        ),
        value_box(
          title="400 metros vallas",
          value=textOutput("m400h")
        )
      ),
      tabPanel("Relevos",
        value_box(
          title="4x100 metros",
          value=textOutput("m4x100")
        ),
        value_box(
          title="4x200 metros",
          value=textOutput("m4x200")
        ),
        value_box(
          title="4x400 metros",
          value=textOutput("m4x400")
        )
      ),
      tabPanel("Media distancia y fondo",
        value_box(
          title="800 metros",
          value=textOutput("m800")
        ),
        value_box(
          title="1.000 metros",
          value=textOutput("m1000")
        ),
        value_box(
          title="1.500 metros",
          value=textOutput("m1500")
        ),
        value_box(
          title="1 milla",
          value=textOutput("m1609")
        ),
        value_box(
          title="3.000 metros",
          value=textOutput("m3000")
        ),
        value_box(
          title="5.000 metros",
          value=textOutput("m5000")
        ),
        value_box(
          title="10.000 metros",
          value=textOutput("m10000")
        ),
        value_box(
          title="2.000 metros obstáculos",
          value=textOutput("m2000s")
        ),
        value_box(
          title="3.000 metros obstáculos",
          value=textOutput("m3000s")
        )
      ),
      tabPanel("Saltos",
        value_box(
          title="Salto de longitud",
          value=textOutput("longitud")
        ),
        value_box(
          title="Triple salto",
          value=textOutput("triple")
        ),
        value_box(
          title="Salto en altura",
          value=textOutput("altura")
        ),
        value_box(
          title="Salto con pértiga",
          value=textOutput("pertiga")
        )
      ),
      tabPanel("Lanzamientos",
        value_box(
          title="Lanzamiento de peso",
          value=textOutput("peso")
        ),
        value_box(
          title="Lanzamiento de disco",
          value=textOutput("disco")
        ),
        value_box(
          title="Lanzamiento de martillo",
          value=textOutput("martillo")
        ),
        value_box(
          title="Lanzamiento de jabalina",
          value=textOutput("jabalina")
        )
      ),
      tabPanel("Ruta",
        value_box(
          title="1 milla",
          value=textOutput("m1609r")
        ),
        value_box(
          title="5 kilómetros",
          value=textOutput("m5000r")
        ),
        value_box(
          title="10 kilómetros",
          value=textOutput("m10000r")
        ),
        value_box(
          title="20 kilómetros",
          value=textOutput("m20000r")
        ),
        value_box(
          title="Media maratón",
          value=textOutput("m21097r")
        ),
        value_box(
          title="Maratón",
          value=textOutput("m42195r")
        )
      ),
      tabPanel("Marcha",
        value_box(
          title="5 kilómetros marcha",
          value=textOutput("m5000rw")
        ),
        value_box(
          title="20 kilómetros marcha",
          value=textOutput("m20000rw")
        ),
        value_box(
          title="35 kilómetros marcha",
          value=textOutput("m35000rw")
        ),
        value_box(
          title="50 kilómetros marcha",
          value=textOutput("m50000rw")
        )
      ),
      tabPanel("Eventos combinados",
        conditionalPanel(
          condition = "input.categ_pr == \"Femenina\"", #No permite usar NOMBRE_CATEG_F aqui
          value_box(
            title="Pentatlón",
            value=textOutput("pentatlon")
          )
        ),  
        value_box(
          title="Heptatlón",
          value=textOutput("heptatlon")
        ),
        conditionalPanel(
          condition = "input.categ_pr == \"Masculina\"", #No permite usar NOMBRE_CATEG_M aqui
          value_box(
            title="Decatlón",
            value=textOutput("decatlon")
          )
        ),
      )
    )
  ),
  tabPanel("Mejores países",
    leafletOutput("mapa_paises", height = 500),
    absolutePanel(top = 150, left = 20,
      selectInput("temp_map", "Temporada", choices=ANYO_DATOS_MAX:ANYO_DATOS_MIN),
      selectInput("categ_map", "Categoría",
                  choices=c(NOMBRE_CATEG_TODAS, NOMBRE_CATEG_M, NOMBRE_CATEG_F))
    ),
    absolutePanel(top = 70, right = 20, width = 250,
      box(width = 12, solidHeader = TRUE,
        h3(textOutput("pais_selec"))
      )
    ),
    absolutePanel(bottom = 30, right = 20, width = 250,
      box(width = 12, solidHeader = TRUE,
        h3("Top"),
      )
    )
  )
)

server <- function(input, output, session) {
  #PESTAÑA POR PRUEBAS

  #Conjunto de datos tras aplicar los inputs
  df_pr <- reactive({
    df_temp <- filtrarPorTemporada(df_todo, input$temp_pr)
    filtrarPorCategoria(df_temp, input$categ_pr)
  })
  
  #Definción común a todos los textOutputs
  lapply(keys(output_a_prueba), function(id) {
    output[[id]] <- renderText({
      df_temp <- filtrarPorPrueba(df_pr(), output_a_prueba[[id]])
      if(nrow(df_temp) == 0) return("Sin datos")
      mejorMarca(df_temp)
    })
  })
  
  #PESTAÑA MEJORES PAÍSES
  
  output$pais_selec <- renderText("TO DO")
  
  datos_mapa <- maps::map('world', plot = FALSE, fill = TRUE)
  output$mapa_paises <- renderLeaflet({
    leaflet(datos_mapa, options = leafletOptions(worldCopyJump = TRUE)) %>%
    addProviderTiles(providers$Esri.WorldGrayCanvas,
                     options = c(maxZoom = 6)) %>%
    addPolygons() %>%
    #Vista inicial del mapa
    setView(lng = 40, lat = 15, zoom = 2)
  })
}

shinyApp(ui, server)
