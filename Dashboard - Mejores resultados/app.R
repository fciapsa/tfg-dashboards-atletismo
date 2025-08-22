library(shiny)
library(leaflet)
library(maps)
library(sf) #transformar de map a sf
library(bslib)
library(readr)
library(lubridate) #year()
library(tidyverse) #slice_max()
library(hash) #para definir un diccionario
library(shinydashboard) #box()

source("utils/diccionarios.R")
source("utils/funciones.R")

#Constantes
ANYO_DATOS_MIN <- 1981
ANYO_DATOS_MAX <- 2023

NOMBRE_CATEG_M <- "Masculina"
NOMBRE_CATEG_F <- "Femenina"
NOMBRE_CATEG_TODAS <- "Todas"

TEXTO_INICIAL_PANEL_SELEC <- "Haz click en un país para ver en qué pruebas es el mejor"
EXPLICACION_TAB_PAISES <- sprintf("El país recibe un punto por cada prueba en la que un atleta de
su nacionalidad obtuvo la mejor marca de la temporada. Si la categoría es \"%s\", solo se
considera al mejor de todas las categorías.", NOMBRE_CATEG_TODAS)

#Conjunto de datos
df_todo <- read_csv("datos/csvs/world-athletics_all-time-top-lists_podado.csv", 
                    col_types = cols(results_score = col_integer(),
                                     date = col_date(format = "%Y-%m-%d"), 
                                     age = col_double()))

#MAPA BASE PARA LEAFLET
#Obtener mapa mundial del paquete maps
mapa_base <- maps::map('world', plot = FALSE, fill = TRUE)
#Transformar al formato esperado por leaflet
mapa_base <- st_as_sf(mapa_base)
#Cambiar el CRS para evitar warning por utilizar un elipsoide antiguo
mapa_base <- st_transform(mapa_base, crs = 4326)
#Transformar los ids (nombres de países en inglés) a nombres en castellano
mapa_base$ID <- lapply(mapa_base$ID, function(id) {
  country_a_pais[[id]]
})
#Sincronizar rownames con ID
rownames(mapa_base) <- mapa_base$ID
#Columnas adicionales para crear una clasificación (ranking)
mapa_base$puntos <- rep(0, nrow(mapa_base))
mapa_base$pruebas <- list(rep(c(), nrow(mapa_base)))

#CONFIGURACIÓN SHINY

ui <- navbarPage(
  "Mejores resultados",
  theme = bs_theme(),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
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
        layout_column_wrap(width = 1/3,
          value_box(
            title="60 metros",
            value=htmlOutput("m60")
          ),
          value_box(
            title="100 metros",
            value=htmlOutput("m100")
          ),
          value_box(
            title="200 metros",
            value=htmlOutput("m200")
          ),
          value_box(
            title="400 metros",
            value=htmlOutput("m400")
          )
        )
      ),
      tabPanel("Vallas",
        layout_column_wrap(width = 1/3,
          value_box(
            title="60 metros vallas",
            value=htmlOutput("m60h")
          ),
          conditionalPanel(
            condition = "input.categ_pr == \"Femenina\"", #No permite usar NOMBRE_CATEG_F aqui
            value_box(
              title="100 metros vallas",
              value=htmlOutput("m100h")
            )
          ),
          conditionalPanel(
            condition = "input.categ_pr == \"Masculina\"", # No permite usar NOMBRE_CATEG_M aqui
            value_box(
              title="110 metros vallas",
              value=htmlOutput("m110h")
            )
          ),
          value_box(
            title="400 metros vallas",
            value=htmlOutput("m400h")
          )
        )
      ),
      tabPanel("Relevos",
        layout_column_wrap(width = 1/3,
          value_box(
            title="4x100 metros",
            value=htmlOutput("m4x100")
          ),
          value_box(
            title="4x200 metros",
            value=htmlOutput("m4x200")
          ),
          value_box(
            title="4x400 metros",
            value=htmlOutput("m4x400")
          )
        )
      ),
      tabPanel("Media distancia y fondo",
        layout_column_wrap(width = 1/5,
          value_box(
            title="800 metros",
            value=htmlOutput("m800")
          ),
          value_box(
            title="1.000 metros",
            value=htmlOutput("m1000")
          ),
          value_box(
            title="1.500 metros",
            value=htmlOutput("m1500")
          ),
          value_box(
            title="1 milla",
            value=htmlOutput("m1609")
          ),
          value_box(
            title="3.000 metros",
            value=htmlOutput("m3000")
          ),
          value_box(
            title="5.000 metros",
            value=htmlOutput("m5000")
          ),
          value_box(
            title="10.000 metros",
            value=htmlOutput("m10000")
          ),
          value_box(
            title="2.000 metros obstáculos",
            value=htmlOutput("m2000s")
          ),
          value_box(
            title="3.000 metros obstáculos",
            value=htmlOutput("m3000s")
          )
        )
      ),
      tabPanel("Saltos",
        layout_column_wrap(width = 1/3,
          value_box(
            title="Salto de longitud",
            value=htmlOutput("longitud")
          ),
          value_box(
            title="Triple salto",
            value=htmlOutput("triple")
          ),
          value_box(
            title="Salto en altura",
            value=htmlOutput("altura")
          ),
          value_box(
            title="Salto con pértiga",
            value=htmlOutput("pertiga")
          )
        )
      ),
      tabPanel("Lanzamientos",
        layout_column_wrap(width = 1/3,
          value_box(
            title="Lanzamiento de peso",
            value=htmlOutput("peso")
          ),
          value_box(
            title="Lanzamiento de disco",
            value=htmlOutput("disco")
          ),
          value_box(
            title="Lanzamiento de martillo",
            value=htmlOutput("martillo")
          ),
          value_box(
            title="Lanzamiento de jabalina",
            value=htmlOutput("jabalina")
          )
        )
      ),
      tabPanel("Ruta",
        layout_column_wrap(width = 1/3,
          value_box(
            title="1 milla",
            value=htmlOutput("m1609r")
          ),
          value_box(
            title="5 kilómetros",
            value=htmlOutput("m5000r")
          ),
          value_box(
            title="10 kilómetros",
            value=htmlOutput("m10000r")
          ),
          value_box(
            title="20 kilómetros",
            value=htmlOutput("m20000r")
          ),
          value_box(
            title="Media maratón",
            value=htmlOutput("m21097r")
          ),
          value_box(
            title="Maratón",
            value=htmlOutput("m42195r")
          )
        )
      ),
      tabPanel("Marcha",
        layout_column_wrap(width = 1/3,
          value_box(
            title="5 kilómetros marcha",
            value=htmlOutput("m5000rw")
          ),
          value_box(
            title="20 kilómetros marcha",
            value=htmlOutput("m20000rw")
          ),
          value_box(
            title="35 kilómetros marcha",
            value=htmlOutput("m35000rw")
          ),
          value_box(
            title="50 kilómetros marcha",
            value=htmlOutput("m50000rw")
          )
        )
      ),
      tabPanel("Eventos combinados",
        layout_column_wrap(width = 1/3,
          conditionalPanel(
            condition = "input.categ_pr == \"Femenina\"", #No permite usar NOMBRE_CATEG_F aqui
            value_box(
              title="Pentatlón",
              value=htmlOutput("pentatlon")
            )
          ),  
          value_box(
            title="Heptatlón",
            value=htmlOutput("heptatlon")
          ),
          conditionalPanel(
            condition = "input.categ_pr == \"Masculina\"", #No permite usar NOMBRE_CATEG_M aqui
            value_box(
              title="Decatlón",
              value=htmlOutput("decatlon")
            )
          )
        )
      )
    )
  ),
  tabPanel(
    title = list("Mejores países",
      tooltip(bsicons::bs_icon("info-circle", title = ""), EXPLICACION_TAB_PAISES)
    ),
    leafletOutput("mapa_paises", height= "85vh"),
    absolutePanel(id = "controles", top = "30vh", left = "3vh", width = "10vw",
                  selectInput("temp_map", "Temporada",
                              choices=ANYO_DATOS_MAX:ANYO_DATOS_MIN),
                  selectInput("categ_map", "Categoría",
                              choices=c(NOMBRE_CATEG_TODAS, NOMBRE_CATEG_M, NOMBRE_CATEG_F))
    ),
    absolutePanel(id = "panel_pais_selec", top = "13vh", right = "3vh", width = "22vw",
      shinydashboard::box(width = 12, solidHeader = TRUE,
        h4(textOutput("pais_selec")),
        textOutput("texto_pais_selec")
      )
    ),
    absolutePanel(id = "panel_top_paises", bottom = "3vh", right = "3vh", width = "20vw",
      shinydashboard::box(width = 12, solidHeader = TRUE,
        h3("Top"),
        tableOutput("top_paises")
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
  
  #Definción común a todos los htmlOutputs
  lapply(keys(output_a_prueba), function(id) {
    output[[id]] <- renderUI({
      df_temp <- filtrarPorPrueba(df_pr(), output_a_prueba[[id]])
      if(nrow(df_temp) == 0)
        return(HTML("Sin datos"))
      HTML(paste(mejorMarca(df_temp), h6(mejorAtleta(df_temp)), sep = '<br/>'))
    })
  })
  
  #PESTAÑA MEJORES PAÍSES
  
  #Conjunto de datos tras aplicar los inputs
  df_map <- reactive({
    df_temp <- filtrarPorTemporada(df_todo, input$temp_map)
    filtrarPorCategoria(df_temp, input$categ_map)
  })
  
  #Mapa ampliado con la puntuación de cada país y las pruebas que puntúan
  datos_mapa <- reactive({
    df_temp <- mapa_base
    
    for (prueba in keys(prueba_df_a_display)) {
      df_prueba <- filtrarPorPrueba(df_map(), prueba)
      if(nrow(df_prueba) == 0) next

      paises_que_puntuan <- mejoresPaises(df_prueba)
      paises_que_puntuan <- transformarAbrevsEnNombres(paises_que_puntuan)
      #Actualizar los datos de los países que puntúan
      indices_por_actualizar <- which(df_temp$ID %in% paises_que_puntuan)
      for (i in indices_por_actualizar) {
        df_temp$puntos[i] <- df_temp$puntos[i] + 1
        df_temp$pruebas[[i]] <- append(df_temp$pruebas[[i]], prueba)
      }
    }

    df_temp
  })
  
  output$top_paises <- renderTable({
    df <- st_drop_geometry(datos_mapa())
    df <- subset(df, select = c("ID", "puntos"))
    df <- slice_max(df, df$puntos, n = 5, with_ties = FALSE)
    names(df) <- c("País", "# mejores resultados")
    df
  }, digits = 0)
  
  #Texto inicial para el panel del país seleccionado
  observeEvent({
    input$temp_map
    input$categ_map
  },{
    output$pais_selec <- renderText(TEXTO_INICIAL_PANEL_SELEC)
    output$texto_pais_selec <- renderText("")
  })
  
  #Observer que gestiona el click sobre la forma de un país
  observeEvent(input$mapa_paises_shape_click, {
    id_click <- unlist(input$mapa_paises_shape_click["id"])
    output$pais_selec <- renderText(id_click)
    
    output$texto_pais_selec <- renderText(generarTextoPaisSelec(datos_mapa(), id_click))
  })
  
  output$mapa_paises <- renderLeaflet({
    #Etiquetas que apareceren cuando el usuario coloca el ratón por encima de un país
    labels <- sprintf(
      "<strong>%s</strong><br/>Mejor país en %d prueba%s",
      datos_mapa()$ID, datos_mapa()$puntos, ifelse(datos_mapa()$puntos == 1, "", "s")
    ) %>% lapply(htmltools::HTML)
    
    #Configurar la paleta. Hacemos depender la cantidad de colores del máximo de puntos
    cantidad_colores <- max(datos_mapa()$puntos)
    if (cantidad_colores >= 10)
      cantidad_colores <- c(0,1,2,4,6,10,15,50) #límites de paso al siguiente color
    paleta <- colorBin("OrRd", domain = datos_mapa()$puntos, bins = cantidad_colores)
    
    leaflet(datos_mapa(), options = leafletOptions(worldCopyJump = TRUE)) %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = c(maxZoom = 6)) %>%
      addPolygons(
        layerId = ~ID, #para que el evento shape_click contenga estos ids
        fillColor = ~paleta(puntos),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")
      ) %>%
      #Vista inicial del mapa
      setView(lng = 40, lat = 15, zoom = 2) %>%
      addLegend(pal = paleta, values = ~puntos, opacity = 0.7, title = NULL,
                position = "bottomleft"
      )
  })
}

shinyApp(ui, server)
