library(shiny)
library(bslib)

ANYO_DATOS_MIN <- 1981
ANYO_DATOS_MAX <- 2023

NOMBRE_CATEG_M <- "Masculina"
NOMBRE_CATEG_F <- "Femenina"

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
      tabPanel("Sprints",
        value_box(
          title="60 metros",
          value=0
        ),
        value_box(
          title="100 metros",
          value=0
        ),
        value_box(
          title="200 metros",
          value=0
        ),
        value_box(
          title="400 metros",
          value=0
        )
      ),
      tabPanel("Vallas",
        value_box(
          title="60 metros vallas",
          value=0
        ),
        conditionalPanel(
          condition = "input.categ_pr == \"Femenina\"", #No permite usar NOMBRE_CATEG_F aqui
          value_box(
            title="100 metros vallas",
            value=0
          )
        ),
        conditionalPanel(
          condition = "input.categ_pr == \"Masculina\"", # No permite usar NOMBRE_CATEG_M aqui
          value_box(
            title="110 metros vallas",
            value=0
          )
        ),
        value_box(
          title="400 metros vallas",
          value=0
        )
      ),
      tabPanel("Relevos",
        value_box(
          title="4x100 metros",
          value=0
        ),
        value_box(
          title="4x200 metros",
          value=0
        ),
        value_box(
          title="4x400 metros",
          value=0
        )
      ),
      tabPanel("Media distancia y fondo",
        value_box(
          title="800 metros",
          value=0
        ),
        value_box(
          title="1.000 metros",
          value=0
        ),
        value_box(
          title="1.500 metros",
          value=0
        ),
        value_box(
          title="1 milla",
          value=0
        ),
        value_box(
          title="3.000 metros",
          value=0
        ),
        value_box(
          title="5.000 metros",
          value=0
        ),
        value_box(
          title="10.000 metros",
          value=0
        ),
        value_box(
          title="2.000 metros obstáculos",
          value=0
        ),
        value_box(
          title="3.000 metros obstáculos",
          value=0
        )
      ),
      tabPanel("Saltos",
        value_box(
          title="Salto de longitud",
          value=0
        ),
        value_box(
          title="Triple salto",
          value=0
        ),
        value_box(
          title="Salto en altura",
          value=0
        ),
        value_box(
          title="Salto con pértiga",
          value=0
        )
      ),
      tabPanel("Lanzamientos",
        value_box(
          title="Lanzamiento de peso",
          value=0
        ),
        value_box(
          title="Lanzamiento de disco",
          value=0
        ),
        value_box(
          title="Lanzamiento de martillo",
          value=0
        ),
        value_box(
          title="Lanzamiento de jabalina",
          value=0
        )
      ),
      tabPanel("Ruta",
        value_box(
          title="1 milla",
          value=0
        ),
        value_box(
          title="5 kilómetros",
          value=0
        ),
        value_box(
          title="10 kilómetros",
          value=0
        ),
        value_box(
          title="20 kilómetros",
          value=0
        ),
        value_box(
          title="Media maratón",
          value=0
        ),
        value_box(
          title="Maratón",
          value=0
        )
      ),
      tabPanel("Marcha",
        value_box(
          title="5 kilómetros marcha",
          value=0
        ),
        value_box(
          title="20 kilómetros marcha",
          value=0
        ),
        value_box(
          title="35 kilómetros marcha",
          value=0
        ),
        value_box(
          title="50 kilómetros marcha",
          value=0
        )
      ),
      tabPanel("Eventos combinados",
        conditionalPanel(
          condition = "input.categ_pr == \"Femenina\"", #No permite usar NOMBRE_CATEG_F aqui
          value_box(
            title="Pentatlón",
            value=0
          )
        ),  
        value_box(
          title="Heptatlón",
          value=0
        ),
        conditionalPanel(
          condition = "input.categ_pr == \"Masculina\"", #No permite usar NOMBRE_CATEG_M aqui
          value_box(
            title="Decatlón",
            value=0
          )
        ),
      )
    )
  ),
  tabPanel("Mejores naciones")
)

server <- function(input, output, session) {
  
}

shinyApp(ui, server)
