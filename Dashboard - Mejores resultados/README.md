Esta aplicación se encuentra desplegada en línea [aquí](https://fciapsa.shinyapps.io/mejores-resultados-atletismo/).

Para su ejecución local, se recomienda el uso de **RStudio** como IDE, ya que permite cargar el proyecto simplemente abriendo el fichero .Rproj con RStudio.

### Estructura

 - `app.R`: contiene el código fuente principal de la aplicación, es decir, la definición de la interfaz de usuario y del servidor basados en `shiny`.

 - `utils`: definiciones de funciones y estructuras de datos auxiliares necesarias para la lógica de `app.R`.

 - `datos`: contiene el script `poda_datos.R`, utilizado para transformar el conjunto de datos original en otro simplificado para su uso en la aplicación. En su subdirectorio `csvs` se incluyen tanto el conjunto de datos original como el transformado. El conjunto de datos se encuentra disponible en Kaggle [aquí](https://www.kaggle.com/datasets/jeannicolasduval/world-athletics-all-time-rankings).

 - `www`: contiene un fichero de estilo con unas mínimas definiciones de CSS para la interfaz de usuario.

 - `rsconnect`: directorio autogenerado con información relativa al despliegue en línea de la aplicación utilizando la plataforma de [shinyapps.io](https://www.shinyapps.io/).
