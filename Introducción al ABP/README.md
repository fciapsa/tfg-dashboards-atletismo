Esta aplicación se encuentra desplegada en línea [aquí](https://sites.google.com/ucm.es/tfg-flavius-abel-ciapsa/intro-abp).

Para su ejecución local, basta con abrir el fichero `introduccion_abp.html` utilizando un navegador web.

### Estructura

 - `introduccion_abp.Rmd`: Definición de la aplicación basada en `flexdashboard`. Su estructura y sintaxis es la de un documento de Markdown, con la salvedad de que la cabecera es una definición de YAML y en el cuerpo del documento se incluyen bloques de código R gracias a `rmarkdown`. Su ejecución genera un documento HTML.

 - `introduccion_abp.html`: documento HTML generado por la ejecución de `introduccion_abp.Rmd`

 - `datos`: archivos CSV del conjunto de datos de ejemplo utilizado con fines ilustrativos en la aplicación.