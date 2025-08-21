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

#Constantes
ANYO_DATOS_MIN <- 1981
ANYO_DATOS_MAX <- 2023

NOMBRE_CATEG_M <- "Masculina"
NOMBRE_CATEG_F <- "Femenina"
NOMBRE_CATEG_TODAS <- "Todas"

TEXTO_INICIAL_PANEL_SELEC <- "Haz click en un país para ver en qué pruebas es el mejor"

#Conjunto de datos
df_todo <- read_csv("datos/csvs/world-athletics_all-time-top-lists_podado.csv", 
                    col_types = cols(results_score = col_integer(), 
                                     date_of_birth = col_date(format = "%Y-%m-%d"), 
                                     date = col_date(format = "%Y-%m-%d"), 
                                     age = col_double()))

#Diccionario de abreviaturas utilizadas por la columna nat del conjunto de datos
#a nombres de países en idioma castellano.
#Abreviaturas no incluidas porque el país ya no existe y no es posible resolver la
#ambigüedad: TCH, ANA, EUN, INT, YUG, ANT, AHO, SCG y ART.
abrev_a_pais <- hash("GDR" = "Alemania", "URS" = "Rusia", "CZE" = "Rep. Checa",
                     "JAM" = "Jamaica", "GER" = "Alemania", "USA" = "Estados Unidos",
                     "FRG" = "Alemania", "NOR" = "Noruega", "SWE" = "Suecia",
                     "BUL" = "Bulgaria", "BRA" = "Brasil", "RSA" = "Sudáfrica",
                     "CHN" = "China", "LTU" = "Lituania", "CUB" = "Cuba",
                     "UKR" = "Ucrania", "ROU" = "Rumanía", "KEN" = "Kenia",
                     "RUS" = "Rusia", "FRA" = "Francia", "EST" = "Estonia",
                     "UGA" = "Uganda", "ETH" = "Etiopía", "DEN" = "Dinamarca",
                     "GBR" = "Reino Unido", "NAM" = "Namibia", "POL" = "Polonia",
                     "MAR" = "Marruecos", "ESP" = "España", "BOT" = "Botsuana",
                     "CRO" = "Croacia", "PAN" = "Panamá", "NED" = "Países Bajos",
                     "VEN" = "Venezuela", "QAT" = "Catar", "ITA" = "Italia",
                     "NZL" = "Nueva Zelanda", "BAH" = "Bahamas", "FIN" = "Finlandia",
                     "GRN" = "Granada", "CAN" = "Canadá", "SUI" = "Suiza",
                     "AUS" = "Australia", "ALG" = "Argelia", "GRE" = "Grecia",
                     "BRN" = "Baréin", "IVB" = "Islas Vírgenes Británicas",
                     "ZAM" = "Zambia", "TAN" = "Tanzania", "SLO" = "Eslovenia",
                     "BLR" = "Bielorrusia", "BUR" = "Burkina Faso", "JPN" = "Japón",
                     "NGR" = "Nigeria", "HUN" = "Hungría", "BEL" = "Bélgica",
                     "DJI" = "Yibuti", "LCA" = "Santa Lucía", "SEN" = "Senegal",
                     "DOM" = "Rep. Dominicana", "SRB" = "Serbia",
                     "POR" = "Portugal", "TTO" = "Trinidad y Tobago",
                     "BAR" = "Barbados", "PHI" = "Filipinas", "BDI" = "Burundi",
                     "IRL" = "Irlanda", "ERI" = "Eritrea", "TPE" = "Taiwán",
                     "KAZ" = "Kazajistán",  "CIV" = "Costa de Marfil",
                     "SYR" = "Siria", "CMR" = "Camerún","SUD" = "Sudán",
                     "TUR" = "Turquía", "PER" = "Perú", "MEX" = "México",
                     "AUT" = "Austria", "ISR" = "Israel", "SVK" = "Eslovaquia",
                     "KSA" = "Arabia Saudita", "TJK" = "Tayikistán",
                     "COL" = "Colombia", "LUX" = "Luxemburgo", "LAT" = "Letonia",
                     "PUR" = "Puerto Rico", "GHA" = "Ghana", "SAM" = "Samoa",
                     "BIH" = "Bosnia y Herzegovina", "MOZ" = "Mozambique",
                     "ECU" = "Ecuador", "LBR" = "Liberia", "PAK" = "Pakistán",
                     "ZIM" = "Zimbabue", "SUR" = "Surinam", "UZB" = "Uzbekistán",
                     "IND" = "India", "GUA" = "Guatemala", "COD" = "RD del Congo",
                     "SSD" = "Sudán del Sur", "BER" = "Bermudas", "NIG" = "Níger",
                     "SOM" = "Somalia", "MRI" = "Mauricio", "CRC" = "Costa Rica",
                     "KOR" = "Corea del Sur", "IRI" = "Irán", "EGY" = "Egipto",
                     "ISL" = "Islandia", "SKN" = "San Cristóbal y Nieves",
                     "TUN" = "Túnez", "THA" = "Tailandia",
                     "ISV" = "Islas Vírgenes de los Estados Unidos",
                     "BEN" = "Benín", "CYP" = "Chipre", "CAY" = "Islas Caimán",
                     "STP" = "Santo Tomé y Príncipe", "UND" = "Alemania",
                     "SRI" = "Sri Lanka", "ALB" = "Albania", "MLI" = "Mali",
                     "OMA" = "Omán", "DMA" = "Dominica", "AZE" = "Azerbaiyán",
                     "UAE" = "Emiratos Árabes Unidos", "ARM" = "Armenia",
                     "HAI" = "Haití", "RWA" = "Ruanda", "ESA" = "El Salvador",
                     "KUW" = "Kuwait", "URU" = "Uruguay", "GUY" = "Guyana",
                     "PRK" = "Corea del Norte", "BOL" = "Bolivia",
                     "ARG" = "Argentina", "AIA" = "Anguila", "CHI" = "Chile",
                     "MDA" = "Moldavia", "INA" = "Indonesia", "GAB" = "Gabón",
                     "GEO" = "Georgia", "HON" = "Honduras",
                     "CGO" = "República del Congo", "HKG" = "China",
                     "KGZ" = "Kirguistán", "MNE" = "Montenegro", "CHA" = "Chad",
                     "GAM" = "Gambia", "SLE" = "Sierra Leona", "MAD" = "Madagascar",
                     "BIZ" = "Belice", "SWZ" = "Esuatini", "LBA" = "Libia",
                     "MGL" = "Mongolia", "VIN" = "San Vicente y las Granadinas",
                     "SGP" = "Singapur", "MAS" = "Malasia", "TKS" = "Reino Unido",
                     "MNT" = "Montserrat", "MLT" = "Malta", "NCA" = "Nicaragua",
                     "LES" = "Lesoto", "BAN" = "Bangladés", "PAR" = "Paraguay",
                     "CPV" = "Cabo Verde", "VIE" = "Vietnam", "AND" = "Andorra",
                     "LIE" = "Liechtenstein", "SMR" = "San Marino",
                     "LBN" = "Líbano", "GUI" = "Guinea", "IRQ" = "Irak",
                     "FIJ" = "Fiyi", "MAW" = "Malaui", "SEY" = "Seychelles",
                     "PYF" = "Polinesia Francesa", "COM" = "Comoras",
                     "TOG" = "Togo", "ARU" = "Aruba", "ANG" = "Angola",
                     "CAF" = "República Centroafricana", "PLE" = "Palestina",
                     "TKM" = "Turkmenistán", "TGA" = "Tonga",
                     "ASA" = "Samoa Americana"
                    )

transformarAbrevsEnNombres <- function(abrevs) {
  nombres <- list()
  for (abrev in abrevs) {
    if (abrev %in% keys(abrev_a_pais))
      nombres <- append(nombres, abrev_a_pais[[abrev]])
  }
  unique(nombres)
}

#Diccionario de nombres de países según aparecen en un mapa del paquete maps
#a nombres de países en idioma castellano.
country_a_pais <- hash("Aruba" = "Aruba", "Afghanistan" = "Afganistán",
                       "Angola" = "Angola", "Anguilla" = "Anguila",
                       "Albania" = "Albania", "Finland" = "Finlandia",
                       "Andorra" = "Andorra",
                       "United Arab Emirates" = "Emiratos Árabes Unidos",
                       "Argentina" = "Argentina", "Armenia" = "Armenia",
                       "American Samoa" = "Samoa Americana",
                       "Antarctica" = "Antártida", "Australia" = "Australia",
                       "French Southern and Antarctic Lands" = "Tierras Australes y Antárticas Francesas",
                       "Antigua" = "Antigua", "Barbuda" = "Barbuda", "Austria" = "Austria",
                       "Azerbaijan" = "Azerbaiyán", "Burundi" = "Burundi",
                       "Belgium" = "Bélgica", "Benin" = "Benín",
                       "Burkina Faso" = "Burkina Faso", "Bangladesh" = "Bangladés",
                       "Bulgaria" = "Bulgaria", "Bahrain" = "Baréin",
                       "Bahamas" = "Bahamas",
                       "Bosnia and Herzegovina" = "Bosnia y Herzegovina",
                       "Saint Barthelemy" = "San Bartolomé",
                       "Belarus" = "Bielorrusia", "Belize" = "Belice",
                       "Bermuda" = "Bermudas", "Bolivia" = "Bolivia",
                       "Brazil" = "Brasil", "Barbados" = "Barbados",
                       "Brunei" = "Brunéi", "Bhutan" = "Bután",
                       "Botswana" = "Botsuana",
                       "Central African Republic" = "República Centroafricana",
                       "Canada" = "Canadá", "Switzerland" = "Suiza",
                       "Chile" = "Chile", "China" = "China",
                       "Ivory Coast" = "Costa de Marfil", "Cameroon" = "Camerún",
                       "Democratic Republic of the Congo" = "RD del Congo",
                       "Republic of Congo" = "República del Congo",
                       "Cook Islands" = "Islas Cook", "Colombia" = "Colombia",
                       "Comoros" = "Comoras", "Cape Verde" = "Cabo Verde",
                       "Costa Rica" = "Costa Rica", "Cuba" = "Cuba",
                       "Curacao" = "Curazao", "Cayman Islands" = "Islas Caimán",
                       "Cyprus" = "Chipre", "Czech Republic" = "Rep. Checa",
                       "Germany" = "Alemania", "Djibouti" = "Yibuti",
                       "Dominica" = "Dominica", "Denmark" = "Dinamarca",
                       "Dominican Republic" = "Rep. Dominicana",
                       "Algeria" = "Argelia", "Ecuador" = "Ecuador",
                       "Egypt" = "Egipto", "Eritrea" = "Eritrea",
                       "Canary Islands" = "Islas Canarias", "Spain" = "España",
                       "Estonia" = "Estonia", "Ethiopia" = "Etiopía", "Fiji" = "Fiyi",
                       "Falkland Islands" = "Islas Malvinas", "Reunion" = "Reunión",
                       "Mayotte" = "Mayotte", "French Guiana" = "Guayana Francesa",
                       "Martinique" = "Martinica", "Guadeloupe" = "Guadalupe",
                       "France" = "Francia", "Faroe Islands" = "Islas Feroe",
                       "Micronesia" = "Micronesia", "Gabon" = "Gabón",
                       "UK" = "Reino Unido", "Georgia" = "Georgia",
                       "Guernsey" = "Guernsey", "Ghana" = "Ghana", "Guinea" = "Guinea",
                       "Gambia" = "Gambia", "Guinea-Bissau" = "Guinea-Bisáu",
                       "Equatorial Guinea" = "Guinea Ecuatorial", "Greece" = "Grecia",
                       "Grenada" = "Granada", "Greenland" = "Groenlandia",
                       "Guatemala" = "Guatemala", "Guam" = "Guam", "Guyana" = "Guyana",
                       "Heard Island" = "Isla Heard", "Honduras" = "Honduras",
                       "Croatia" = "Croacia", "Haiti" = "Haití", "Hungary" = "Hungría",
                       "Indonesia" = "Indonesia", "Isle of Man" = "Isla de Man",
                       "India" = "India", "Cocos Islands" = "Islas Cocos",
                       "Christmas Island" = "Isla de Navidad",
                       "Chagos Archipelago" = "Archipiélago de Chagos",
                       "Ireland" = "Irlanda", "Iran" = "Irán", "Iraq" = "Irak",
                       "Iceland" = "Islandia", "Israel" = "Israel", "Italy" = "Italia",
                       "San Marino" = "San Marino", "Jamaica" = "Jamaica",
                       "Jersey" = "Jersey", "Jordan" = "Jordania", "Japan" = "Japón",
                       "Siachen Glacier" = "Glaciar Siachen", "Kazakhstan" = "Kazajistán",
                       "Kenya" = "Kenia", "Kyrgyzstan" = "Kirguistán",
                       "Cambodia" = "Camboya", "Kiribati" = "Kiribati",
                       "Nevis" = "Nieves", "Saint Kitts" = "San Cristóbal",
                       "South Korea" = "Corea del Sur", "Kosovo" = "Kosovo",
                       "Kuwait" = "Kuwait", "Laos" = "Laos", "Lebanon" = "Líbano",
                       "Liberia" = "Liberia", "Libya" = "Libia",
                       "Saint Lucia" = "Santa Lucía", "Liechtenstein" = "Liechtenstein",
                       "Sri Lanka" = "Sri Lanka", "Lesotho" = "Lesoto",
                       "Lithuania" = "Lituania", "Luxembourg" = "Luxemburgo",
                       "Latvia" = "Letonia", "Saint Martin" = "San Martín",
                       "Morocco" = "Marruecos", "Monaco" = "Mónaco",
                       "Moldova" = "Moldavia", "Madagascar" = "Madagascar",
                       "Maldives" = "Maldivas", "Mexico" = "México",
                       "Marshall Islands" = "Islas Marshall",
                       "North Macedonia" = "Macedonia del Norte", "Mali" = "Mali",
                       "Malta" = "Malta", "Myanmar" = "Myanmar",
                       "Montenegro" = "Montenegro", "Mongolia" = "Mongolia",
                       "Northern Mariana Islands" = "Islas Marianas del Norte",
                       "Mozambique" = "Mozambique", "Mauritania" = "Mauritania",
                       "Montserrat" = "Montserrat", "Mauritius" = "Mauricio",
                       "Malawi" = "Malaui", "Malaysia" = "Malasia",
                       "Namibia" = "Namibia", "New Caledonia" = "Nueva Caledonia",
                       "Niger" = "Níger", "Norfolk Island" = "Isla Norfolk",
                       "Nigeria" = "Nigeria", "Nicaragua" = "Nicaragua",
                       "Niue" = "Niue", "Bonaire" = "Bonaire",
                       "Sint Eustatius" = "San Eustaquio", "Saba" = "Saba",
                       "Netherlands" = "Países Bajos", "Norway" = "Noruega",
                       "Nepal" = "Nepal", "Nauru" = "Nauru", "New Zealand" =
                        "Nueva Zelanda", "Oman" = "Omán", "Pakistan" = "Pakistán",
                       "Panama" = "Panamá", "Pitcairn Islands" = "Islas Pitcairn",
                       "Peru" = "Perú", "Philippines" = "Filipinas",
                       "Palau" = "Palaos", "Papua New Guinea" = "Papúa Nueva Guinea",
                       "Poland" = "Polonia", "Puerto Rico" = "Puerto Rico",
                       "North Korea" = "Corea del Norte",
                       "Madeira Islands" = "Islas Madeira", "Azores" = "Azores",
                       "Portugal" = "Portugal", "Paraguay" = "Paraguay",
                       "Palestine" = "Palestina", "French Polynesia" = "Polinesia Francesa",
                       "Qatar" = "Catar", "Romania" = "Rumanía", "Russia" = "Rusia",
                       "Rwanda" = "Ruanda", "Western Sahara" = "Sáhara Occidental",
                       "Saudi Arabia" = "Arabia Saudita", "Sudan" = "Sudán",
                       "South Sudan" = "Sudán del Sur", "Senegal" = "Senegal",
                       "Singapore" = "Singapur",
                       "South Sandwich Islands" = "Islas Sandwich del Sur",
                       "South Georgia" = "Isla Georgia del Sur",
                       "Saint Helena" = "Santa Elena", "Ascension Island" = "Isla Ascensión",
                       "Solomon Islands" = "Islas Salomón", "Sierra Leone" = "Sierra Leona",
                       "El Salvador" = "El Salvador", "Somalia" = "Somalia",
                       "Saint Pierre and Miquelon" = "San Pedro y Miquelón",
                       "Serbia" = "Serbia", "Sao Tome and Principe" = "Santo Tomé y Príncipe",
                       "Suriname" = "Surinam", "Slovakia" = "Eslovaquia",
                       "Slovenia" = "Eslovenia", "Sweden" = "Suecia",
                       "Swaziland" = "Esuatini", "Sint Maarten" = "Sint Maarten",
                       "Seychelles" = "Seychelles", "Syria" = "Siria",
                       "Turks and Caicos Islands" = "Islas Turcas y Caicos",
                       "Chad" = "Chad", "Togo" = "Togo", "Thailand" = "Tailandia",
                       "Tajikistan" = "Tayikistán", "Turkmenistan" = "Turkmenistán",
                       "Timor-Leste" = "Timor Oriental", "Tonga" = "Tonga",
                       "Trinidad" = "Trinidad", "Tobago" = "Tobago",
                       "Tunisia" = "Túnez", "Turkey" = "Turquía",
                       "Taiwan" = "Taiwán", "Tanzania" = "Tanzania",
                       "Uganda" = "Uganda", "Ukraine" = "Ucrania",
                       "Uruguay" = "Uruguay", "USA" = "Estados Unidos",
                       "Uzbekistan" = "Uzbekistán", "Vatican" = "Ciudad del Vaticano",
                       "Grenadines" = "Granadinas", "Saint Vincent" = "San Vicente",
                       "Venezuela" = "Venezuela",
                       "Virgin Islands, British" = "Islas Vírgenes Británicas",
                       "Virgin Islands, US" = "Islas Vírgenes de los Estados Unidos",
                       "Vietnam" = "Vietnam", "Vanuatu" = "Vanuatu",
                       "Wallis and Futuna" = "Wallis y Futuna", "Samoa" = "Samoa",
                       "Yemen" = "Yemen", "South Africa" = "Sudáfrica",
                       "Zambia" = "Zambia", "Zimbabwe" = "Zimbabue"
                      )

#Diccionario de ids de outputs
#a nombres de pruebas según aparecen en la columna event_name del conjunto de datos
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

#Diccionario de nombres de pruebas según aparecen en la columna event_name del
#conjunto de datos
#a nombres formateados para mostrar al usuario
prueba_df_a_display <- hash("60-metres" = "60m", "100-metres" = "100m",
                        "200-metres" = "200m", "400-metres" = "400m",
                        "60-metres-hurdles" = "60mv", "100-metres-hurdles" = "100mv",
                        "110-metres-hurdles" = "110mv", "400-metres-hurdles" = "400mv",
                        "4x100-metres-relay"  = "4x100m", "4x200-metres-relay" = "4x200m",
                        "4x400-metres-relay" = "4x400m",
                        "800-metres" = "800m", "1000-metres" = "1000m",
                        "1500-metres" = "1500m", "one-mile" = "Milla",
                        "3000-metres" = "3000m", "5000-metres" = "5000m",
                        "10000-metres" = "10000m", "2000-metres-steeplechase" = "2000m obs",
                        "3000-metres-steeplechase" = "3000m obs",
                        "long-jump" = "Longitud", "triple-jump" = "Triple",
                        "high-jump" = "Altura", "pole-vault" = "Pértiga",
                        "shot-put" = "Peso", "discus-throw" = "Disco",
                        "hammer-throw" = "Martillo", "javelin-throw" = "Jabalina",
                        "1-mile-road" = "Milla en ruta", "5-kilometres" = "5km ruta",
                        "10-kilometres" = "10km ruta", "20-kilometres" = "20km ruta",
                        "half-marathon" = "Media maratón", "marathon" = "Maratón",
                        "5-kilometres-race-walk" = "5km marcha",
                        "20-kilometres-race-walk" = "20km marcha",
                        "35-kilometres-race-walk" = "35km marcha",
                        "50-kilometres-race-walk" = "50km marcha",
                        "pentathlon" = "Pentatlón", "heptathlon" = "Heptatlón",
                        "decathlon" = "Decatlón")

#Utilidades
filtrarPorTemporada <-  function(df, temporada) {
  df[year(df$date) == temporada, ]
}

filtrarPorCategoria <- function(df, categ) {
  if (categ == NOMBRE_CATEG_TODAS)
    return(df)
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
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    div(id = "contenedor_mapa",
      leafletOutput("mapa_paises", height = "100%", width = "100%"),
      absolutePanel(id = "controles", top = 100, left = 20, width = 120,
                    selectInput("temp_map", "Temporada", choices=ANYO_DATOS_MAX:ANYO_DATOS_MIN),
                    selectInput("categ_map", "Categoría",
                                choices=c(NOMBRE_CATEG_TODAS, NOMBRE_CATEG_M, NOMBRE_CATEG_F))
      ),
      absolutePanel(id = "panel_pais_selec", top = 20, right = 20, width = 250,
                    shinydashboard::box(width = 12, solidHeader = TRUE,
                                        h4(textOutput("pais_selec")),
                                        textOutput("texto_pais_selec")
                    )
      ),
      absolutePanel(id = "panel_top_paises", bottom = 20, right = 20, width = 200,
                    shinydashboard::box(width = 12, solidHeader = TRUE,
                                        h3("Top"),
                                        tableOutput("top_paises")
                    )
      )
    ),
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
    
    paleta <- colorBin("YlOrRd", domain = datos_mapa()$puntos, bins = 10)
    
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
