load('final_d3_data.RData')
library(httr)
library(jsonlite)
library(dplyr) # para manipulación de dataframes

###### MANUALMENT, ARREGLEM ELS UNKNOWNS i LES NO DETECTADES COM A CIUTAT #######

data$city <- as.character(data$city)
data$nationality <- as.character(data$nationality)

data$nationality <- ifelse(data$nationality == "Puerto Rico", "PuertoRico", data$nationality)
data$city <- ifelse(data$city == "Edwards Air Force Base", "Edwards", data$city)
data$city <- ifelse(data$city == "Teton County", "Jackson", data$city)
data$city <- ifelse(data$city == "Jasper County", "Rensselaer", data$city)

unknown_city_artists <- data %>%
  filter(city == "Unknown") %>%
  select(artist_name, nationality) %>%
  distinct()
print(unknown_city_artists)

correct_info <- data.frame(
  artist_name = c("Band Aid", "Tyler, The Creator", "Dan + Shay", "NAV", "DripReport", "StaySolidRocky", "Internet Money", "ATB"),
  city = c("London", "Los Angeles", "Nashville", "Toronto", "Atlanta", "Richmond", "Los Angeles", "Freiburg"),
  nationality = c("United Kingdom", "United States", "United States", "Canada", "United States", "United States", "United States", "Germany"),
  stringsAsFactors = FALSE
)

data$artist_name <- as.character(data$artist_name)

data <- data %>%
  left_join(correct_info, by = "artist_name", suffix = c("", ".corrected")) %>%
  mutate(
    city = ifelse(!is.na(city.corrected), city.corrected, city),
    nationality = ifelse(!is.na(nationality.corrected), nationality.corrected, nationality)
  ) %>%
  select(-city.corrected, -nationality.corrected)

####################### LATITUD I LONGITUD ############################

api_key <- "XAv1H8bp0iD+snw9znO3XA==g8KXgtstezF2zl2A"

# Crear un conjunto único de combinaciones city, nationality
unique_locations <- data %>%
  select(city, nationality) %>%
  distinct()

# Añadir columnas para latitud y longitud
unique_locations$latitude <- NA
unique_locations$longitude <- NA

# Obtener coordenadas para combinaciones únicas
for (i in 1:nrow(unique_locations))  {
  city <- as.character(unique_locations$city[i])
  country <- as.character(unique_locations$nationality[i])
  city <- URLencode(city)
  country <- URLencode(country)  # Asegúrate de que la columna se llama 'nationality'
  
  api_url <- sprintf('https://api.api-ninjas.com/v1/geocoding?city=%s&country=%s', city, country)
  
  # Realizar la solicitud GET
  response <- GET(url = api_url, add_headers(`X-Api-Key` = api_key))
  
  # Verificar el estado de la respuesta y procesar el JSON
  if (status_code(response) == 200) {
    dades <- fromJSON(content(response, "text", encoding = "UTF-8"))
    if (length(dades) > 0) {  # Comprobar que la respuesta contiene datos
      first_proposal <- dades[1,]  # Tomar la primera propuesta
      unique_locations$latitude[i] <- first_proposal$latitude
      unique_locations$longitude[i] <- first_proposal$longitude
    } else {
      print(sprintf("No data returned for row %s: city = %s, country = %s", i, city, country))
    }
  } else {
    print(sprintf("Error in row %s: %s %s", i, status_code(response), content(response, "text", encoding = "UTF-8")))
  }
  
  #Sys.sleep(1)
}

# Unir las coordenadas con el dataframe original
data <- data %>%
  left_join(unique_locations, by = c("city", "nationality"))

View(data)

data <- as.data.frame(data)
save(data, file = "./7_Geoespacial/data_coordenades.RData")