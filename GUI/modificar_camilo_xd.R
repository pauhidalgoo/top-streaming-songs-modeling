artists_data <- readRDS("./7_Geoespacial/artists_data.rds")
class(artists_data)


# CAMILO

artists_data <- artists_data %>%
  mutate(
    nationality = ifelse(artist_name == "Camilo", "Colombia", nationality),
    city = ifelse(artist_name == "Camilo", "Medellín", city),
    latitude = ifelse(artist_name == "Camilo", 6.2443382, latitude),
    longitude = ifelse(artist_name == "Camilo", -75.57355, longitude)
  )


# ROSALIA - Barcelona

artists_data <- artists_data %>%
  mutate(
    city = ifelse(artist_name == "ROSALÍA", "Sant Esteve Sesrovires", city),
    latitude = ifelse(artist_name == "ROSALÍA", 41.4878900, latitude),
    longitude = ifelse(artist_name == "ROSALÍA", 1.8798200, longitude)
  )

# HVME - Murcia

artists_data <- artists_data %>%
  mutate(
    city = ifelse(artist_name == "HVME", "Murcia", city),
    latitude = ifelse(artist_name == "HVME", 37.9922, latitude),
    longitude = ifelse(artist_name == "HVME", -1.1307, longitude)
  )

# Actualizar full_address
artists_data <- artists_data %>%
  mutate(
    full_address = paste(city, nationality, sep = ", ")
  )

# Guardar el dataset actualizado
saveRDS(artists_data, file = "./7_Geoespacial/artists_data.rds")


View(artists_data)
