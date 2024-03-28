# PROFILING DE CLUSTERING DE TIME SERIES

# Afegim la informació del clúster a 'data'
load("./3_Preprocessing/data_knn_imputed_unknown.RData")

# Cal executar després d'haver creat els clusters en l'script de clustering de time series
data$cluster <- clusters_ts[match(data$track_name, unique(data$track_name))]

# Ara creem un nou conjunt de dades amb una entrada per cada cançó única
# Eliminem la variable 'streams' i deixem totes les altres
unique_songs_data <- data %>%
  select(-streams) %>%
  distinct(track_name, .keep_all = TRUE)

# Imprimim les primeres files del nou conjunt de dades per verificar
print(head(unique_songs_data))

# Perfil de clústers per a variables categòriques: Moda de cada clúster
for (cat_var in c("explicit", "gender", "is_group", "nationality", "album_type", "pop", "hip_hop", "rock", "electro", "christmas", "cinema", "latino", "collab", "artist_name", "year_release", "weekday_release")) {
  # Comptem les ocurrències de cada categoria dins de cada clúster
  cat_count <- unique_songs_data %>%
    group_by(cluster, .data[[cat_var]]) %>%
    summarise(count = n(), .groups = 'drop') 
  
  # Calculem el total de cançons en cada clúster
  total_count <- unique_songs_data %>%
    group_by(cluster) %>%
    summarise(total = n(), .groups = 'drop')
  
  # Unim els comptes per categoria amb el total per clúster
  cat_profiles <- left_join(cat_count, total_count, by = "cluster")
  
  # Calculem el percentatge que representa cada categoria dins del seu clúster
  cat_profiles <- cat_profiles %>%
    mutate(percentage = (count / total) * 100) 
  
  # Ordenem per clúster i per compte descendent
  cat_profiles <- cat_profiles %>%
    arrange(cluster, desc(count))
  
  print(cat_profiles, n = Inf)
}

# Perfil de clústers per a variables numèriques: Mitjana de cada clúster
for (num_var in c("duration")) {
  num_profiles <- data %>%
    group_by(cluster) %>%
    summarise(mean = mean(.data[[num_var]], na.rm = TRUE)) %>%
    ungroup()
  
  print(num_profiles)
}
