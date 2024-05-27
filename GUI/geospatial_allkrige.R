load('./7_Geoespacial/mean_new_data.RData')

mean_new_data <- na.omit(mean_new_data) # Borrem el global


# variograma i kriging ----------------------------------------------------
perform_kriging <- function(df, variable_name, rangemax, rangemin, continent = NULL, data_name = "data") {
  
  # Països i informació de continents
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  # Filtrem les dades per continent si cal
  if (!is.null(continent)) {
    if (continent == "Europe") {
      european_countries <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", 
                              "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", "Cyprus", 
                              "Czechia", "Denmark", "Estonia", "Finland", "France", "Georgia", 
                              "Germany", "Greece", "Hungary", "Iceland", "Ireland", "Italy", 
                              "Kazakhstan", "Kosovo", "Latvia", "Liechtenstein", "Lithuania", 
                              "Luxembourg", "Malta", "Moldova", "Monaco", "Montenegro", "Netherlands", 
                              "North Macedonia", "Norway", "Poland", "Portugal", "Romania", 
                              "San Marino", "Serbia", "Slovakia", "Slovenia", "Spain", 
                              "Sweden", "Switzerland", "Turkey", "Ukraine", "United Kingdom", 
                              "Vatican City")
      overseas_territories <- c("French Guiana", "Guadeloupe", "Martinique", "Réunion", "Mayotte")
      world <- world[world$name %in% european_countries, ]
      world <- world[!world$name %in% overseas_territories, ]
      df <- df[df$continent == continent,]
    }else{
      world <- world[world$continent == continent, ]
      df <- df[df$continent == continent,]
    }
  }
  
  lon_range <- c(-174, 174)
  lat_range <- c(-84, 84)
  resolution <- 0.5
  
  lon_grid <- seq(lon_range[1], lon_range[2], by = resolution)
  lat_grid <- seq(lat_range[1], lat_range[2], by = resolution)
  
  # Creem le grid
  grid <- expand.grid(lon = lon_grid, lat = lat_grid)
  grid_sf <- st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
  grid_land <- st_join(grid_sf, world, join = st_within)
  
  # Filtrem els punts que no estan a sobre el mapa (oceà)
  
  # Reconvertim a spatial per fer el kriging
  grid_land_sp <- as(grid_land, "Spatial")
  gridded(grid_land_sp) <- TRUE
  
  grid <- grid_land_sp
  
  # Variograma
  coordinates(df) <- ~lon + lat
  proj4string(df) <- CRS("+proj=longlat +datum=WGS84 +no_defs")
  
  formula <- as.formula(paste(variable_name, "~ 1"))
  
  repeat {
    cutoff <- as.numeric(readline(prompt = "Enter variogram cutoff: "))
    width <- as.numeric(readline(prompt = "Enter variogram width: "))
    
    v <- variogram(formula, data = df, cutoff = cutoff, width = width)
    print(plot(v))
    
    if (tolower(readline(prompt = "Are you satisfied with the cutoff and width? (yes/no): ")) == "yes") {
      break
    }
  }
  
  print(show.vgms())
  model <- readline(prompt = "Enter the model: ")
  
  repeat {
    psill <- as.numeric(readline(prompt = "Enter partial sill: "))
    range <- as.numeric(readline(prompt = "Enter range: "))
    nugget <- as.numeric(readline(prompt = "Enter nugget: "))
    
    variogram_model <- vgm(psill = psill, model = model, range = range, nugget = nugget)
    print(plot(v, pl = TRUE, model = variogram_model))
    
    if (tolower(readline(prompt = "Are you satisfied with the variogram model? (yes/no): ")) == "yes") {
      break
    }
  }
  
  # Variograma "automàtic"
  v_model <- fit.variogram(v, model = variogram_model)
  print(plot(v, model = v_model, main = "Ajust del Model automàtic"))
  
  # Pregunta quin vols
  use_fitted <- tolower(readline(prompt = "Do you want to use the fitted variogram model? (yes/no): "))
  if (use_fitted == "yes") {
    final_model <- v_model
    print(v_model)
  } else {
    final_model <- variogram_model
  }
  
  # Mateix tipus de coordenades
  gridded(grid) <- TRUE
  proj4string(grid) <- CRS("+proj=longlat +datum=WGS84")
  
  
  # Validació
  kriging_cv <- krige.cv(formula, df, model = final_model, nfold = nrow(data), verbose = FALSE)
  
  me <- mean(kriging_cv$residual)
  rmse <- sqrt(mean(kriging_cv$residual^2))
  msre <- mean(kriging_cv$zscore^2)
  
  cat("Error medio (ME):", me, "\n")
  cat("Raíz del error cuadrático medio (RMSE):", rmse, "\n")
  cat("Error cuadrático medio de los z-scores (MSRE):", msre, "\n")
  
  
  # Interpol·lació
  kriged <- krige(formula, locations = df, newdata = grid, model = final_model)
  
  plot(kriged)
  
  spplot(kriged["var1.pred"], main = paste("Kriged", variable_name))
  
  kriged$var1.pred <- pmax(pmin(kriged$var1.pred, rangemax), rangemin)
  
  spplot(kriged["var1.pred"], main = paste("Kriged", variable_name))
  
  kriged_df <- as.data.frame(kriged)
  
  df_sf <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  
  # Plot amb ggplot2
  a <- ggplot() +
    geom_tile(data = kriged_df, aes(x = coords.x1, y = coords.x2, fill = var1.pred)) +
    scale_fill_gradient(low = "blue", high = "red", limits = c(rangemin, rangemax), name = paste("Predicted", variable_name)) +
    geom_sf(data = df_sf, color = "black", size = 2) +
    theme_minimal() +
    ggtitle(paste("Kriged", variable_name, "with Original Data Points")) +
    coord_sf(xlim = lon_range, ylim = lat_range)
  print(a)
  
  print(paste0("Values used: cutoff - ", cutoff, ", width - ", width, ", psill - ", psill, ", range - ", range, ", nugget - ", nugget))
  kriged_df
}


krige_variables <- function(variables) {
  kriged_results <- lapply(variables, function(var) {
    # Perform kriging for each variable
    krige_output <- perform_kriging(mean_new_data, var, rangemin = 0, rangemax = 1)
    krige_output$var1.pred
  })
  kriged_df <- as.data.frame(do.call(cbind, kriged_results))
  colnames(kriged_df) <- variables
  kriged_df
}


allkrige <- krige_variables(c( "energy", "danceability", "valence"))

krige_output <- perform_kriging(mean_new_data, "popularity",100, 0)
krige_output$track_popularity <- krige_output$var1.pred
coords <- krige_output[c("track_popularity","coords.x1", "coords.x2")]
coords <- bind_cols(allkrige, coords)
coordinates(coords) <- ~coords.x1 + coords.x2
allkrige <- coords
save(allkrige, file="GUI/allkrigeresults.RData")

allkrige[coords$coo]


mean_new_data$is_explicit
mean_new_data <- mean_new_data %>% 
  mutate(explicit_num = ifelse(is_explicit=='True', 1, 0))

explicit <- krige_variables(c( "explicit_num"))



