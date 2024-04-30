setwd("C:/Users/Cai Selvas Sala/GIA_UPC/2nC/1rQ/ME/Project")

load('data_preprocessed.RData')

################################################################################

# Variables numèriques
numerical_vars <- !sapply(data, is.factor)
sum(numerical_vars == T)

# Dataset només amb numèriques
data_numerical <- data[, numerical_vars]
names(data_numerical)

# Eliminem les variables que no aporten informació nova
data_numerical <- subset(data_numerical, select = -c(album_popularity, artist_followers))
names(data_numerical)

columnas_a_mantener <- setdiff(colnames(data), colnames(data_numerical))

data_categorical <- data[, columnas_a_mantener]
names(data_categorical)

data_categorical <- subset(data_categorical, select = -c(track_id, track_name, album_name, album_label, artist_name, day_release, year_release, artist_followers, album_popularity))

################################################################################
# Guardem en variables els colors que utilitzarem pels plots
custom_green = "#1db954"
custom_orange = "#ff7b24"
custom_purple = "#df75ff"
custom_darkgreen = "#2d6d62"
custom_yellow = "#cdf564"
custom_red = "#c6182c"
custom_blue = "#0051ff"
custom_lightgreen = "#4acf79"

################################################################################

# PCA
pc1 <- prcomp(data_numerical, scale = TRUE)
dim(pc1)
names(pc1)
pc1$rotation
pc1$sdev
pc1$x

################################################################################

# Guardem els valors de la variança
varianza <- pc1$sdev^2
varianza
total_varianza <- sum(varianza)
total_varianza
perc_varianza <- 100*varianza/total_varianza

library(ggplot2) # Per fer gràfics més bonics

# Creem el dataframe per ggplot
df_varianza <- data.frame(
  Componente = 1:length(perc_varianza),
  Varianza = perc_varianza)

# Gràfic de la variança en percentatge per cada component
p <- ggplot(df_varianza, aes(x = factor(Componente))) +
  # Barres de variança
  geom_bar(aes(y = Varianza), stat = "identity", fill = custom_green, alpha = 1) +
  
  # Etiquetes i títols
  labs(
    x = "Component Principal",
    y = "Percentatge (%)",
    title = "Variança explicada per Components Principals",
  ) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank()
  )

ggsave(plot = p, filename = paste('Percentatge_varianca_per_component', '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Plots/ACP', sep = ""), width = 8, height = 6, dpi = 300)


perc_varianza_acum <- 100*cumsum(varianza[1:ncol(data_numerical)])/ncol(data_numerical)
perc_varianza

################################################################################

# Gráfic acumulatiu de la variança explicada
barplot(perc_varianza_acum, 
        names.arg = 1:length(perc_varianza_acum),
        ylim = c(0, 100),
        xlab = "Nombre de components",
        ylab = "Percentatge (%) acumulat de variança",
        col = custom_green,
        border = custom_darkgreen)
abline(h = 80, col = custom_red, lty = 2) # Afegeix la línia discontínua en 80%

# Trobem el número de componenets que expliquen el 80% de les dades
num_comp <- which(cumsum(perc_varianza) >= 80)[1]
num_comp

################################################################################

psi = pc1$x[, 1:num_comp] # ENS QUEDEM NOMÉS AMB LES DIMENSIONS QUE HEM TRIAT (9)
dim(psi)

# Càlcul dels percentatges de variació explicada per els dos primers components
perc_var1 <- round(perc_varianza[1], 2)
perc_var2 <- round(perc_varianza[2], 2)

# Etiquetes pels eixos dels gràfics
label_x <- paste("1r component (", perc_var1, "%)")
label_y <- paste("2n component (", perc_var2, "%)")

iden = row.names(data_numerical)
etiq = names(data_numerical) # ETIQUETEM ELS NOSTRES INDIVIDUS
ze = rep(0,length(etiq)) # Vector de 0s

eje1 <- 1
eje2 <- 2

df_psi <- data.frame(PC1 = psi[, eje1], PC2 = psi[, eje2], ID = iden)

# Gràfic de totes les dades projectades sobre els dos primers components principals
p <- ggplot(df_psi, aes(x = PC1, y = PC2)) +
  geom_vline(aes(xintercept = 0), color = "black", linewidth = 0.55) +
  geom_hline(aes(yintercept = 0), color = "black", linewidth = 0.55) +
  theme_minimal() +
  theme(axis.line = element_line(color = "black"),
        panel.grid.major = element_line(linetype = "dashed", color = "black"),
        legend.position = "none") +
  geom_point(aes(color = ID), alpha = 0.35) +
  labs(x = label_x,y = label_y,
       title = "Projecció de totes les dades sobre els 2 primers Components Principals",
       subtitle = "Punts colorejats per nombre de fila (files ordenades per data de cada setmana)")

ggsave(plot = p, filename = paste('Data_C1_C2', '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Plots/ACP', sep = ""), width = 8, height = 6, dpi = 300)


################################################################################

# Loop per generar el gràfic de les variables numèriques projectades a totes les combinacions dels 9 components principals
for (eje1 in 1:(num_comp - 1)) {
  for (eje2 in (eje1 + 1):num_comp) {
    
    # Actualitzar les correlaciones dels eixos
    phi = cor(data_numerical, psi)
    df_phi <- data.frame(x = phi[, eje1], y = phi[, eje2], Variable = etiq)
    
    # Generar el gràfic
    p <- ggplot(df_phi, aes(x = x, y = y)) +
      theme_minimal() +
      theme(
        axis.line = element_line(color = "black"),
        # Cambiar la cuadrícula principal de los ejes a líneas punteadas
        panel.grid.major.x = element_line(color = "black", linetype = "dashed"),
        panel.grid.major.y = element_line(color = "black", linetype = "dashed"),
        panel.grid.minor = element_blank(), # Eliminar la cuadrícula secundaria
        axis.ticks = element_line(color = "black"),
        legend.position = "none"
      ) + 
      geom_hline(yintercept = 0, linetype = "solid", size = 0.75, color = "black") +
      geom_vline(xintercept = 0, linetype = "solid", size = 0.75, color = "black") +
      geom_point(color = custom_orange, size = 3, alpha = 0.5) +
      geom_segment(aes(x = 0, y = 0, xend = x, yend = y), arrow = arrow(length = unit(0.1, "inches")), size = 0.75 , alpha = 0.9, color = custom_green) + 
      geom_text(aes(label = Variable), color = custom_green, size = 3.25, fontface = "bold", hjust = 0.75, vjust = 2) + 
      labs(
        x = paste("Component", eje1, '(', round(perc_varianza[eje1], 2), '%)'), 
        y = paste("Component", eje2,'(', round(perc_varianza[eje2], 2),'%)'),
        title = paste("Correlació de las variables numèriques amb els components", eje1, "y", eje2)
      )
    
    # Imprimir gràfic
    print(p)
    
    ggsave(plot = p, filename = paste('Num_C', eje1, '_C' , eje2, '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Plots/ACP', sep = ""), width = 8, height = 6, dpi = 300)
    
  }
}

################################################################################

eje1 <- 1
eje2 <- 2
combined_data <- data.frame(x = psi[, eje1], y = psi[, eje2])
df_phi <- data.frame(x = phi[, eje1], y = phi[, eje2], Variable = etiq)

# Determinem el rang de les dimensions
range_x = max(psi[, eje1]) - min(psi[, eje1])
range_y = max(psi[, eje2]) - min(psi[, eje2])

# Determinem la magnitud màxima de les fletxes
max_arrow_magnitude = sqrt(max(df_phi$x)^2 + max(df_phi$y)^2)

# Calculem el factor d'escala per reduir la mida de les fletxes
scale_factor = min(range_x, range_y) / (2 * max_arrow_magnitude)

# Apliquem el factor d'escala
df_phi$x_scaled = df_phi$x * scale_factor
df_phi$y_scaled = df_phi$y * scale_factor

# Gràfic de cada variable categòrica sobre les fletxes de les variables numèriques i projectada en els primers 2 components principals. També es mostren les mitjanes de cada categoria de la variable
for (varcat_name in colnames(data_categorical)){
  cat("Processing plot:", varcat_name, "\n")
  varcat = factor(data[, varcat_name])
  
  combined_data$category <- varcat
  
  p <- ggplot() +
    theme_minimal() +
    
    # Punts de colors
    geom_point(data = combined_data, aes(x = x, y = y), color = "gray80", size = 2, alpha = 0.2) + 
    
    theme(
      axis.line = element_line(color = "black"),
      # Cambiar la cuadrícula principal de los ejes a líneas punteadas
      panel.grid.major.x = element_line(color = "black", linetype = "dashed"),
      panel.grid.major.y = element_line(color = "black", linetype = "dashed"),
      panel.grid.minor = element_blank(), # Eliminar la cuadrícula secundaria
      axis.ticks = element_line(color = "black"),
      legend.position = "none"
    ) + 
    geom_hline(yintercept = 0, linetype = "solid", size = 0.75, color = "black") +
    geom_vline(xintercept = 0, linetype = "solid", size = 0.75, color = "black") +
    
    # Fletxes
    geom_segment(data = df_phi, aes(x = 0, y = 0, xend = df_phi$x_scaled, yend = df_phi$y_scaled), arrow = arrow(length = unit(0.1, "inches")), size = 0.75, alpha = 0.9, color = custom_green) +
    geom_text(data = df_phi, aes(x = x_scaled, y = y_scaled, label = Variable), color = custom_lightgreen, fontface = "bold", size = 3.25, hjust = 0.75, vjust = 2) +	
    
    labs(
      x = label_x, y = label_y,
      title = paste("Variable", varcat_name, "projectada en el 1r y 2n Component Principal"),
      color = varcat_name
    ) +
    theme(legend.position = "bottom") +
    scale_color_discrete(name = paste("Valor de la variable", varcat_name))
  
  # Calculem les mitjanes
  tapply1 = tapply(combined_data$x, combined_data$category, mean)
  tapply2 = tapply(combined_data$y, combined_data$category, mean)
  
  # Text
  df_text = data.frame(x = tapply1, y = tapply2, label = levels(varcat))
  p2 <- p + geom_text(data = df_text, aes(x = x, y = y, label = label),
                      color = custom_red, size = 5, fontface = "bold")
  print(p2)
  
  ggsave(plot = p2, filename = paste('Cat_C1_C2_', varcat_name, '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Plots/ACP', sep = ""), width = 8, height = 6, dpi = 300)
}

################################################################################

# Retallem el dataset de variables categòriques perquè només hi hagi les que ens aporten més informació en els primers components principals
new_data_categorical <- subset(data_categorical, select = -c(album_type, electro, explicit, mode, month_release, month_week, pop, rank_group, weekday_release, key, hip_hop))
dim(new_data_categorical)

custom_colors <- c(custom_green, custom_orange, custom_purple, custom_darkgreen, custom_yellow, custom_red, custom_blue)
color_mapping <- setNames(custom_colors, colnames(new_data_categorical))


list_df_text <- list()

# Gràfic de totes les mitjanes de cada classe de cada variable categòrica sobre les variables numèriques i el 1r i 2n component principal
for (varcat_name in colnames(new_data_categorical)) {
  varcat <- factor(data[, varcat_name])
  
  tapply1 <- tapply(combined_data$x, varcat, mean)
  tapply2 <- tapply(combined_data$y, varcat, mean)
  
  df_temp <- data.frame(x = tapply1, y = tapply2, label = levels(varcat), category = rep(varcat_name, length(levels(varcat))))
  list_df_text <- append(list_df_text, list(df_temp))
}

df_all_text <- do.call(rbind, list_df_text)

new_scaled_division <- 2

# Realitzar el gràfic
p <- ggplot() +
  theme_minimal() +
  theme(
    axis.line = element_line(color = "black"),
    # Cambiar la cuadrícula principal de los ejes a líneas punteadas
    panel.grid.major.x = element_line(color = "black", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "black", linetype = "dashed"),
    panel.grid.minor = element_blank(), # Eliminar la cuadrícula secundaria
    axis.ticks = element_line(color = "black"),
    legend.position = "bottom"
  ) + 
  scale_fill_discrete(name = "Variable categòrica") +
  
  geom_hline(yintercept = 0, linetype = "solid", size = 0.75, color = "black") +
  geom_vline(xintercept = 0, linetype = "solid", size = 0.75, color = "black") +
  
  # Fletxes de variables numèriques
  geom_segment(data = df_phi, aes(x = 0, y = 0, xend = x_scaled/new_scaled_division, yend = y_scaled/new_scaled_division), arrow = arrow(length = unit(0.1, "inches")), size = 0.75, alpha = 0.9, color = "grey50") +
  geom_text(data = df_phi, aes(x = x_scaled/new_scaled_division, y = y_scaled/new_scaled_division, label = Variable), fontface = "bold", size = 3.25, hjust = 0.75, vjust = 2, color = "grey50") +
  
  # Categories
  geom_text(data = df_all_text, aes(x = x, y = y, label = label, color = category), fontface = "bold", size = 5) +
  scale_color_manual(values = color_mapping) +	
  
  labs(
    x = label_x, y = label_y,
    title = "Relació de les variables amb els 2 primers Components Principals",
    subtitle = "Mitjanes de les categories de cada variable categòrica"
  )

print(p)

ggsave(plot = p, filename = paste('All_Cat_C1_C2_', varcat_name, '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Plots/ACP', sep = ""), width = 8, height = 6, dpi = 300)




