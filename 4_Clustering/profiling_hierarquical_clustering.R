#read data only if required
setwd("C:/Users/Cai Selvas Sala/GIA_UPC/2nC/1rQ/ME/Project")

# Només descomentar-ne un:
load('data_k3_s.RData') # K-Means s
load('data_h4_s.RData') # Hierarchical 4 s
load('data_h2_s.RData') # Hierarchical 2 s

data <- data_k3

data$clusterK3 <- as.factor(data_k3$clusterK3)
data$clusterH4 <- as.factor(data_h4$clusterH4)
data$clusterH2 <- as.factor(data_h2$clusterH2)

levels(data$clusterK3) <- c('C1', 'C2', 'C3')
levels(data$clusterH4) <- c('C1', 'C2', 'C3', 'C4')
levels(data$clusterH2) <- c('C1', 'C2')

# Descomentar només 1
#class_colors <- c("#1db954", "#ff7b24")
#class_colors <- c("#1db954", "#ff7b24", "#df75ff")
class_colors <- c("#1db954", "#ff7b24", "#df75ff", "#67b4ff")

#Calcula els valor test de la variable Xnum per totes les modalitats del factor P
ValorTestXnum <- function(Xnum, C){
  #freq dis of fac
  nk <- as.vector(table(C));
  n <- sum(nk);
  #mitjanes x grups
  xk <- tapply(Xnum, C,mean);
  #valors test
  txk <- (xk-mean(Xnum))/(sd(Xnum)*sqrt((n-nk)/(n*nk)));
  #p-values
  pxk <- pt(txk,n-1,lower.tail=F);
  for(c in 1:length(levels(as.factor(C)))){if (pxk[c]>0.5){pxk[c]<-1-pxk[c]}}
  return (pxk)
}

ValorTestXquali <- function(C ,Xquali){
  taula <- table(C ,Xquali);
  n <- sum(taula);
  pk <- apply(taula,1,sum)/n;
  pj <- apply(taula,2,sum)/n;
  pf <- taula/(n*pk);
  pjm <- matrix(data=pj,nrow=dim(pf)[1],ncol=dim(pf)[2], byrow=TRUE);      
  dpf <- pf - pjm;
  dvt <- sqrt(((1-pk)/(n*pk))%*%t(pj*(1-pj)));
  #i hi ha divisions iguals a 0 dona NA i no funciona
  zkj <- dpf
  zkj[dpf!=0]<-dpf[dpf!=0]/dvt[dpf!=0];
  pzkj <- pnorm(zkj,lower.tail=F);
  for(c in 1:length(levels(as.factor(C)))){for (s in 1:length(levels(Xquali))){if (pzkj[c,s]> 0.5){pzkj[c,s]<-1- pzkj[c,s]}}}
  return (list(rowpf=pf,vtest=zkj,pval=pzkj))
  # rowpf --> La matriz de probabilidades observadas.
  # vtest --> La matriz de valores Z.
  # pval --> La matriz de p-valores.
}

#dades contain the dataset
# descomentar-ne només una
#dades <- subset(data, select = c(clusterK3, track_popularity, album_popularity, artist_num, artist_followers, artist_popularity, danceability, energy, speechiness, acousticness, loudness, valence, liveness, tempo, duration, streams, album_type, pop, hip_hop, rock, electro, christmas, cinema, latino, collab, explicit, key, mode, year_release, month_release, day_release, weekday_release, year_week, month_week, time_signature, week_index, rank_group))
dades <- subset(data, select = c(clusterH4, track_popularity, album_popularity, artist_num, artist_followers, artist_popularity, danceability, energy, speechiness, acousticness, loudness, valence, liveness, tempo, duration, streams, album_type, pop, hip_hop, rock, electro, christmas, cinema, latino, collab, explicit, key, mode, year_release, month_release, day_release, weekday_release, year_week, month_week, time_signature, week_index, rank_group))
#dades <- subset(data, select = c(clusterH2, track_popularity, album_popularity, artist_num, artist_followers, artist_popularity, danceability, energy, speechiness, acousticness, loudness, valence, liveness, tempo, duration, streams, album_type, pop, hip_hop, rock, electro, christmas, cinema, latino, collab, explicit, key, mode, year_release, month_release, day_release, weekday_release, year_week, month_week, time_signature, week_index, rank_group))

num_cols_dades <- ncol(dades)
num_rows_dades <- nrow(dades)
indexs_numerical_cols <- which(sapply(dades, is.numeric))
index_categorical_cols <- which(!sapply(dades, is.numeric))

#C must contain the class variable
# Només descomentar-ne una
#C <- dades$clusterK3
C <- dades$clusterH4
#C <- dades$clusterH2

nameC <- "classe"

num_classes <- length(levels(factor(C)))

pvalk <- matrix(data=0, nrow=num_classes, ncol=num_cols_dades, dimnames=list(levels(C), names(dades)))

# ESTADÍSTIQUES NUMÈRIQUES
for (k in indexs_numerical_cols){
  print(paste("Anàlisi per classes de la variable numèrica:", names(dades)[k]))
  print("Estadístics per groups:")
  for(s in levels(as.factor(dades$C))) {
    print(summary(dades[dades$C == s, k]))
  }
  
  # ANOVA and Kruskal-Wallis tests
  o <- oneway.test(dades[, k] ~ C)
  print(paste("p-value ANOVA:", o$p.value))
  kw <- kruskal.test(dades[, k] ~ C)
  print(paste("p-value Kruskal-Wallis:", kw$p.value))
  
  # ValorTestXnum function
  # Esta parte del código depende de la implementación de la función ValorTestXnum
  pvalk[, k] <- ValorTestXnum(dades[, k], C)
  print("p-values ValorsTest: ")
  print(pvalk[, k])
  cat('------------------------------------------------------------------------------------------------------\n')
}

# ESTADÍSTIQUES CATEGÒRIQUES
for (k in index_categorical_cols){
  print(paste("Variable", names(dades)[k]))
  print(append("Modalitats=", levels(as.factor(dades[, k]))))
  cat('\n')
  print("Test Chi quadrat: ")
  print(chisq.test(dades[, k], as.factor(C)))
  
  print("valorsTest:")
  print(ValorTestXquali(C, dades[, k]))
  cat('------------------------------------------------------------------------------------------------------\n')
}

library(ggplot2)

# PLOTS NUMÈRIQUES
for(k in indexs_numerical_cols){
  # Boxplot
  p <- ggplot(data = dades, aes_string(x = "C", y = names(dades)[k])) +
    geom_boxplot(color = 'black', fill = class_colors) +
    ggtitle(paste("Boxplot de la variable", names(dades)[k], "per", nameC)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Classe")
  
  print(p)
  
  ggsave(plot = p, filename = paste('Num_BoxPlot_', names(dades)[k], '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Plots/Profiling', sep = ""), width = 8, height = 6, dpi = 300)
  
  # Barplot of means
  means <- tapply(dades[, k], C, mean)
  p <- ggplot(data = data.frame(Category = names(means), Mean = means), aes(x = Category, y = Mean)) +
    geom_bar(stat = "identity", fill = class_colors) +
    geom_hline(aes(yintercept = mean(dades[, k], na.rm = TRUE)), linetype = "dashed", linewidth = 1) +
    scale_fill_manual(values = class_colors) +
    ggtitle(paste("Mitjana de", names(dades)[k], "per", nameC)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Mitjana") +
    xlab("Classe")
  
  print(p)
  
  ggsave(plot = p, filename = paste('Num_BarPlot_', names(dades)[k], '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Plots/Profiling', sep = ""), width = 8, height = 6, dpi = 300)
}

# PLOTS CATEGÒRIQUES
for(k in index_categorical_cols){
  if(class(dades[, k]) == "Date"){
    print(summary(dades[, k]))
    print(sd(dades[, k]))
    #decide breaks: weeks, months, quarters...
    hist(dades[, k], breaks="weeks")
  }else{
    ## Este script se utiliza para crear los dataframes que utilizan los gráficos de las cualitativas ##
    ##       (distinguiendo las frecuencias y proporciones de cada modalidad por clase)
    table_mod_freq <- table(dades[, k], C)
    table_props_modalitatClasse <- prop.table(table_mod_freq, 1)
    props_modalitatClasse <- as.data.frame(table_props_modalitatClasse)
    freqs_modalitatClasse <- as.data.frame(table_mod_freq)
    
    freqs_modalitatClasse[,"prop"] <- props_modalitatClasse[,"Freq"]
    qualis_freq_props_df <- freqs_modalitatClasse
    names(qualis_freq_props_df) <- c("Modalitat", "Classe", "Freq", "PropMod" )
    
    
    dades[, k] <- as.factor(dades[, k])
    
    marg <- table(as.factor(C))/num_rows_dades
    
    if (length(levels(dades[, k])) > 12){
      modality_colors <- c("#1db954", "#ff7b24", "#e6194B", "#ffb05f", "#4363d8", "#8b329c", "#46f0b9", "#f032e6",
                           "#bcf60c", "#fabebe", "#008080", "#e6beff", "#9A6324", "#fffac8", "#800000", "#aaffc3",
                           "#808000", "#ffd8b1", "#000075", "#808080", "#67b4ff", "#000000", "#a9a9a9", "#000080",
                           "#30005a", "#aa6e28", "#fff000", "#205633", "#00ff00", "#2fffff", "#0082c8", "#964B1f")
    }else{
      modality_colors <- c("#191414", "#1db954", "#3229da", "#c325cf", "#cf2a25", "#d46c06", "#205633", "#444444", "#67b1ff", "#8b329c" ,"#cf2555", "#d4ab06")
    }
    
    ## SNAKE Plot ##
    title_snake <- paste("Proporció de assignació a cada classe\nper cada modalitat de", names(dades)[k])
    snake_plot <- ggplot(data=qualis_freq_props_df, aes(x=Classe, y =PropMod, group=Modalitat)) +
      geom_line(aes(color=Modalitat), linewidth=0.7) +
      scale_color_manual(values=modality_colors) +
      ylab(label="Proporció de cada modalitat") +
      ylim(c(0, 1)) +
      ggtitle(title_snake) +
      theme(plot.title = element_text(hjust=0.5))
    
    print(snake_plot)
    
    ggsave(plot = snake_plot, filename = paste('Cat_SnakePlot_', names(dades)[k], '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Plots/Profiling', sep = ""), width = 8, height = 6, dpi = 300)
    
    ## BARPLOTS Múltiples ##
    title_barplot <- paste("Quantitat de instàncies de", names(dades)[k])
    title_barplot <- paste(title_barplot, "per classe")
    
    barplot <- ggplot(data=qualis_freq_props_df , aes(Classe, y=Freq, fill=Modalitat)) +
      geom_bar(stat = "identity", position=position_dodge()) +
      scale_fill_manual(values=modality_colors) +
      ylab("Freqüència") +
      ggtitle(title_barplot) +
      theme(plot.title = element_text(hjust = 0.5))
    
    print(barplot)
    
    ggsave(plot = barplot, filename = paste('Cat_BarPlot_', names(dades)[k], '.png', sep = ""), bg = 'white', path = paste(getwd(), '/Plots/Profiling', sep = ""), width = 8, height = 6, dpi = 300)
  }
}#endfor

#descriptors de les classes més significatius. Afegir info qualits
for (c in 1:length(levels(as.factor(C)))) {
  if(!is.na(levels(as.factor(C))[c])){
    print(paste("P.values per classe:", levels(as.factor(C))[c]));
    print(sort(pvalk[c,]), digits=3)
  }
  cat('-------------------------------------------------------------------------------------------------------------------------------\n')
}