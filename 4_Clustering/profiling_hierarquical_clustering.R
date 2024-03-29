#PROFILING HIERARCHICAL CLUSTERING

load("./3_Preprocessing/data_knn_imputed_unknown.RData")

# Cal executar després d'haver creat els clusters en l'script del HIERARCHICAL CLUSTERING

data$cluster_hier <- as.factor(data$cluster_hier)

levels(data$cluster_hier) <- c('C1', 'C2', 'C3', 'C4')

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

var_num <- data[,c("cluster_hier","track_popularity", "album_popularity", "artist_num", "artist_followers", "artist_popularity", "danceability", "energy", "loudness", "speechiness", "acousticness", "valence", "liveness", "tempo", "duration", "streams")]
categorical_vars <- data[, sapply(data, is.factor)]
categorical_vars <- subset(categorical_vars, select = -c(track_id, track_name, album_name, artist_name))

actives <- data.frame(categorical_vars, numeriques_normalitzades)
data_profiling <- subset(actives, select = -c(week_index, day_release))

num_cols_dades <- ncol(data_profiling)
num_rows_dades <- nrow(data_profiling)
indexs_numerical_cols <- which(sapply(data_profiling, is.numeric))
index_categorical_cols <- which(!sapply(data_profiling, is.numeric))

index_binary_cols <- which(sapply(data_profiling, function(x) length(unique(x)) == 2))
data_binary_cols <- data_profiling[, index_binary_cols]
index_cat_bin_cols = c(index_categorical_cols, index_binary_cols)

#C must contain the class variable
C <- data_profiling$cluster_hier

nameC <- "classe"

num_classes <- length(levels(factor(C)))

pvalk <- matrix(data=0, nrow=num_classes, ncol=num_cols_dades, dimnames=list(levels(C), names(data_profiling)))

# ESTADÍSTIQUES NUMÈRIQUES
for (k in indexs_numerical_cols){
  print(paste("Anàlisi per classes de la variable numèrica:", names(data_profiling)[k]))
  print("Estadístics per groups:")
  for(s in levels(as.factor(data_profiling$C))) {
    print(summary(data_profiling[data_profiling$C == s, k]))
  }
  
  # ANOVA and Kruskal-Wallis tests
  o <- oneway.test(data_profiling[, k] ~ C)
  print(paste("p-value ANOVA:", o$p.value))
  kw <- kruskal.test(data_profiling[, k] ~ C)
  print(paste("p-value Kruskal-Wallis:", kw$p.value))
  
  # ValorTestXnum function
  # Esta parte del código depende de la implementación de la función ValorTestXnum
  pvalk[, k] <- ValorTestXnum(data_profiling[, k], C)
  print("p-values ValorsTest: ")
  print(pvalk[, k])
  cat('------------------------------------------------------------------------------------------------------\n')
}

# ESTADÍSTIQUES CATEGÒRIQUES
for (k in index_cat_bin_cols){
  print(paste("Variable", names(data_profiling)[k]))
  print(append("Modalitats=", levels(as.factor(data_profiling[, k]))))
  cat('\n')
  print("Test Chi quadrat: ")
  print(chisq.test(data_profiling[, k], as.factor(C)))
  
  print("valorsTest:")
  print(ValorTestXquali(C, data_profiling[, k]))
  cat('------------------------------------------------------------------------------------------------------\n')
}

library(ggplot2)

# PLOTS NUMÈRIQUES
for(k in indexs_numerical_cols){
  # Boxplot
  p <- ggplot(data = data_profiling, aes_string(x = "C", y = names(data_profiling)[k])) +
    geom_boxplot(color = 'black', fill = class_colors) +
    ggtitle(paste("Boxplot de la variable", names(data_profiling)[k], "per", nameC)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    xlab("Classe")
  
  print(p)
  
  ggsave(plot = p, filename = paste('Num_BoxPlot_', names(data_profiling)[k], '.png', sep = ""), bg = 'white', path = paste('C:/Users/abril/Desktop/IA/2nASSIGNATURES/2n quatri/PMAAD/imatges_clustering', sep = ""), width = 8, height = 6, dpi = 300)
  
  # Barplot of means
  means <- tapply(data_profiling[, k], C, mean)
  p <- ggplot(data = data.frame(Category = names(means), Mean = means), aes(x = Category, y = Mean)) +
    geom_bar(stat = "identity", fill = class_colors) +
    geom_hline(aes(yintercept = mean(data_profiling[, k], na.rm = TRUE)), linetype = "dashed", linewidth = 1) +
    scale_fill_manual(values = class_colors) +
    ggtitle(paste("Mitjana de", names(data_profiling)[k], "per", nameC)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    ylab("Mitjana") +
    xlab("Classe")
  
  print(p)
  
  ggsave(plot = p, filename = paste('Num_BarPlot_', names(data_profiling)[k], '.png', sep = ""), bg = 'white', path = paste('C:/Users/abril/Desktop/IA/2nASSIGNATURES/2n quatri/PMAAD/imatges_clustering', sep = ""), width = 8, height = 6, dpi = 300)
}
library(RColorBrewer)
library(viridis)

colors_250 <- viridis::viridis(250, option = "C")

# PLOTS CATEGÒRIQUES
for(k in index_cat_bin_cols){
  if(class(data_profiling[, k]) == "Date"){
    print(summary(data_profiling[, k]))
    print(sd(data_profiling[, k]))
    #decide breaks: weeks, months, quarters...
    hist(data_profiling[, k], breaks="weeks")
  }else{
    ## Este script se utiliza para crear los dataframes que utilizan los gráficos de las cualitativas ##
    ##       (distinguiendo las frecuencias y proporciones de cada modalidad por clase)
    table_mod_freq <- table(data_profiling[, k], C)
    table_props_modalitatClasse <- prop.table(table_mod_freq, 1)
    props_modalitatClasse <- as.data.frame(table_props_modalitatClasse)
    freqs_modalitatClasse <- as.data.frame(table_mod_freq)
    
    freqs_modalitatClasse[,"prop"] <- props_modalitatClasse[,"Freq"]
    qualis_freq_props_df <- freqs_modalitatClasse
    names(qualis_freq_props_df) <- c("Modalitat", "Classe", "Freq", "PropMod" )
    
    
    data_profiling[, k] <- as.factor(data_profiling[, k])
    
    marg <- table(as.factor(C))/num_rows_dades
    
    if (length(levels(data_profiling[, k])) > 12){
      modality_colors <- viridis::viridis(length(levels(data_profiling[, k])))
    } else {
      modality_colors <- c("#191414", "#1db954", "#3229da", "#c325cf", "#cf2a25", "#d46c06", "#205633", "#444444", "#67b1ff", "#8b329c" ,"#cf2555", "#d4ab06")
    }
    
    ## SNAKE Plot ##
    title_snake <- paste("Proporció de assignació a cada classe\nper cada modalitat de", names(data_profiling)[k])
    snake_plot <- ggplot(data=qualis_freq_props_df, aes(x=Classe, y =PropMod, group=Modalitat)) +
      geom_line(aes(color=Modalitat), linewidth=0.7) +
      scale_color_manual(values=modality_colors) +
      ylab(label="Proporció de cada modalitat") +
      ylim(c(0, 1)) +
      ggtitle(title_snake) +
      theme(plot.title = element_text(hjust=0.5))
    
    print(snake_plot)
    
    ggsave(plot = snake_plot, filename = paste('Cat_SnakePlot_', names(data_profiling)[k], '.png', sep = ""), bg = 'white', path = paste('C:/Users/abril/Desktop/IA/2nASSIGNATURES/2n quatri/PMAAD/imatges_clustering', sep = ""), width = 8, height = 6, dpi = 300)
    
    ## BARPLOTS Múltiples ##
    title_barplot <- paste("Quantitat de instàncies de", names(data_profiling)[k])
    title_barplot <- paste(title_barplot, "per classe")
    
    barplot <- ggplot(data=qualis_freq_props_df , aes(Classe, y=Freq, fill=Modalitat)) +
      geom_bar(stat = "identity", position=position_dodge()) +
      scale_fill_manual(values=modality_colors) +
      ylab("Freqüència") +
      ggtitle(title_barplot) +
      theme(plot.title = element_text(hjust = 0.5))
    
    print(barplot)
   ggsave(plot = barplot, filename = paste('Cat_BarPlot_', names(data_profiling)[k], '.png', sep = ""), bg = 'white', path = paste('C:/Users/abril/Desktop/IA/2nASSIGNATURES/2n quatri/PMAAD/imatges_clustering', sep = ""), width = 8, height = 6, dpi = 300)
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