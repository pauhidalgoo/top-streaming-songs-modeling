library(FactoMineR)
library(Matrix)
library(ggplot2)  
library(factoextra)
library(ggrepel)
library(FactoClass)
library(Rcpp)
library(broom)
library(pander)
library(corrplot)
library(gridExtra)

load('final_d3_data.RData')



#?MCA
### Lectura de la base de dades ###

#assuming R data (not pages web datafile)

#subset con las variables que queremos utilizar en el ACM
#sense nationality
# categorical_vars <- sapply(data, function(x) !is.numeric(x))
cats <- c('album_type', 'pop', 'hip_hop', 'rock', 'electro', 'christmas', 'cinema', 'latino', 'collab', 'explicit', 'key', 'major_mode', 'time_signature', 'is_group', 'gender', 'rank_group', "nationality")
categorical_data <- data[, cats]

# Falten algunes temporals encara


#data_acm<-subset(data,select=c('track_popularity','album_popularity', "artist_followers", 'streams', 'artist_popularity','danceability', 'energy','album_type', 'pop','hip_hop','rock','electro','christmas','cinema','latino','collab','explicit','key','major_mode','year_release','month_release','day_release','weekday_release','year_week','month_week','rank_group', 'nationality','city',  'gender', 'is_group', "loudness","speechiness", "acousticness", "liveness", "valence", "tempo","duration"))
data_acm <-subset(data, select=c('album_type', 'pop', 'hip_hop', 'rock', 'electro', 'christmas', 'cinema', 'latino', 'collab', 'explicit', 'key', 'major_mode', 'time_signature', 'is_group', 'gender', 'rank_group', "track_popularity","album_popularity", "artist_num", "artist_followers", "artist_popularity","danceability", "energy", "streams","loudness","speechiness", "acousticness", "liveness", "valence", "tempo","duration" , "year_release", "month_release", "day_release", "nationality"))

#Mini bucles para buscar el índice de las variables en nums y cats, para lugo utilizar vector de índices en suplementarias de ACM
nums<-c("track_popularity","album_popularity", "artist_num", "artist_followers", "artist_popularity","danceability", "energy", "streams","loudness","speechiness", "acousticness", "liveness", "valence", "tempo","duration" )

quali_sup <- c("year_release", "month_release", "day_release")

quali_sup_n <- c()
for (i in quali_sup){
  quali_sup_n <- append(quali_sup_n,match(i,names(data_acm)))
}

nums_n<-c()
cats_n<-c()
for (i in nums){
  nums_n <- append(nums_n,match(i,names(data_acm)))
}

for (i in cats){
  cats_n <- append(cats_n,match(i,names(data_acm)))
}

"pop" = "#1db954"
"hip_hop" = "#2d6d62"
"rock" = "#cdf564"
"electro" = "#ff7b24"
"christmas" = "#c6182c" 
"cinema" = "#df75ff"
"latino" = "pink"

res.mca1<-MCA(data_acm, quanti.sup=nums_n, quali.sup= quali_sup)
pander(head(res.mca1$eig))
#Plot para comprobar la varianza explicada por cada dimensión
dim_explainedVariance <- fviz_screeplot(res.mca1, addlabels = FALSE,
                                        barfill="#1db954", barcolor="#1db954", linecolor="#2d6d62", ylim=c(0,7.5), 
                                        main="Variància/Dimensió",xlab= "Dimensions",ylab= "Percentatge de variància explicada")
print(dim_explainedVariance)
#ggsave("ACM1_varianciaDimensions.png", plot=dim_explainedVariance, bg="white", path = PATH_PLOTS)

MCA1_contribVars1 <- fviz_contrib(res.mca1, choice="var", axes = 1, top=20,
                                  fill="#1db954", color="#1db954") +
  labs(title="Contribució de Variables a Dimensió 1") + ylab("Contribucions (%)")
print(MCA1_contribVars1)
#ggsave("ACM1_varaiblesContribDim1.png",plot=MCA1_contribVars1, bg="white", path = PATH_PLOTS,dpi=150,limitsize = FALSE,width = 1920, height=1080, units = "px")

MCA1_contribVars2 <- fviz_contrib(res.mca1, choice="var", axes = 2, top=20,
                                  fill="#cdf564", color="#cdf564") +
  labs(title="Contribució de Variables a Dimensió 2") + ylab("Contribucions (%)")
print(MCA1_contribVars2)

#plots de los niveles de las variables según cos2 y contribución:
mca1_var_cos2<-fviz_mca_var(res.mca1, col.var = "cos2", repel = TRUE, label=c("var"),
                            gradient.cols=c("#1db954","#ff7b24","#df75ff"), alpha.var = "cos2",
                            ylim=c(-10,12), xlim=c(-7,10))

print(mca1_var_cos2)
mca1_var_contrib<-fviz_mca_var(res.mca1, col.var = "contrib", repel = TRUE, label=c("var"),
                               gradient.cols=c("#1db954","#ff7b24","#df75ff"), ylim=c(-10,12), xlim=c(-7,10))

#ggsave("ACM1_variablesCos2.png", plot=mca1_var_cos2, bg="white", path = PATH_PLOTS, dpi=150,limitsize = FALSE,width = 1920, height=1080, units = "px")

#ggsave("ACM1_varaiblesContrib.png",plot=mca1_var_contrib, bg="white", path = PATH_PLOTS,dpi=150,limitsize = FALSE,width = 1920, height=1080, units = "px")

##########################################################################

length(res.mca1$eig)

num_dimensions <- min(12, ncol(res.mca1$var$coord))

# Coordenadas de los individuos para las dimensiones disponibles
if (num_dimensions > 0) {
  ind_coords_avail <- res.mca1$ind$coord[, 1:num_dimensions]
  # Coordenadas de las variables para las dimensiones disponibles
  var_coords_avail <- res.mca1$var$coord[, 1:num_dimensions]
} else {
  message("No hay dimensiones disponibles para extraer.")
}

ind_coords_12d <- res.mca1$ind$coord[, 1:12]
var_coords_12d <- res.mca1$var$coord[, 1:12]

# Usar fviz_contrib para mostrar las contribuciones en las primeras dos dimensiones
library(factoextra)
fviz_contrib(res.mca1, choice = "var", axes = 1:2, top = 20)

# Guardar las coordenadas de los individuos para las primeras 12 dimensiones
write.csv(ind_coords_12d, "ind_coords_12_dimensions.csv")

# Guardar las coordenadas de las variables para las primeras 12 dimensiones
write.csv(var_coords_12d, "var_coords_12_dimensions.csv")

##########################################################################

molts_levels <- c("year_release", "day_release", "month_release", "weekday_release","year_week","month_week", "key", "city") 
cat_supplement<-c()
for(cat in molts_levels){
  cat_supplement <- append(cat_supplement, match(cat, names(data_acm)))
}

res.mca2<-MCA(data_acm, quanti.sup=nums_n, quali.sup=cat_supplement)

pander(head(res.mca2$eig))
res.mca2$ind$coord
res.mca2$var$coord


#Categories Coordinates
cat_vars_coords <- (res.mca2$var$coord)[,1:2]

dim1<-cat_vars_coords[,1]
dim2<-cat_vars_coords[,2]
cat_var <- names(dim1)

cat_vars_coords_df <- data.frame(cat_var, dim1,dim2)
cat_vars_coords_df

print(res.mca2$eig)
eigenvalues_mean <- mean(res.mca2$eig[,2])
eigenvalues_mean
#Plot para comprobar la varianza explicada por cada dimensión
dim_explainedVariancemca2 <- fviz_screeplot(res.mca2, addlabels = TRUE,
                                            barfill="#1db954", barcolor="#1db954", linecolor="#2d6d62", 
                                            main="Variància/Dimensió",xlab= "Dimensions",ylab= "Percentatge de variància explicada")+ geom_hline(yintercept = 2.8, linetype = 2, color = "red")

ggsave("ACM2_varianciaDimensions.png", plot=dim_explainedVariancemca2, bg="white", path = PATH_PLOTS)

MCA2_contribVars1 <- fviz_contrib(res.mca2, choice="var", axes = 1,
                                  fill="#1db954", color="#1db954") +
  labs(title="Contribució de Variables a Dimensió 1") + ylab("Contribucions (%)")

MCA2_contribVars2 <- fviz_contrib(res.mca2, choice="var", axes = 2,
                                  fill="#cdf564", color="#cdf564") +
  labs(title="Contribució de Variables a Dimensió 2") + ylab("Contribucions (%)")


MCA2_contribVars3 <- fviz_contrib(res.mca2, choice="var", axes = 3,
                                  fill="#cdf564", color="#cdf564") +
  labs(title="Contribució de Variables a Dimensió 3") + ylab("Contribucions (%)")

MCA2_contribVars4 <- fviz_contrib(res.mca2, choice="var", axes = 4,
                                  fill="#cdf564", color="#cdf564") +
  labs(title="Contribució de Variables a Dimensió 4") + ylab("Contribucions (%)")

ggsave("ACM2_contribVars1.png", plot=MCA2_contribVars1, bg="white", path = PATH_PLOTS,dpi=200, width=1400,height=800, units = "px")
ggsave("ACM2_contribVars2.png", plot=MCA2_contribVars2, bg="white", path = PATH_PLOTS,dpi=200, width=1400,height=800, units = "px")
ggsave("ACM2_contribVars3.png", plot=MCA2_contribVars3, bg="white", path = PATH_PLOTS,dpi=200, width=1400,height=800, units = "px")
ggsave("ACM2_contribVars4.png", plot=MCA2_contribVars4, bg="white", path = PATH_PLOTS,dpi=200, width=1400,height=800, units = "px")


#Plots de proyecciones de niveles de variables e individuos
acm2_var_cos2 <-fviz_mca_var(res.mca2, col.var = "cos2", repel = TRUE, label=c("var"),invisible="quali.sup",
                             gradient.cols=c("#1db954","#ff7b24","#df75ff"),
                             ylim=c(-3,6), xlim=c(-2,2))


acm2_var_cos202 <-fviz_mca_var(res.mca2, col.var = "cos2", repel = TRUE, label=c("var"),invisible="quali.sup", select=list(name = NULL, cos2 = 0.2, contrib = NULL),
                               
                               gradient.cols=c("#e6a235","#df75ff"),
                               ylim=c(-3,6), xlim=c(-2,2))
plot(acm2_var_cos202)

ggsave("ACM2_variablesCos2.png", plot=acm2_var_cos2, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")
ggsave("ACM2_variablesCos2_02.png", plot=acm2_var_cos202, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")


acm2_var_cos2_13 <-fviz_mca_var(res.mca2, col.var = "cos2", axes=c(1,3), repel = TRUE, label=c("var"),invisible="quali.sup",
                             gradient.cols=c("#1db954","#ff7b24","#df75ff"),
                             ylim=c(-3,6), xlim=c(-2,2))

acm2_var_cos2_1302 <-fviz_mca_var(res.mca2, col.var = "cos2",axes=c(1,3), repel = TRUE, label=c("var"),invisible="quali.sup", select=list(name = NULL, cos2 = 0.2, contrib = NULL),
                               
                               gradient.cols=c("#e6a235","#df75ff"),
                               ylim=c(-3,6), xlim=c(-2,2))

ggsave("ACM2_variablesCos2_ax13.png", plot=acm2_var_cos2_13, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")
ggsave("ACM2_variablesCos2_ax1302.png", plot=acm2_var_cos2_1302, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")


acm2_var_corr <- fviz_mca_var(res.mca2, choice = "mca.cor",
             repel = TRUE,
             ggtheme = theme_grey())
plot(acm2_var_corr)
ggsave("ACM2_variablesCorr.png", plot=acm2_var_corr, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")


var <- get_mca_var(res.mca2)
pander(head(round(var$coord, 2), 15))


correlations_plot <- function() {
  corrplot(var$cos2, is.corr = FALSE, insig = 'blank')
}


correlations_plot()


MCA2_biplot <- fviz_mca_biplot(res.mca2, repel=TRUE, col.ind="cos2",label=c("var"), invisible="quali.sup",
                gradient.cols=c("#ff7b24","#df75ff", "blue"), col.var=("#2d6d62"),
                ylim=c(-1.5,5), xlim=c(-1.5,1.5))

ggsave("ACM2_indivBiplot.png", plot=MCA2_biplot, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")

ind <- get_mca_ind(res.mca2)

head(ind)

ind_coords <- (res.mca2$ind$coord)[,1:2]
dim1 <- ind_coords[,1]
dim2 <- ind_coords[,2]
cos2 <- (res.mca2$ind$cos2)[,1] + (res.mca2$ind$cos2)[,2]

ind_coords_df <- data.frame(dim1, dim2, cos2)

nonation_cat_vars_coords_df <- cat_vars_coords_df[!grepl("^nationality", cat_vars_coords_df$cat_var), ]

MCA2_biplot <- ggplot(nonation_cat_vars_coords_df, aes(x=dim1, y=dim2)) +
  scale_colour_gradient2(low="#ff7b24",mid="#df75ff", high="blue", midpoint = 0.3) +
  geom_point(data=ind_coords_df, aes(x=dim1, y=dim2, colour=cos2)) +
  geom_point(color="#2d6d62") +
  geom_text_repel(box.padding = 0.7,point.padding = 0.5, segment.color="#2d6d62", max.overlaps = 100, label=nonation_cat_vars_coords_df$cat_var, colour="#2d6d62") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + theme_minimal()

ggsave("ACM2_biplot.png", plot=MCA2_biplot, bg="white", path = PATH_PLOTS,dpi=110, width=1400,height=800, units = "px")


ACM2cos2_top500 <- fviz_mca_ind(res.mca2, select.ind = list(cos2 = 500), repel=TRUE, col.ind="cos2", gradient.cols=c("#ff7b24","#df75ff", "blue"))
ggsave("ACM2_cos2Top500.png", plot=ACM2cos2_top500, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")

#buscar individuos y sus títulos en los diferentes grupos que se pueden diferenciar entre los top 300 individuos por valor de cos2 
list(res.mca2$cos2)
res.mca2$ind$cos2

vec_individus = c(7721, 5823, 7809, 4715, 8066, 3170, 600, 406, 4118, 5242, 3700, 2881)
for(i in vec_individus){
  print(data[i,]$track_name)
}

fviz_ellipses(res.mca2, 1:4, 
              geom = "point")


#Plots para distinguir entre niveles de variables eje 1
MCA2_indByVarsPop<-fviz_mca_ind(res.mca2, 
                                label = "none", # hide individual labels
                                habillage = c("pop"), # color by groups 
                                palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                addEllipses = TRUE, ellipse.type = "confidence",
                                ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Pop")

MCA2_indByVarsHipHop<-fviz_mca_ind(res.mca2, 
                                   label = "none", # hide individual labels
                                   habillage = c("hip_hop"), # color by groups 
                                   palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                   addEllipses = TRUE, ellipse.type = "confidence",
                                   ggtheme = theme_minimal())  + labs(title="Individus segons els valors de Hip_hop")

MCA2_indByVarsLatino<-fviz_mca_ind(res.mca2, 
                                   label = "none", # hide individual labels
                                   habillage = c("latino"), # color by groups 
                                   palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                   addEllipses = TRUE, ellipse.type = "confidence",
                                   ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Latino")

MCA2_indByVarsChristmas<-fviz_mca_ind(res.mca2, 
                                      label = "none", # hide individual labels
                                      habillage = c("christmas"), # color by groups 
                                      palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                      addEllipses = TRUE, ellipse.type = "confidence",
                                      ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Christmas")

MCA2_indByVarsRock<-fviz_mca_ind(res.mca2, 
                                 label = "none", # hide individual labels
                                 habillage = c("rock"), # color by groups 
                                 palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                 addEllipses = TRUE, ellipse.type = "confidence",
                                 ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Rock")

MCA2_indByVarsAlbum<-fviz_mca_ind(res.mca2, 
                                  label = "none", # hide individual labels
                                  habillage = c("album_type"), # color by groups 
                                  palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                  addEllipses = TRUE, ellipse.type = "confidence",
                                  ggtheme = theme_minimal())  + labs(title="Individus segons els valors d'Album_Type")

MCA2_indByVarsExplicit<-fviz_mca_ind(res.mca2, 
                                     label = "none", # hide individual labels
                                     habillage = c("explicit"), # color by groups 
                                     palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                     addEllipses = TRUE, ellipse.type = "confidence",
                                     ggtheme = theme_minimal()) + labs(title="Individus segons els valors d'Explicit")

MCA2_indByVarsGroup<-fviz_mca_ind(res.mca2, 
                                     label = "none", # hide individual labels
                                     habillage = c("is_group"), # color by groups 
                                     palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                     addEllipses = TRUE, ellipse.type = "confidence",
                                     ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Is Group")

MCA2_indByVarsGender<-fviz_mca_ind(res.mca2, 
                                     label = "none", # hide individual labels
                                     habillage = c("gender"), # color by groups 
                                     palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                     addEllipses = TRUE, ellipse.type = "confidence",
                                     ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Gender")

ggsave("MCA2_indByVarsPop.png", plot=MCA2_indByVarsPop, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsHipHop.png", plot=MCA2_indByVarsHipHop, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsLatino.png", plot=MCA2_indByVarsLatino, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsChristmas.png", plot=MCA2_indByVarsChristmas, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsRock.png", plot=MCA2_indByVarsRock, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsAlbum.png", plot=MCA2_indByVarsAlbum, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsExplicit.png", plot=MCA2_indByVarsExplicit, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsGroup.png", plot=MCA2_indByVarsGroup, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsGender.png", plot=MCA2_indByVarsGender, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")

