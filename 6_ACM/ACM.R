
library(FactoMineR)

library(Matrix)
library(ggplot2)  
library(factoextra)
library(ggrepel)


load("./3_Preprocessing/data_knn_imputed_unknown.RData")

PATH_PLOTS = paste(getwd(),"./Media/ACM",sep="")


#?MCA
### Lectura de la base de dades ###

#assuming R data (not pages web datafile)
vec<- 1:42
exclosos<-c(1,2,3,4,6,7,8,9,10,11,21,22,26,27,27,28,29,30,31,32,33,34,35,37,40,41)



#subset con las variables que queremos utilizar en el ACM
data_acm<-subset(data,select=c('track_popularity','album_popularity', 'artist_popularity','danceability', 'energy','album_type', 'pop','hip_hop','rock','electro','christmas','cinema','latino','collab','explicit','key','major_mode','year_release','month_release','day_release','weekday_release','year_week','month_week','rank_group', 'nationality', 'city', 'gender', 'is_group'))


#Mini bucles para buscar el índice de las variables en nums y cats, para lugo utilizar vector de índices en suplementarias de ACM
nums<-c("track_popularity","album_popularity", "artist_popularity","danceability", "energy")
cats<-c("album_type", "pop","hip_hop","rock","electro","christmas","cinema","latino","collab","explicit","key","major_mode","year_release","month_release","day_release","weekday_release","year_week","month_week","rank_group")

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
"christmas" = "#c6182c", 
"cinema" = "#df75ff"
"latino" = "pink"

#Ejecución del ACM, supkementarias todas las numéricas y variables activas todas las categoricas

res.mca1<-MCA(data_acm, quanti.sup=nums_n)

res.mca1$eig

#Plot para comprobar la varianza explicada por cada dimensión
dim_explainedVariance <- fviz_screeplot(res.mca1, addlabels = FALSE,
                                        barfill="#1db954", barcolor="#1db954", linecolor="#2d6d62", ylim=c(0,7.5), 
                                        main="Variància/Dimensió",xlab= "Dimensions",ylab= "Percentatge de variància explicada")
plot(dim_explainedVariance)
ggsave("ACM1_varianciaDimensions.png", plot=dim_explainedVariance, bg="white", path = PATH_PLOTS)

MCA1_contribVars1 <- fviz_contrib(res.mca1, choice="var", axes = 1, top=20,
                                  fill="#1db954", color="#1db954") +
  labs(title="Contribució de Variables a Dimensió 1") + ylab("Contribucions (%)")

MCA1_contribVars2 <- fviz_contrib(res.mca1, choice="var", axes = 2, top=20,
                                  fill="#cdf564", color="#cdf564") +
  labs(title="Contribució de Variables a Dimensió 2") + ylab("Contribucions (%)")



#plots de los niveles de las variables según cos2 y contribución:
mca1_var_cos2<-fviz_mca_var(res.mca1, col.var = "cos2", repel = TRUE, label=c("var"),
                            gradient.cols=c("#1db954","#ff7b24","#df75ff"), alpha.var = "cos2",
                            ylim=c(-10,12), xlim=c(-7,10))

mca1_var_contrib<-fviz_mca_var(res.mca1, col.var = "contrib", repel = TRUE, label=c("var"),
                               gradient.cols=c("#1db954","#ff7b24","#df75ff"), ylim=c(-10,12), xlim=c(-7,10))

plot(mca1_var_cos2)
ggsave("ACM1_variablesCos2.png", plot=mca1_var_cos2, bg="white", path = PATH_PLOTS, dpi=150,limitsize = FALSE,width = 1920, height=1080, units = "px")

ggsave("ACM1_varaiblesContrib.png",plot=mca1_var_contrib, bg="white", path = PATH_PLOTS,dpi=150,limitsize = FALSE,width = 1920, height=1080, units = "px")



##########################################################################
#Nos da un 2.48% en el mejor eje, probamos a quitar variables temporales
molts_levels <- c("year_release", "day_release", "month_release", "weekday_release","year_week","month_week", "key", "city") 
cat_supplement<-c()
for(cat in molts_levels){
  cat_supplement <- append(cat_supplement, match(cat, names(data_acm)))
}

res.mca2<-MCA(data_acm, quanti.sup=nums_n, quali.sup=cat_supplement)

res.mca2$ind$coord
res.mca2$var$coord


#Categories Coordinates
cat_vars_coords <- (res.mca2$var$coord)[,1:2]

dim1<-cat_vars_coords[,1]
dim2<-cat_vars_coords[,2]
cat_var <- names(dim1)

cat_vars_coords_df <- data.frame(cat_var, dim1,dim2)
cat_vars_coords_df

#Plot para comprobar la varianza explicada por cada dimensión
dim_explainedVariancemca2 <- fviz_screeplot(res.mca2, addlabels = FALSE,
                                            barfill="#1db954", barcolor="#1db954", linecolor="#2d6d62", 
                                            main="Variància/Dimensió",xlab= "Dimensions",ylab= "Percentatge de variància explicada")
plot(dim_explainedVariancemca2)
ggsave("ACM2_varianciaDimensions.png", plot=dim_explainedVariancemca2, bg="white", path = PATH_PLOTS)
ggsav
MCA2_contribVars1 <- fviz_contrib(res.mca2, choice="var", axes = 1,
                                  fill="#1db954", color="#1db954") +
  labs(title="Contribució de Variables a Dimensió 1") + ylab("Contribucions (%)")

MCA2_contribVars2 <- fviz_contrib(res.mca2, choice="var", axes = 2,
                                  fill="#cdf564", color="#cdf564") +
  labs(title="Contribució de Variables a Dimensió 2") + ylab("Contribucions (%)")

ggsave("ACM2_contribVars1.png", plot=MCA2_contribVars1, bg="white", path = PATH_PLOTS,dpi=200, width=1400,height=800, units = "px")
ggsave("ACM2_contribVars2.png", plot=MCA2_contribVars2, bg="white", path = PATH_PLOTS,dpi=200, width=1400,height=800, units = "px")


#Plots de proyecciones de niveles de variables e individuos
acm2_var_cos2 <-fviz_mca_var(res.mca2, col.var = "cos2", repel = TRUE, label=c("var"),invisible="quali.sup",
                             gradient.cols=c("#1db954","#ff7b24","#df75ff"),
                             ylim=c(-3,6), xlim=c(-2,2))
plot(acm2_var_cos2)
ggsave("ACM2_variablesCos2.png", plot=acm2_var_cos2, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")


'''
MCA2_biplot <- fviz_mca_biplot(res.mca2, repel=TRUE, col.ind="cos2",label=c("var"), invisible="quali.sup",
                gradient.cols=c("#ff7b24","#df75ff", "blue"), col.var=("#2d6d62"),
                ylim=c(-1.5,5), xlim=c(-1.5,1.5))
'''
plot(MCA2_biplot)
MCA2_biplot <- ggplot(cat_vars_coords_df, aes(x=dim1, y=dim2)) +
  scale_colour_gradient2(low="#ff7b24",mid="#df75ff", high="blue", midpoint = 0.3) +
  geom_point(data=ind_coords_df, aes(x=dim1, y=dim2, colour=cos2)) +
  geom_point(color="#2d6d62") +
  geom_text_repel(box.padding = 0.7,point.padding = 0.5, segment.color="#2d6d62", max.overlaps = 100, label=cat_var, colour="#2d6d62") + 
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + theme_minimal()

ggsave("ACM2_biplot.png", plot=MCA2_biplot, bg="white", path = PATH_PLOTS,dpi=110, width=1400,height=800, units = "px")


ACM2cos2_top500 <- fviz_mca_ind(res.mca2, select.ind = list(cos2 = 500), repel=TRUE, col.ind="cos2", gradient.cols=c("#ff7b24","#df75ff", "blue"))
ggsave("ACM2_cos2Top500.png", plot=ACM2cos2_top500, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")


plot(ACM2cos2_top500)
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

ggsave("MCA2_indByVarsPop.png", plot=MCA2_indByVarsPop, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsHipHop.png", plot=MCA2_indByVarsHipHop, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsLatino.png", plot=MCA2_indByVarsLatino, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsChristmas.png", plot=MCA2_indByVarsChristmas, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsRock.png", plot=MCA2_indByVarsRock, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsAlbum.png", plot=MCA2_indByVarsAlbum, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("MCA2_indByVarsExplicit.png", plot=MCA2_indByVarsExplicit, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")




