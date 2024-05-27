library(FactoMineR)
library(Matrix)
library(ggplot2)  
library(factoextra)
library(ggrepel)

# Inicialització del PATH i càrrega de dades
PATH_PLOTS = paste(getwd(),"/Media/ACM2",sep="")

load('final_d3_data.RData')
full_data <- data



#-------------------------------------------------------------------------------
# Preparar los datos para obtener las variables categóricas del ACM
#-------------------------------------------------------------------------------
data_logical <- Filter(is.logical, full_data)
data_factors <- Filter(is.factor, full_data)

categorical_data <- cbind(data_factors, data_logical)
categorical_data <- subset(categorical_data, select = -c(track_id, track_name, album_name, day_release, month_release, year_release, weekday_release, year_week, month_week, week_index))
categorical_data <- subset(categorical_data, select = -c(artist_name, album_label, city)) # Remove variables with too many classes
categorical_data <- categorical_data[, !names(categorical_data) %in% "explicit"] # Remove target variable

names(categorical_data)


#-------------------------------------------------------------------------------
#Ejecució del ACM amb les variables utilitzades per la Xarxa Neuronal
#-------------------------------------------------------------------------------

res.mca1<-MCA(categorical_data)
res.mca1$eig

# Variància explicada per Dimensions
dim_explainedVariance <- fviz_screeplot(res.mca1, addlabels = FALSE,
                                        barfill="#1db954", barcolor="#1db954", linecolor="#2d6d62", ylim=c(0,7.5), 
                                        main="Variància/Dimensió",xlab= "Dimensions",ylab= "Percentatge de variància explicada")
ggsave("ACM1_varianciaDimensions.png", plot=dim_explainedVariance, bg="white", path = PATH_PLOTS)
#-------------------------------------------------------------------------------

# Variància aportada per Variables a Dim1 i Dim2
MCA1_contribVars1 <- fviz_contrib(res.mca1, choice="var", axes = 1, top=20,
                                  fill="#1db954", color="#1db954") +
  labs(title="Contribució de Variables a Dimensió 1") + ylab("Contribucions (%)")

# Dim2
MCA1_contribVars2 <- fviz_contrib(res.mca1, choice="var", axes = 2, top=20,
                                  fill="#cdf564", color="#cdf564") +
  labs(title="Contribució de Variables a Dimensió 2") + ylab("Contribucions (%)")

ggsave("ACM1_contribVars1.png", plot=MCA1_contribVars1, bg="white", path = PATH_PLOTS,dpi=200, width=1400,height=800, units = "px")
ggsave("ACM1_contribVars2.png", plot=MCA1_contribVars2, bg="white", path = PATH_PLOTS,dpi=200, width=1400,height=800, units = "px")
#-------------------------------------------------------------------------------

# Gràfiques amb explicibalitat segons:
#   - Cos2
mca1_var_cos2<-fviz_mca_var(res.mca1, col.var = "cos2", repel = TRUE, label=c("var"),
                            gradient.cols=c("#1db954","#ff7b24","#df75ff"), alpha.var = "cos2",
                            ylim=c(-10,12), xlim=c(-7,10))
#   - Contrib
mca1_var_contrib<-fviz_mca_var(res.mca1, col.var = "contrib", repel = TRUE, label=c("var"),
                               gradient.cols=c("#1db954","#ff7b24","#df75ff"), ylim=c(-10,12), xlim=c(-7,10))

ggsave("ACM1_variablesCos2.png", plot=mca1_var_cos2, bg="white", path = PATH_PLOTS, dpi=150,limitsize = FALSE,width = 1920, height=1080, units = "px")
ggsave("ACM1_varaiblesContrib.png",plot=mca1_var_contrib, bg="white", path = PATH_PLOTS,dpi=150,limitsize = FALSE,width = 1920, height=1080, units = "px")
#-------------------------------------------------------------------------------

# Gràfics que diferencien les projeccions segons els gèneres
#   - pop
MCA2_indByVarsPop<-fviz_mca_ind(res.mca1, 
                                label = "none", # hide individual labels
                                habillage = c("pop"), # color by groups 
                                palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                addEllipses = TRUE, ellipse.type = "confidence",
                                ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Pop")
#   - hip_hop
MCA2_indByVarsHipHop<-fviz_mca_ind(res.mca1, 
                                   label = "none", # hide individual labels
                                   habillage = c("hip_hop"), # color by groups 
                                   palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                   addEllipses = TRUE, ellipse.type = "confidence",
                                   ggtheme = theme_minimal())  + labs(title="Individus segons els valors de Hip_hop")
#   - latino
MCA2_indByVarsLatino<-fviz_mca_ind(res.mca1, 
                                   label = "none", # hide individual labels
                                   habillage = c("latino"), # color by groups 
                                   palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                   addEllipses = TRUE, ellipse.type = "confidence",
                                   ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Latino")
#   - christmas
MCA2_indByVarsChristmas<-fviz_mca_ind(res.mca1, 
                                      label = "none", # hide individual labels
                                      habillage = c("christmas"), # color by groups 
                                      palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                      addEllipses = TRUE, ellipse.type = "confidence",
                                      ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Christmas")
#   - rock
MCA2_indByVarsRock<-fviz_mca_ind(res.mca1, 
                                 label = "none", # hide individual labels
                                 habillage = c("rock"), # color by groups 
                                 palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                 addEllipses = TRUE, ellipse.type = "confidence",
                                 ggtheme = theme_minimal()) + labs(title="Individus segons els valors de Rock")
#   - album_type
MCA2_indByVarsAlbum<-fviz_mca_ind(res.mca1, 
                                  label = "none", # hide individual labels
                                  habillage = c("album_type"), # color by groups 
                                  palette = c("#1db954", "#ff7b24","#df75ff","blue","purple"),
                                  addEllipses = TRUE, ellipse.type = "confidence",
                                  ggtheme = theme_minimal())  + labs(title="Individus segons els valors d'Album_Type")


ggsave("ACM1_indByVarsPop.png", plot=MCA2_indByVarsPop, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("ACM1_indByVarsHipHop.png", plot=MCA2_indByVarsHipHop, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("ACM1_indByVarsLatino.png", plot=MCA2_indByVarsLatino, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("ACM1_indByVarsChristmas.png", plot=MCA2_indByVarsChristmas, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("ACM1_indByVarsRock.png", plot=MCA2_indByVarsRock, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
ggsave("ACM1_indByVarsAlbum.png", plot=MCA2_indByVarsAlbum, bg="white", path = PATH_PLOTS,dpi=120, width=1400,height=800, units = "px")
#-------------------------------------------------------------------------------





#-------------------------------------------------------------------------------
# ACM2
#-------------------------------------------------------------------------------

# Dada la poca explicabilidad debido a la variable nacionality, fem un segon
# ACM per poder interpretar millor els eixos, tot i que el ACM dut a terme en la
# xarxa neuronal és el previ

cat_supplement<-c()
cat_supplement <- c(match("nationality", names(categorical_data)))


res.mca2<-MCA(categorical_data, quali.sup = cat_supplement)

#Categories Coordinates
cat_vars_coords <- (res.mca2$var$coord)[,1:2]
cos2_vars <- res.mca2$var$cos2[,1:2]
cos2_vars <- apply(cos2_vars,1,sum)

dim1<-cat_vars_coords[,1]
dim2<-cat_vars_coords[,2]
cat_var <- names(dim1)

cat_vars_coords_df <- data.frame(cat_var, dim1,dim2,cos2_vars)
cat_vars_coords_df

#Plot para comprobar la varianza explicada por cada dimensión
dim_explainedVariancemca2 <- fviz_screeplot(res.mca2, addlabels = FALSE,
                                            barfill="#1db954", barcolor="#1db954", linecolor="#2d6d62", 
                                            main="Variància/Dimensió",xlab= "Dimensions",ylab= "Percentatge de variància explicada")
ggsave("ACM2_varianciaDimensions.png", plot=dim_explainedVariancemca2, bg="white", path = PATH_PLOTS)

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
ggsave("ACM2_variablesCos2.png", plot=acm2_var_cos2, bg="white", path = PATH_PLOTS,dpi=130,limitsize = FALSE,width = 1920, height=1080, units = "px")

acm2_var_cos2 <- ggplot(cat_vars_coords_df, aes(x=dim1, y=dim2, fill=cos2_vars)) + 
  scale_colour_gradient2(low="#1db954",mid="#ff7b24", high="#df75ff", midpoint = 0.3) +
  geom_point(aes(color=cos2_vars)) +
  geom_text_repel(color=cos2_vars ,box.padding = 0.7,point.padding = 0.5, max.overlaps = 100, label=cat_var) + 
  
  geom_hline(yintercept = 0) + 
  geom_vline(xintercept = 0) + theme_minimal()

acm2_var_cos2

'''
MCA2_biplot <- fviz_mca_biplot(res.mca2, repel=TRUE, col.ind="cos2",label=c("var"), invisible="quali.sup",
                gradient.cols=c("#ff7b24","#df75ff", "blue"), col.var=("#2d6d62"),
                ylim=c(-1.5,5), xlim=c(-1.5,1.5))
'''
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


#buscar individuos y sus títulos en los diferentes grupos que se pueden diferenciar entre los top 300 individuos por valor de cos2 
list(res.mca2$cos2)
res.mca2$ind$cos2

vec_individus = c(7721, 5823, 7809, 4715, 8066, 3170, 600, 406, 4118, 5242, 3700, 2881)
for(i in vec_individus){
  print(data[i,]$track_name)
}





