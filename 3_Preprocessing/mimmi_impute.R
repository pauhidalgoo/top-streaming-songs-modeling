# This code has been created by Karina Gibert, from Universitat PolitÃ¨cnica de Catalunya (Barcelona, Spain)
# Please, in all uses of this code, refer to her and the paper https://doi.org/10.1080/00207160.2013.783209
# Complete reference:
#  Gibert, K. (2014). Mixed intelligent-multivariate missing imputation. International Journal of Computer Mathematics, 91(1), 85-96
#Barcelona May 2019

install.packages("StatMatch")
library(cluster)
require(StatMatch)

#assume missings represented with NA
uncompleteVar<-function(vector){any(is.na(vector))}

Mode <- function(x) 
{
  x<-as.factor(x)
  maxV<-which.max(table(x))
  return(levels(x)[maxV])
}



MiMMi <- function(data, priork=-1)
{
  #Identify columns without missings
  colsMiss<-which(sapply(data, uncompleteVar))
  if(length(colsMiss)==0){
    print("Non missing values found")
    out<-dd
  }else{
    K<-dim(data)[2]
    colsNoMiss<-setdiff(c(1:K),as.vector(colsMiss))
    
    #cluster with complete data
    dissimMatrix <- daisy(data[,colsNoMiss], metric = "gower", stand=TRUE)
    distMatrix<-dissimMatrix^2
    
    hcdata<-hclust(distMatrix, method = "ward.D2")
    plot(hcdata)
    
    if(priork==-1){
      nk<-readline("See the dendrogramm and enter a high number of clusters (must be a positive integer). k: ")
      nk<-as.integer(nk)
    }else{nk<-priork}
    
    partition<-cutree(hcdata, nk)
    CompleteData<-data
    #nomes cal per tenir tra?a de com s'ha fet la substituci?
    newCol<-K+1
    CompleteData[,newCol]<-partition
    names(CompleteData)[newCol]<-"ClassAux"
    
    setOfClasses<-as.numeric(levels(as.factor(partition)))
    imputationTable<-data.frame(row.names=setOfClasses)
    p<-1
    
    for(k in colsMiss)
    {
      #Files amb valors utils
      rowsWithFullValues<-!is.na(CompleteData[,k])
      
      #calcular valors d'imputacio
      if(is.numeric(CompleteData[,k]))
      {
        imputingValues<-aggregate(CompleteData[rowsWithFullValues,k], by=list(partition[rowsWithFullValues]), FUN=mean)
      }else{
        imputingValues<-aggregate(CompleteData[rowsWithFullValues,k], by=list(partition[rowsWithFullValues]), FUN=Mode)
      }
      
      #Impute
      
      for(c in setOfClasses)
      {
        CompleteData[is.na(CompleteData[,k]) & partition==c,k]<-imputingValues[c,2]
      }
      print(imputingValues)
      #Imputation Table
      imputationTable[,p]<-imputingValues[,2]
      names(imputationTable)[p]<-names(data)[k]
      p<-p+1
    }
    
    rownames(imputationTable)<-paste0("c", 1:nk)
    out<-new.env()
    out$imputedData<-data
    out$imputation<-imputationTable
    out$completeData<-CompleteData
  }
  return(out)
}



### Checking
load("./3_Preprocessing/data_na_added.RData")
numeric_cols <- c("track_popularity","album_popularity", "artist_popularity", "artist_num","energy","loudness","speechiness","acousticness", "danceability","liveness","valence","tempo", "duration","streams")

dd <- data[,numeric_cols]

#Now dd has 2 missing values and MIMMI can help


#run MIMMI
dimpute<-MiMMi(dd)

#table of imputation values used
dimpute$imputation
a <- dimpute$completeData
b <- a[,-15]
data[,numeric_cols] <- b


save(data, file = "./3_Preprocessing/data_mimmi_imputed.RData")

