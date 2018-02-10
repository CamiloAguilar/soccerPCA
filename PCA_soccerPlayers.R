#*****************************************************************************************************************
#*****************************************  Soccer Players   *****************************************************
#*****************************************************************************************************************
source("fun.R")
options(scipen=999)
load.lib("FactoMineR", "Factoshiny", "corrplot", "PerformanceAnalytics", "foreign", "factoextra", "ggplot2",
         "corrplot", "readxl", "fastcluster")


## lectura de la informacion
jugadores<-read.table("jugadores.csv", sep=";", header=T)
rownames(jugadores)<-jugadores[,1]; jugadores<-jugadores[,-1]

## escalamos la matriz
ACP<-PCA((jugadores[,4:24]))
write.infile(ACP, "ACP_parcial1.csv")

## Contribución de las variables
sort(ACP$var$contrib[,1], decreasing = T) ## Eje1
sort(ACP$var$contrib[,2], decreasing = T) ## Eje2

## correlación de las variables
ACP$var$cor

##biplot
fviz_pca_biplot(ACP, alpha.var="cos2", label = "var", 
                title = "Biplot Jugadores", axes = c(1,2)) +
      scale_color_brewer(palette="Dark2")

fviz_pca_biplot(ACP, alpha.var="cos2", label = "ind", 
                title = "Biplot Jugadores", axes = c(1,2)) +
      scale_color_brewer(palette="Dark2")

## Análisis clúster
HCPC.Jugadores<-HCPCshiny(ACP)

##Grupo de Jackson
res.HCPC<-HCPC(ACP,nb.clust=5,consol=FALSE,graph=FALSE,metric='euclidean')
res.hcpc<-res.HCPC
fviz_cluster(res.HCPC)

## Se observa que pertenece al cluster 3
res.HCPC$data.clust[c("Jackson Martinez"),c("clust")]


##Jugadores por cluster

clustersJugadores<-as.data.frame(table(res.HCPC$data.clust$clust))
names(clustersJugadores)<-c("cluster","Num.Jugadores")
clustersJugadores


