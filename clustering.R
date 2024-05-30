library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")
library("stringr")
library("stringdist")
library("DescTools")
library("PTXQC")
library("qgraph")

source("seqToString.R")
source("utilsClustering.R")

#on supprime les valeurs nulles dans list_of_string
list_of_strings_full <- na.omit(list_of_strings)

#calcul de la matrice de distance
matrix<-stringdistmatrix(list_of_strings_full, list_of_strings_full, method = "lcs")

#Partie représentation graphique par un force-directed graph
GRAPHE=FALSE
if (GRAPHE){matrix_qg <-1/matrix
qgraph(matrix_qg, layout='spring', vsize=3)
}

#création de l'arbre
tree<-hclust(as.dist(matrix), method="ward.D") 
dend<-as.dendrogram(tree)
par(mar=c(6.1, 4.1, 4.1, 2.1))

plot(tree)

N_CLUSTER = 5

#récupère les elements annotés de leur cluster + calcul des representants
cluster<-cutree(tree, N_CLUSTER)
df_clustered<-tibble(list_of_strings_full, cluster)
colnames(df_clustered)<-c("string", "cluster")
nodes<-get_representants(df_clustered, N_CLUSTER)


