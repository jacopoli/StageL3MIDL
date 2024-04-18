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

setwd("c:\\Users\\malat\\OneDrive\\Bureau\\S6\\Stage\\StageL3MIDL")
source("seqToString.R")
source("utilsClustering.R")

matrix<-stringdistmatrix(list_of_strings, list_of_strings, method = "lcs")

#Partie représentation graphique par un force-directed graph
matrix_qg <-1/matrix
qgraph(matrix_qg, layout='spring', vsize=3)

tree<-hclust(as.dist(matrix), method="ward.D") 
dend<-as.dendrogram(tree)
par(mar=c(6.1, 4.1, 4.1, 2.1))

plot(tree)

N_CLUSTER = 4

cluster<-cutree(tree, N_CLUSTER)
df<-tibble(list_of_strings, cluster)
nodes_data<-get_nodes_mat(df)
nodes<-nodes_data

ds_clustered<-tibble(str_to_subset(nodes[1]), rep(1,length(nodes[1])))
colnames(ds_clustered)<-c("actionName", "sequence")
for (i in 2:N_CLUSTER){
  X<-tibble(str_to_subset(nodes[i]), rep(i,length(nodes[i])))
  colnames(X)<-c("actionName", "sequence")
  ds_clustered<-rbind(ds_clustered, X)
}
