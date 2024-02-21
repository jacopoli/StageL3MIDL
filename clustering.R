library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")
library("stringr")
library("stringdist")
library("DescTools")
library("PTXQC")

source("seqToString.R")
source("utilsClustering.R")

matrix<-stringdistmatrix(list_of_strings, list_of_strings, method = "lcs")

tree<-hclust(as.dist(matrix), method="ward.D") 

dend<-as.dendrogram(tree)
par(mar=c(6.1, 4.1, 4.1, 2.1))

plot(tree)

cluster<-cutree(tree, 5)
df<-tibble(list_of_strings, cluster)
nodes<-get_nodes(df)

ds_clustered<-tibble(str_to_subset(nodes[1]), rep(1,length(nodes[1])))
colnames(ds_clustered)<-c("actionName", "sequence")
for (i in 2:5){
  X<-tibble(str_to_subset(nodes[i]), rep(i,length(nodes[i])))
  colnames(X)<-c("actionName", "sequence")
  ds_clustered<-rbind(ds_clustered, X)
}
