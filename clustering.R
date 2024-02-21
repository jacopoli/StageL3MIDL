library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")
library("stringr")
library("stringdist")
library("DescTools")
library("PTXQC")

source("utilsClustering.R")
source("demo.R")

matrix<-stringdistmatrix(list_of_strings, list_of_strings, method = "lcs")

tree<-hclust(as.dist(matrix), method="ward.D") 

dend<-as.dendrogram(tree)
par(mar=c(6.1, 4.1, 4.1, 2.1))

plot(tree)

