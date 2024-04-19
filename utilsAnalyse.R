library("tidyverse")
library("ggplot2")
library("dplyr")

dataset<-data_2300_restricted

showDeplacementSequence <- function (sequences){
  sequences_r= data.frame(sequences["x_coord"], sequences["y_coord"], sequences["sequence_id"])
  ggplot(data = sequences_r, mapping = aes(x=x_coord, y=y_coord))+
  geom_line(color=sequences_r$sequence_id, size=2)+
    geom_point(shape=sequences_r$sequence_id)
}

showDeplacementSequence(dataset)