library("tidyverse")
library("ggplot2")
library("dplyr")

dataset<-data_2300_restricted

### Plot les actions
  showDeplacementSequence <- function (sequences){
    sequences_r= data.frame(sequences["x_coord"], sequences["y_coord"], sequences["sequence_id"])
    ggplot(data = sequences_r, mapping = aes(x=x_coord, y=y_coord))+
    geom_line(color=sequences_r$sequence_id, size=2)+
      geom_point()
  }
  
  showDeplacementSequence(dataset)
  
### Calcul le pourcentage de possession.
  dataPossession <- subset(ds_final, subset=(action==15))
  dataPossession$diff <- dataPossession$ps_endstamp-dataPossession$ps_timestamp
  
  n<-nrow(dataPossession)
  possession_2300<-rep(0,n+1)
  possession_2350<-rep(0,n+1)
  
  for (i in 2:n){
    if (dataPossession[i-1,]$team_id == 2300){
      possession_2300[i+1]=possession_2300[i]+dataPossession[i,]$diff
      possession_2350[i+1]=possession_2350[i]
    }
    else{
      possession_2350[i+1]=possession_2350[i]+dataPossession[i,]$diff
      possession_2300[i+1]=possession_2300[i]
    }
  }
  evolPossession<- tibble(possession_2300, possession_2350)
  
  
  
  
  
  
  
