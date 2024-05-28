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
  
  
#Calcul des distances
ds<-dataset
ds_distance = data.frame(ds["ID"], ds["team_id"], 
                         ds["ps_timestamp"], ds["ps_endstamp"], 
                         ds["x_coord"], ds["y_coord"], ds["x_coord_end"], ds["y_coord_end"],
                         ds["actionName"])
unused_actions = list("Ref Review", "Sub In", "Sub Out", "Sequence", "", "Playmaker Options", "Period", " ")
ds_distance <- subset(ds_distance, subset = !(actionName %in% unused_actions ))

#View(ds_distance)

ds_distance_possession <- subset(ds_distance, subset = ds_distance$actionName == "Possession")
#View(ds_distance_possession)

ds_distance_2300 <- subset(ds_distance_possession, subset = ds_distance_possession$team_id == 2300)
ds_distance_2350 <- subset(ds_distance_possession, subset = ds_distance_possession$team_id == 2350)
#View(ds_distance_2300)


diff_distances_2300 = c()
diff_distances_2350 = c()

for (l in 1:nrow(ds_distance_2300)){
  diff = ds_distance_2300[l, "x_coord_end"] - ds_distance_2300[l, "x_coord"]
  diff_distances_2300 = c(diff_distances_2300, diff)
}

for (l in 1:nrow(ds_distance_2350)){
  diff = ds_distance_2350[l, "x_coord_end"] - ds_distance_2350[l, "x_coord"]
  diff_distances_2350 = c(diff_distances_2350, diff)
}


hist(diff_distances_2300, main = "equipe 2300", xlim = c(-20, 80))
hist(diff_distances_2350, main = "equipe 2350")
print(c(mean(diff_distances_2300), sd(diff_distances_2300)))
print(c(mean(diff_distances_2350), sd(diff_distances_2350)))

View(ds)
  
  
  
  
