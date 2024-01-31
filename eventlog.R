library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")


setwd("c:\\Users\\malat\\OneDrive\\Bureau\\S6\\Stage\\StageL3MIDL")
ds <- read.csv(".\\data_rugby.csv", sep=";", dec=".")

#dictionnaire qui nous permet de trouver le nom de l'action à partir de son identifiant

temp <- data.frame(ds["action"], ds["actionName"]) %>% distinct() 
dico_actionNames <- as.vector(temp)$action
names(dico_actionNames) <- as.vector(temp)$actionName
dico_actionNames <- names(sort(dico_actionNames))

#on filtre les donées pour garder uniquement les colonnes "utiles"
  
ds_final = data.frame(ds["team_id"], ds["ps_timestamp"],ds["ps_endstamp"], ds["action"], ds["actionName"], ds["sequence_id"])
unused_actions = list("Ref Review", "Sub In", "Sub Out")
ds_final <- subset(ds_final, subset = actionName != "Ref Review")
ds_final <- subset(ds_final, subset = !(actionName %in% unused_actions ))

#conversion des timestamp en type POSIXct
ds_final <- mutate(ds_final, ps_timestamp = seconds_to_period(as.vector(ps_timestamp))) 
ds_final <- mutate(ds_final, ps_timestamp = format(as.POSIXct(as.vector(ds_final[["ps_timestamp"]]), tz="UTC"), "%H-%M-%OS"))
View(ds_final)


#séquençage 
n_sequence = 1
for (l in 1:nrow(ds_final)){
  if (ds_final[l,"actionName"] == "Sequence"){
    n_sequence = n_sequence +1}
  
  ds_final[l,"sequence_id"] = n_sequence
}

#on sépare les séquences des deux équipes 


