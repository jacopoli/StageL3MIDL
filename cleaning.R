library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")


  setwd("c:\\Users\\malat\\OneDrive\\Bureau\\S6\\Stage\\StageL3MIDL")
ds <- read.csv(".\\data_rugby.csv", sep=";", dec=".")

DATE_MATCH = "29-04-2023"

#on filtre les donées pour garder uniquement les colonnes "utiles"
  
ds_final = data.frame(ds["team_id"], ds["ps_timestamp"],ds["ps_endstamp"], ds["action"], ds["actionName"], ds["sequence_id"])
unused_actions = list("Ref Review", "Sub In", "Sub Out", "Sequence", "", "Playmaker Options", "Period", " ", "Collection")
ds_final <- subset(ds_final, subset = !(actionName %in% unused_actions ))

#conversion des timestamp en type POSIXct

secondes_avec_ms <-  as.numeric(ds_final$ps_timestamp)

date_posix <- as.POSIXct(secondes_avec_ms, origin = "1970-01-01", tz = "UTC")

ds_final$date_posix_start <- date_posix

secondes_avec_ms <-  as.numeric(ds_final$ps_endstamp)

date_posix <- as.POSIXct(secondes_avec_ms, origin = "1970-01-01", tz = "UTC")

ds_final$date_posix_end <- date_posix

 
#ajout colonne position sur le terrain

{
  ds_final$zoneAction <- rep(1, nrow(ds_final))

  set_zone <- function(equipe){
    if (equipe == 2300){
      return(0)
    }
    if (equipe == 2350){
      return(2)
    }
  }
  
  l<-1
  while (l<nrow(ds_final)){
    if (ds_final[l,"actionName"]=="Opposition 22 Entry"){
      debut <- ds_final[l, "date_posix_start"]
      fin <- ds_final[l, "date_posix_end"]
      equipe <- ds_final[l, "team_id"]
      
      while (ds_final[l, "date_posix_end"]<=fin){
        ds_final[l, "zoneAction"]<- set_zone(equipe)
        l <- l+1
      }
    } else{
      l <- l+1
    }
  }
}
#séquençage 
{
  n_sequence = 1
  l<-1
  while (l %in% 1:nrow(ds_final)){
    
    ds_final[l,"sequence_id"] = n_sequence
    print(ds_final[l,"actionName"])
    if (ds_final[l,"actionName"] == "Possession"){
      
      temps_fin <- ds_final[l,"ps_endstamp"]
      print(temps_fin)
      l<-l+1
      
      while (ds_final[l, "ps_endstamp"]<=temps_fin){
        
        ds_final[l,"sequence_id"] = n_sequence
        l<-l+1
      }
      if (ds_final[l, "actionName"]=="Goal Kick"){
        ds_final[l,"sequence_id"] = n_sequence
      }
      n_sequence <- n_sequence +1
    }
    l<-l+1
  }
}

#on catégorise chaque action selon 3 catégories: O (Offensif), D (Defensif), N (Neutre)
{categories = c("O", "D", "O", "O", "D", "O", "N", "N", "O", "O", "O", "D", "N")
  categories[14]<- "N"
  categories[21]<-"N"
  categories[23]<-"D"
  categories[24]<-"D"
  categories[27]<-"O"
  categories[40]<-"D"
  categories[43]<-"N"
  categories[44]<-"O"
  categories[45]<-"D"
  categories[46]<-"O"
  l<-1
  for (l in 1:nrow(ds_final)){
    ds_final[l, "actionName"] <-  paste0(categories[ds_final[l, "action"]],"-",ds_final[l, "actionName"])
  }
  
}


#on sépare les séquences des deux équipes 

liste_sequences_2300 <- c()
liste_sequences_2350 <- c()
i <- 1
j <- 1
for (l in 1:nrow(ds_final)){
  
  if (ds_final[l, "action"] == 15 & ds_final[l, "team_id"] == 2300){
    liste_sequences_2300[i] <- ds_final[l, "sequence_id"]
    i <- i + 1
  } else {
    liste_sequences_2350[j] <- ds_final[l, "sequence_id"]
    j <- j + 1
  }
}

data_2300 <- data.frame(team_id=integer(),
                        date_posix_start=POSIXct(),
                        date_posix_end=POSIXct(),
                        actionName=character(),
                        zoneAction=integer(),
                        sequence_id=integer()
)
data_2350 <- data.frame(team_id=integer(),
                        date_posix_start=POSIXct(),
                        date_posix_end=POSIXct(),
                        actionName=character(),
                        zoneAction=integer(),
                        sequence_id=integer()
)

for (l in 1:nrow(ds_final)) {
  
  new_row =  list(team_id = ds_final[l, "team_id"], 
                  date_posix_start = ds_final[l, "date_posix_start"], 
                  date_posix_end = ds_final[l, "date_posix_end"],
                  actionName = ds_final[l, "actionName"],
                  zoneAction = ds_final[l, "zoneAction"],
                  sequence_id = ds_final[l, "sequence_id"]
                  
  )
  if (ds_final[l, "sequence_id"] %in% liste_sequences_2300) {
    data_2300[nrow(data_2300)+1,]<-new_row
  } else {
    data_2350[nrow(data_2350)+1,]<-new_row
  }
  
}

#informations que l'on va utiliser pour la suite 
data_2300 = distinct(data_2300)
data_2350 = distinct(data_2350)


#Isolation des séquences souhaitées"
data_2300_restricted <- subset(data_2300, subset=(sequence_id %in% 12:26))
data_2350_restricted <- subset(data_2350, subset=(sequence_id %in% 12:26))








