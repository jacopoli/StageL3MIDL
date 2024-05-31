library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")
library("stringr")
library("stringdist")
library("DescTools")
library("PTXQC")


source("clustering.R")

CLUSTERED_DATA=TRUE #travail avec les données des clusters

if (CLUSTERED_DATA){
  
  df_representants <- c()
  
  #On ajoute les représentants dans un vecteur de manière pondérée
  for (i in 1:N_CLUSTER){
    seq_i <- str_to_seq(nodes[i]) #on transforme le représentant en sequence
    for (j in 1:get_size_cluster(df_clustered, i)){   
      df_representants<-rbind(df_representants, mutate(seq_i, sequence_id=sequence_id+200*j))
    }
  }

  colnames(df_representants)[9:10]<-c("start", "complete") #on renomme les colonnes timestamp/endstamp
  casted_data <- df_representants %>% 
  convert_timestamps(columns = c("start", "complete"), format = ymd_hms)
  casted_data_log <- activitylog(casted_data, case_id = "sequence_id", activity_id = "actionName", timestamps = c("start", "complete"), resource_id = "sequence_id")
  
}else{
  casted_data<-data_2300 #choix du dataset 
  colnames(casted_data)[9:10]<-c("start","complete")
  casted_data<-convert_timestamps(casted_data, columns = c("start", "complete"), format = ymd_hms)
  casted_data_log<-activitylog(casted_data, case_id = "sequence_id", activity_id = "actionName", timestamps = c("start", "complete"), resource_id = "sequence_id")
  
}
#création du graphe POV frequence d'apparition
casted_data_log %>%
  process_map(frequency("relative"))

#création du graphe POV performance durée
casted_data_log %>%
  process_map(performance(mean, units="secs"))

#graphique mettant en avant la durée moyenne de chaque action
casted_data_log %>%
  processing_time("activity") %>%
  plot()

#graphique pour precedence matrix
casted_data_log %>%
  process_matrix(frequency("relative")) %>%
  plot()

#graphique pour la proportion de chaque action
casted_data_log %>%
  activity_presence() %>%
  plot()
