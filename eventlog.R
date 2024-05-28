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

CLUSTERED_DATA=FALSE #travail avec les données des clusters

if (CLUSTERED_DATA){
  
  df_representants <- str_to_seq(nodes[1]) 
  
  for (i in 2:N_CLUSTER){
    df_representants<-rbind(df_representants, str_to_seq(nodes[i]))
  }

  colnames(df_representants)[9:10]<-c("start", "complete") #on renomme les colonnes timestamp/endstamp
  casted_data <- df_representants %>% 
  convert_timestamps(columns = c("start", "complete"), format = ymd_hms) %>%
  activitylog(case_id = "sequence_id", activity_id = "actionName", timestamps = c("start", "complete"), resource_id = "sequence_id")
  
}else{
  casted_data<-ds_final #on prend le dataset complet (sans Possession)
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
  resource_matrix() %>%
  plot()