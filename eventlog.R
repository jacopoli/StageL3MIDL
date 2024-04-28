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

df_representants <- str_to_seq(nodes[1])

for (i in 2:N_CLUSTER){
  df_representants<-rbind(df_representants, str_to_seq(nodes[i]))
}

colnames(df_representants)[9:10]<-c("start", "complete")


casted_data <- df_representants %>% 
  convert_timestamps(columns = c("start", "complete"), format = ymd_hms) %>%
  activitylog(case_id = "sequence_id", activity_id = "actionName", timestamps = c("start", "complete"), resource_id = "sequence_id")

#création du graphe POV frequence d'apparition
casted_data %>%
  process_map(frequency("relative"))

#création du graphe POV performance durée
casted_data %>%
  process_map(performance(mean, units="secs"))

#graphique mettant en avant la durée moyenne de chaque action
casted_data %>%
  processing_time("activity") %>%
  plot()