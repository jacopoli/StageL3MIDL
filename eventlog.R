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

df_representants %>% 
  convert_timestamps(columns = c("start", "complete"), format = ymd_hms) %>%
  activitylog(case_id = "sequence_id", activity_id = "actionName", timestamps = c("start", "complete"))%>%
  process_map(frequency("relative"))

