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

#on va adapter les datas des clusters 

#création de faux timestamp
start <- as.POSIXct(c(1:length(ds_clustered$actionName)), origin = "1970-01-01", tz = "UTC")
complete<-start

ds_clust_format <- tibble(start, complete, ds_clustered)
ds_clust_format %>% 
  convert_timestamps(columns = c("start", "complete"), format = ymd_hms) %>%
  activitylog(case_id = "sequence", activity_id = "actionName", timestamps = c("start", "complete"), resource_id = "sequence")%>%
  process_map(frequency("relative"))

