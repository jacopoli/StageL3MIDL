#######
### Analyse de la possession de balle
#######
library(tidyverse)
library(ggplot2)



source("cleaning.R")

####################################
### Evolution de la possession au cours du match
###

### Possession générale

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

evolPossession<- tibble( possession_2300, possession_2350, possession_2300+possession_2350)
colnames(evolPossession)<-c("poss_2300", "poss_2350", "total")
evolPossession$ratio_2300 <- evolPossession$poss_2300/evolPossession$total
ggplot(evolPossession, aes(x=seq(1,n+1),y=(ratio_2300)))+
geom_area(fill="grey")+
labs(x="Numéro sequence", y="Possession (en %)", title="Possession au cours des sequences")

####################################
### Nombre d'actions en fonction de la zone de jeu
###

### Match entier

d_2300_2<-length(which(data_2300$zoneAction==2))
d_2300_1<-length(which(data_2300$zoneAction==1))
d_2300_0<-length(which(data_2300$zoneAction==0))

d_2350_2<-length(which(data_2350$zoneAction==2))
d_2350_1<-length(which(data_2350$zoneAction==1))
d_2350_0<-length(which(data_2350$zoneAction==0))

frequences<-c(d_2300_0, d_2300_1, d_2300_2, d_2350_0, d_2350_1, d_2350_2)
zones<-c(0,1,2,0,1,2)
teams<-c(rep("Leinster", 3), rep("Toulouse", 3))
infos <-data.frame(frequences, zones, teams)

ggplot(infos,aes(x=zones, y=frequences, fill=teams))+
  geom_bar(position="dodge", stat="identity")

### Zone restreinte

d_2300_2<-length(which(data_2300_restricted$zoneAction==2))
d_2300_1<-length(which(data_2300_restricted$zoneAction==1))
d_2300_0<-length(which(data_2300_restricted$zoneAction==0))

d_2350_2<-length(which(data_2350_restricted$zoneAction==2))
d_2350_1<-length(which(data_2350_restricted$zoneAction==1))
d_2350_0<-length(which(data_2350_restricted$zoneAction==0))

frequences<-c(d_2300_0, d_2300_1, d_2300_2, d_2350_0, d_2350_1, d_2350_2)
zones<-c(0,1,2,0,1,2)
teams<-c(rep("Leinster", 3), rep("Toulouse", 3))
infos <-data.frame(frequences, zones, teams)

ggplot(infos,aes(x=zones, y=frequences, fill=teams))+
  geom_bar(position="dodge", stat="identity")



####################################
### Dispersion de la possession
###

### Match entier 
data_possession_2300 <- subset(dataPossession, subset=(team_id==2300 ))$diff
data_possession_2350 <- subset(dataPossession, subset=(team_id==2350 ))$diff
data<-tibble(dataPossession$team_id, dataPossession$diff)
colnames(data)<-c("team", "diff")
ggplot(data, aes(x=team, y=diff, group=team))+
  geom_boxplot()+
  labs(x="Equipe", y="Durée d'une phase de possession (en sec)", title="Répartition des phases de possession selon l'équipe")

### Zone restreinte

data_possession_2300_restricted <- subset(dataPossession, subset=(team_id==2300 & sequence_id %in% 12:26))$diff
data_possession_2350_restricted <- subset(dataPossession, subset=(team_id==2350 & sequence_id %in% 12:26))$diff

data<-tibble(dataPossession$team_id, dataPossession$diff)
colnames(data)<-c("team", "diff")
ggplot(data, aes(x=team, y=diff, group=team))+
  geom_boxplot()+
  labs(x="Equipe", y="Durée d'une phase de possession (en sec)", title="Répartition des phases de possession selon l'équipe")

