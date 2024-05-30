library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")
library("stringr")
library("PTXQC")

source("cleaning.R")

ds_studied<-data_2300

#creation d'une liste avec les differentes actions et une liste de meme taille avec des lettres (a la main mdr)
list_of_action <- unlist(ds_studied$actionName)
list_of_action <- list_of_action[!duplicated(list_of_action)]
list_of_letter <- list("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "M", "L", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X")


#fonction qui renvoie une liste des actions de la sequence en parametre
seqID_to_actions <- function(seq_id){
  return(subset(ds_studied, subset=(sequence_id==seq_id))$actionName)
}

#fonction qui renvoie un vecteur avec les identifiants des sequences contenue dans la liste
get_all_id_seq <- function(list_seq){
  return (as.vector(unique(list_seq$sequence_id)))
}

#fonction qui renvoie la chaine de caracteres d'une sequence (on applique action_to_letter a chaque action et apres on concatene la lettre a une str)
actions_to_str <- function(subset){
  str <- ""
  for (action in subset){
    letter <- action_to_letter(action)
    str <- paste(str, letter, sep = "")
  }
  return(str)
}

str_to_actions <- function(str){
  letters <- str_to_vec(str)
  actions <- c()
  i <- 1
  for(letter in letters){
    actions[i] = letter_to_action(letter)
    i <- i+1
  }
  return(actions)
}

#fonction qui renvoie une lettre a une action
action_to_letter<-function(action){
  index <- which(list_of_action == action)
  return(unlist(list_of_letter[index]))
}

letter_to_action<-function(letter){
  index <- which(list_of_letter == letter)
  return(unlist(list_of_action[index]))
}


#donne l'id de la sequence associée
find_seqId<- function(str) {
  ids<-get_all_id_seq(ds_studied)
  for (i in ids) {
    print(i)
    if (str == list_of_strings[i]) {
      return(i)
    }
  }
  return (-1)
}

str_to_seq <- function(str) {
  id <- find_seqId(str)
  seq <- subset(ds_final_bis, subset = (sequence_id==id))
  return(seq)
}

#-----------------------------MAIN-----------------------------#

#on cree la liste contenant les sequences sous forme de chaine de caracteres
list_of_strings <- c()
ids<-get_all_id_seq(ds_studied)
for (i in ids){
  list_of_strings[i] <- actions_to_str(seqID_to_actions(i))
}


#ZONE TEST

#print(list_of_strings)
#all_seq<-get_all_id_seq(ds_studied)
#print(str_to_seq("NDEFGGGGGGMO"))
 


