library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")
library("stringr")
library("PTXQC")

setwd("/home/sergio/Documents/StageL3MIDL")
ds <- read.csv("data_rugby.csv", sep=";", dec=".")

source("eventlog.R")


#creation d'une liste avec les differentes actions et une liste de meme taille avec des lettres (a la main mdr)
list_of_actions <- unlist(ds_final$actionName)
list_of_actions <- list_of_actions[!duplicated(list_of_actions)]
list_of_letter <- list("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "M", "L", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X")
#fonction qui renvoie une lettre a une action
action_to_letter<-function(action){
  index <- which(list_of_actions == action)
  return(unlist(list_of_letter[index]))
}

letter_to_action<-function(letter){
  index <- which(list_of_letter == letter)
  return(unlist(list_of_actions[index]))
}

#fonction qui renvoie une liste des actions de la sequence en parametre
get_seqsubset <- function(seq){
  actions_of_seq <- c()
  for (l in 1:nrow(ds_final)){
    if (ds_final[l, "sequence_id"] == seq){
      actions_of_seq <- append(actions_of_seq, ds_final[l, "actionName"])
    }
  }
  return(actions_of_seq)
}

#fonction qui renvoie la chaine de caracteres d'une sequence (on applique action_to_letter a chaque action et apres on concatene la lettre a une str)
subset_to_str <- function(subset){
  str <- ""
  for (action in subset){
    letter <- action_to_letter(action)
    str <- paste(str, letter, sep = "")
  }
  return(str)
}

str_to_subset <- function(str){
  letters <- str_to_vec(str)
  actions <- c()
  i <- 1
  for(letter in letters){
    actions[i] = letter_to_action(letter)
    i <- i+1
  }
  return(actions)
}

#on cree la liste contenant les sequences sous forme de chaine de caracteres
list_of_strings <- c()
for (i in 1:max(ds_final$sequence_id)){
  str_i <- get_seqsubset(i)
  list_of_strings <- append(list_of_strings, subset_to_str(str_i))
  
}

#fonction qui retourne la distance entre deux strings et la substring commune la plus longue
distance <- function(string1, string2) {
  seqcom <- LCSn(c(string1, string2))
  return(c(str_length(seqcom[1]), seqcom))
}

source("clustering.R")
merge_rand(a, b, d)  