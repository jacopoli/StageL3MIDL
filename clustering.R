library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")
library("stringr")


setwd("c:\\Users\\malat\\OneDrive\\Bureau\\S6\\Stage\\StageL3MIDL")
ds <- read.csv(".\\data_rugby.csv", sep=";", dec=".")

merge_rand <- function(str1, str2, com_subseq){
  
  split1 <- split_string(str1, com_subseq)
  split2 <- split_string(str2, com_subseq)
  
  lgt_left<-str_length(c(split1[1], split2[1]))
  lgt_right<-str_length(c(split1[2], split2[2]))
  
  delta_left<-lgt_left[1]-lgt_left[2]
  delta_right<-lgt_right[1]-lgt_right[2]
  
  tmp_left<-fill_str(split1[1], split2[1], delta_left)
  tmp_right<-fill_str(split1[2], split2[2], delta_right, left=FALSE)
  
  tmp<-reconstruct(tmp_left, tmp_right, com_subseq)
  str1<-tmp[1]
  str2<-tmp[2]
  
  output<-choose_letters(str1, str2)
  output<-str_replace_all(output, " ", "")
  
  return(output)
}

split_string<-function(str1, com_subseq){
  index<-str_locate(str1, com_subseq)
  start<-str_sub(str1, end=index[1]-1)
  end<-str_sub(str1, start=index[2]+1)
  return(c(start, end))
}

choose_letters<-function(str1, str2){
  n<-str_length(str1)
  res<-rbinom(n, 1, .5)
  str1_vec<-str_to_vec(str1)
  str2_vec<-str_to_vec(str2)
  output<-""
  for (i in 1:n) {
    if (res[i]==1){
      output<-str_c(output, str2_vec[i])
    }
    else{
      output<-str_c(output, str1_vec[i])
    }
  }
  return(output)
}

fill_str <-function(str1, str2, delta, left=TRUE){
  if(delta<0){
    str1<-add_blank(str1, -delta, left)
  }
  else{
    str2<-add_blank(str2, delta, left)
  }
  return(c(str1, str2))
}

add_blank<-function(str1, n, left=TRUE){
  blank<-str_dup(" ", n)
  if (left){
     output<-str_c(blank, str1)
  }
  else{
     output<-str_c(str1, blank)
  }
  return(output)
}

reconstruct<-function(left, right, com_subseq){
  str1<-str_c(left[1], com_subseq, right[1])
  str2<-str_c(left[2], com_subseq, right[2])
  return(c(str1, str2))
}

str_to_vec<-function(str1){
  return(unlist(str_split(str1, "")))
}

merge_rand("babbabakkko", "akkkio", "kkk")