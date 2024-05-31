library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")
library("stringr")
library("stringdist")
library("DescTools")
library("PTXQC")


#retourne la liste des représentant de chaque cluster. 
#entrée: un dataframe 2 colonnes (les elements et le cluster assigné), N nombre de clusters

get_representants<-function(df, N){
  
  output<-c() #vecteur pour stocker les representants
  
  for (index in 1:N){
    cluster_i <- get_cluster_i(df,index)
    if (length(cluster_i)==1){                 #si un seul element dans le cluster, c'est le representant
      output<-append(output, cluster_i[1]) 
    }
    else{
      mat_dist_cluster<-stringdistmatrix(cluster_i, cluster_i, method = "lcs")  #on calcule les distances des elements du cluster UNIQUEMENT
      output<-append(output, find_representant(mat_dist_cluster, cluster_i)) #on trouve son représentant et on l'ajoute
    }
  }
  return(output)
}

#retourne les elements du Keme cluster. 
#entrée: -un dataframe 2 colonnes (les elements | le cluster qui leur est assigné)
#        -l'indice du cluster
get_cluster_i<-function(df,k){
  colnames(df)<-c("string", "cluster")
  cluster <- subset(df, subset= (cluster==k))
  return(cluster$string)
}

#retourne le nombre d'élément dans le cluster k.
get_size_cluster<-function(df,k){
  return(length(get_cluster_i(df,k)))
}



#fonction qui trouve le représentant d'un cluster à partir de la matrice de distance et du vecteur des différents elements
#on trouve l'element qui a la plus petite distance au carré cumulée.
find_representant <- function(matrix_dist, vec_str){
  row_dist = rowSums(matrix_dist**2)
  ranked_dist<-min_rank(row_dist)
  return(vec_str[which(ranked_dist==1)][1])
}

#fonction qui fusionne N strings à partir de leur séquence commune
merge_strings <- function(vec_str, com_subseq){
  
  splited <- split_string(vec_str, com_subseq)
  
  output<-str_c(splited[1],com_subseq, splited[2])
  
  return(output)
}


#fonction qui retourne la distance entre deux strings et la substring commune la plus longue
distance <- function(string1, string2) {
  seqcom <- LCSn(c(string1, string2))
  return(c(str_length(seqcom[1]), seqcom))
}

merge_from_subseq <- function(str1, str2, subseq){
  pos_str1 = get_pos_subseq(str_to_vec(str1), str_to_vec(subseq))
  pos_str2 = get_pos_subseq(str_to_vec(str2), str_to_vec(subseq))
  subset_str1<-get_subsets(str1, pos_str1)
  subset_str2<-get_subsets(str2, pos_str2)
  tmp<-c()
  tmp <- StrAlign(c(subset_str1[1], subset_str2[1]), sep="\\r")
  for (k in 2:(length(subset_str1))){
    tmp=append(tmp,StrAlign(c(subset_str1[k], subset_str2[k]), sep="\\l"))
  }
  output<-c()
  for (k in 1:(length(tmp)/2)){
    output=append(output,choose_letters(tmp[2*k-1], tmp[2*k]))
  }
  output<-str_replace_all(output, " ", "")
  output <- paste(output, collapse="")
  return(output)
}

#retourne le 1er indice d'apparition de la sous sequence (subseq) dans str1
get_pos_subseq<- function(str1, subseq){
  output_pos = c()
  i=1
  j=1
  while (i<=length(str1) && j<=length(subseq)){
    if (str1[i]==subseq[j]){
      output_pos[j]=i
      j=j+1
    }
    i=i+1
  }
  return(output_pos)
}

#retourne la chaine str1 divisée 
get_subsets <- function(str1, pos_subseq){
  output<-c()
  output[1]<-str_sub(str1, end=pos_subseq[1]-1)
  for (i in 1:(length(pos_subseq)-1)){
    output[i+1]<-str_sub(str1, start=pos_subseq[i], end=pos_subseq[i+1]-1)
  }
  output[length(pos_subseq)+1]<-str_sub(str1, start=pos_subseq[length(pos_subseq)])
  return(output)
}

#fonction qui divise tous les mots en 2 sans prendre la partie commune, et qui les fusionne en 1
split_string<-function(vec_str, com_subseq){
  start<-c()
  end<-c()
  index<-str_locate(vec_str, com_subseq)
  for (i in 1:length(vec_str)){
    start<-append(start, str_sub(vec_str[i], end=index[i,1]-1))
    end<-append(end,str_sub(vec_str[i], start=index[i,2]+1))
  }
  start<-StrAlign(start, sep="\\r")
  end<-StrAlign(end, sep="\\l")
  start<-choose_letters(start)
  end<-choose_letters(end)
  return(c(start, end))
}

#fonction qui forme un mot au hasard à partir des mots entrés
choose_letters<-function(vec_str){
  n<-length(vec_str)
  k<-str_length(vec_str[1])
  res<-sample(x=1:n, size=k, replace=TRUE)
  output<-""
  for (i in 1:k) {
    output<-append(output, str_sub(vec_str[res[i]], start=i, end=i))
  }
  output<-str_replace_all(output, " ", "")
  output <- paste(output, collapse="")
  return(output)
}

str_to_vec<-function(str1){
  return(unlist(str_split(str1, "")))
}



