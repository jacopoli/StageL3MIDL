{
library("tidyverse")
library("ggplot2")
library("dplyr")
library("bupaverse")
library("lubridate")
library("stringr")
library("PTXQC")

setwd("/home/sergio/Documents/StageL3MIDL")
ds <- read.csv("data_rugby.csv", sep=";", dec=".")

source("seq_to_lettre.r")
source("clustering.R")
}

# generation de sequences
{
sequence_1 <- get_seqsubset(1)
sequence_2 <- get_seqsubset(2)
sequence_3 <- get_seqsubset(3)
sequence_4 <- get_seqsubset(4)

print(sequence_1)
print("--------------------------------------------------------------------------------------------------------------------------")
print(sequence_2)
print("--------------------------------------------------------------------------------------------------------------------------")
print(sequence_3)
print("--------------------------------------------------------------------------------------------------------------------------")
print(sequence_4)
}

# generation des str correspondant aux sequences
{
str_1 <- subset_to_str(sequence_1)
str_2 <- subset_to_str(sequence_2)
str_3 <- subset_to_str(sequence_3)
str_4 <- subset_to_str(sequence_4)

print(str_1)
print(str_2)
print(str_3)
print(str_4)
}

# distance entre seq1 et les autres seq
{
dist1_2 <- distance(str_1, str_2)
dist1_3 <- distance(str_1, str_3)
dist1_4 <- distance(str_1, str_4)

print(dist1_2)
print(dist1_3)
print(dist1_4)
}
{
print(str_to_subset(dist1_2[2]))
print(str_to_subset(dist1_3[2]))
print(str_to_subset(dist1_4[2]))
}

# on applique la fonction merge
{
str_merge_12 <- merge_rand(str_1, str_2, dist1_2[2])
str_merge_13 <- merge_rand(str_1, str_3, dist1_3[2])
str_merge_14 <- merge_rand(str_1, str_4, dist1_4[2])

print(str_merge_12)
print(str_merge_13)
print(str_merge_14)
}
{
print(str_to_subset(str_merge_12))
print("--------------------------------------------------------------------------------------------------------------------------")
print(str_to_subset(str_merge_13))
print("--------------------------------------------------------------------------------------------------------------------------")
print(str_to_subset(str_merge_14))
}

