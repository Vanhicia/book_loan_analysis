library(ggplot2)
library(plyr)

df_book_Tls = read.csv("dataset_Toulouse.csv", header=TRUE, sep=";")
df_all_Paris = read.csv("dataset_Paris.csv", header=TRUE, sep=";")