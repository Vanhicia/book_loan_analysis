library(ggplot2)
#library(plyr)
library(dplyr)

df_book_Tls = read.csv("dataset_Toulouse.csv", header=TRUE, sep=";")
df_all_Paris = read.csv("dataset_Paris.csv", header=TRUE, sep=";")

#Toulouse

# Nettoyage :
df_book_Tls = rename(df_book_Tls, Année=ANNEE, Titre=TITRE)

# rassembler les catégories similaires

# rassembler les catégries "enfant" (BB, TP, E)
df_book_Tls$Cat.1[df_book_Tls$Cat.1 %in% c("BB", "TP", "E")] <- "E"
#df_book_Tls$Cat.1[df_book_Tls$Cat.1 %in% c("A")] <- "Adulte"

#df_book_Tls$Cat.2[df_book_Tls$Cat.2 %in% c("ALB")] <- "ALBUM"
df_book_Tls$Cat.2[df_book_Tls$Cat.2 %in% c("LIV", "LV")] <- "LIV"
df_book_Tls$Cat.2[df_book_Tls$Cat.2 %in% c("CDVDROM")] <- "CD"
#df_book_Tls$Cat.2[df_book_Tls$Cat.2 %in% c("LIVDOC")] <- "LIVRE DOC"
df_book_Tls$Cat.2[df_book_Tls$Cat.2 %in% c("LIVCD", "LIVCDVDR")] <- "LIVCD"
#df_book_Tls$Cat.2[df_book_Tls$Cat.2 %in% c("POL")] <- "POLAR"
df_book_Tls$Cat.2[df_book_Tls$Cat.2 %in% c("ROM", "TE")] <- "ROM"
#df_book_Tls$Cat.2[df_book_Tls$Cat.2 %in% c("SF")] <- "SCIENCE FICTION"

# supprimer les lignes où le nombre de prêts n'est pas indiqué
df_book_Tls = na.omit(df_book_Tls) 

# plot l'évolution du nombre de prêts par catégorie au cours des années
df_total_par_cat_Tls = ddply(df_book_Tls, .(Année,Cat.1,Cat.2), summarize, Total.nbre.prêts=sum(Nbre.de.prêts))
ggplot(df_total_par_cat_Tls, aes(x=Année, y=Total.nbre.prêts, color=Cat.2)) + geom_line() + facet_grid(.~Cat.1)