library(ggplot2)
library(plyr)
library(reshape2)

df_book_Tls <- read.csv("dataset_Toulouse.csv", header=TRUE, sep=";")
df_all_Paris <- read.csv("dataset_Paris.csv", header=TRUE, sep=";")

#Toulouse

# Nettoyage :
df_book_Tls <- rename(df_book_Tls, c(ANNEE="Année", TITRE="Titre", AUTEUR="Auteur"))

# nettoyer noms des auteurs : suppr infos sur date de naissance entre parenthèses
df_book_Tls$Auteur <- gsub("[(].*[)]","",df_book_Tls$Auteur)

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
df_book_Tls <- na.omit(df_book_Tls) 

# plot l'évolution du nombre de prêts par catégorie au cours des années
df_total_par_cat_Tls <- ddply(df_book_Tls, .(Année,Cat.1,Cat.2), summarize, Total.nbre.prêts=sum(Nbre.de.prêts))
#TODO : enlever lignes sans catégorie 1
ggplot(df_total_par_cat_Tls, aes(x=Année, y=Total.nbre.prêts, color=Cat.2)) + geom_line() + facet_grid(.~Cat.1)

# top 10 des auteurs
auteurs <- ddply(df_book_Tls, .(Auteur,Année), summarize, Nb.prêts=sum(Nbre.de.prêts))
auteurs <- auteurs[auteurs$Auteur != "-",]
auteurs2018 <- auteurs[auteurs$Année == 2018,]
top10auteurs2018 <- head(auteurs2018[order(auteurs2018$Nb.prêts, decreasing = TRUE),],10)
grepl("Sobral, Patrick", top10auteurs2018$Auteur)
ggplot(top10auteurs2018, aes(x=Auteur, y=Nb.prêts)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() 
# TODO : parvenir à rassembler les lignes avec le m$eme auteur
