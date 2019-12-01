library(ggplot2)
library(plyr)
library(reshape2)
library(fmsb)
library(stringr)

df_book_Tls <- read.csv("dataset_Toulouse.csv",  fileEncoding="UTF-8-BOM", header=TRUE, sep=";")

# ============================= TOULOUSE ============================ #

# Composition du dataset ---------------------------------------------

# Année 
# Nb_prêts 
# Titre 
# Auteur 
# Editeur 
# Indice 
# Bib
# Côte
# Cat1 : Indique si l'imprimé est pour les enfants (E) ou adultes (A)
# Cat2 : Indique son type : Album (ALB), Livre (LIV), CD (CD), Polar (POL), etc. 


# Renommage des colonnes et des valeurs du dataframe --------------------------------

df_book_Tls <- rename(df_book_Tls, c(ANNEE = "Année", 
                                     Nbre.de.prêts = "Nb_prêts", 
                                     TITRE = "Titre", 
                                     AUTEUR = "Auteur", 
                                     Cat.1 = "Cat1", 
                                     Cat.2 = "Cat2"))

# Nettoyage des noms des auteurs : suppr infos sur date de naissance entre parenthèses
df_book_Tls$Auteur <- gsub("[(].*[)]","",df_book_Tls$Auteur)


# Nettoyage des éditeurs
# On enlève les adresses et pays des éditeurs
df_book_Tls$Editeur <- gsub(" ?[(].+[)] ?","",df_book_Tls$Editeur)

# On nettoie la colonne des éditeurs et harmonise les notations
df_book_Tls$Editeur <- gsub("[,][ ][0-9]{4}-?","",df_book_Tls$Editeur)
df_book_Tls$Editeur <- gsub("[.]","",df_book_Tls$Editeur)
df_book_Tls$Editeur <- gsub("[]],","] ;",df_book_Tls$Editeur)
df_book_Tls$Editeur <- gsub("[ ],"," ;",df_book_Tls$Editeur)



# Renommage et fusion des catégories similaires ----------------------

# Bébé (BB) + Très petit (TP) + Enfant (E) devient Enfant (E)
df_book_Tls$Cat1[df_book_Tls$Cat1 %in% c("BB", "TP", "E")] <- "E"

# Livre (LIV) + Livre (LV) devient Livre (LIV)
df_book_Tls$Cat2[df_book_Tls$Cat2 %in% c("LIV", "LV")] <- "LIV"

# CD/DVD ROM (CDVDROM) devient CD (CD)
df_book_Tls$Cat2[df_book_Tls$Cat2 %in% c("CDVDROM")] <- "CD"

# Livre CD (LIVCD) + Livre CD/DVD ROM (LIVCDVDR) devient Livre CD (LIVCD)
df_book_Tls$Cat2[df_book_Tls$Cat2 %in% c("LIVCD", "LIVCDVDR")] <- "LIVCD"

# Roman (ROM) + E-book (TE) devient Roman (ROM)
df_book_Tls$Cat2[df_book_Tls$Cat2 %in% c("ROM", "TE")] <- "ROM"

# Renommage des catégories 1
df_book_Tls$Cat1 <- as.character(df_book_Tls$Cat1)
df_book_Tls$Cat1[df_book_Tls$Cat1=="A"] <- "Adulte"
df_book_Tls$Cat1[df_book_Tls$Cat1=="E"] <- "Enfant"
df_book_Tls$Cat1 <- as.factor(df_book_Tls$Cat1)


# Suppression des lignes vides -----
df_book_Tls <- na.omit(df_book_Tls) 


# Plot du nombre de prêts par catégorie au cours des années ----------
df_total_par_cat_Tls <- ddply(df_book_Tls, .(Année,Cat1,Cat2), summarize, Total.nbre.prêts=sum(Nb_prêts))

# On enleve les lignes sans catégorie 1
df_total_par_cat_Tls <- df_total_par_cat_Tls[df_total_par_cat_Tls$Cat1 !="",]

# Plot dataframe 
ggplot(df_total_par_cat_Tls, aes(x=Année, y=Total.nbre.prêts, color=Cat2)) + geom_line() + facet_grid(.~Cat1)

# Top 10 des auteurs
# On calcule le nombre de prêt par année et auteur
auteurs <- ddply(df_book_Tls, .(Auteur,Année), summarize, Nb_prêts=sum(Nb_prêts))

# On garde les auteurs de 2018
auteurs <- auteurs[auteurs$Auteur != "-",]
auteurs2018 <- auteurs[auteurs$Année == 2018,]

# On enlève les espaces inutiles dans les noms des auteurs
auteurs2018$Auteur <- trimws(auteurs2018$Auteur, which = c("right"))

# On fusionne les lignes ayant les mêmes noms d'auteurs
auteurs2018 <- ddply(auteurs2018, .(Auteur,Année), summarize, Nb_prêts=sum(Nb_prêts))

# On récupère le top 10
top10auteurs2018 <- head(auteurs2018[order(auteurs2018$Nb_prêts, decreasing = TRUE),],10)

# On l'affiche
ggplot(top10auteurs2018, aes(x=Auteur, y=Nb_prêts)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() 



# Pour 2018 pour chaque éditeur, le nombre d'imprimés par type -------
# On garde que les emprunts de 2018
df_2018 <- df_book_Tls[df_book_Tls$Année == "2018",] 

# On trie par éditeur et type d'imprimés
df_2018_editeur <- df_2018[,c("Editeur","Cat2")]
df_2018_editeur <- ddply(df_2018_editeur, .(Editeur), function(x){
  table(x$Cat2)
}) 

# On enlève les colonnes vides
df_2018_editeur <- df_2018_editeur[, colSums(df_2018_editeur != 0) > 0]

# On remplace les noms des lignes par le nom des éditeurs et on enlève la colonne correspondante
df_2018_editeur_with_rownames <- data.frame(df_2018_editeur[,-1], row.names=df_2018_editeur[,1])

# On ajoute le nombre de prêts pour effectuer le top 5 après
df_top_editeur2018 <- df_2018_editeur_with_rownames
df_top_editeur2018$Nb_prêts <- apply(df_top_editeur2018,1,sum)

# On détermine les bornes du graphe radar
value_max = max(df_2018_editeur_with_rownames, na.rm=TRUE)
times = nrow(df_2018_editeur)

# Plot le graphe radar
radarchart(rbind(rep(value_max,times) , rep(0,times) , df_2018_editeur_with_rownames))



# TOP 5 des éditeurs en 2018 ----------------------------------------
# On remet la colonne Editeur
df_top_editeur2018 <- tibble::rownames_to_column(df_top_editeur2018, "Editeur")

# On garde les 5 éditeurs dont les imprimés ont été le plus empruntés
df_top_editeur2018 <- head(df_top_editeur2018[order(df_top_editeur2018$Nb_prêts, decreasing = TRUE),],5)

# On remplace les noms des lignes par le nom des éditeurs et on enlève la colonne correspondante
df_top_editeur2018 <- data.frame(df_top_editeur2018[,-1], row.names=df_top_editeur2018[,1])

# On enlève la colonne du nombre de prêts
df_top_editeur2018$Nb_prêts <- NULL

# On détermine les bornes du graphe radar
value_max = max(df_top_editeur2018, na.rm=TRUE)
times = nrow(df_top_editeur2018)

# On ajoute les bornes dans le dataframe et on l'affiche
layout(matrix(1:6, ncol=3)) 
lapply(1:5, function(i) { 
  radarchart(rbind(rep(value_max,times) , rep(0,times) , df_top_editeur2018[i,-1]))
})


# Chaque année pour un éditeur, le nombre d'imprimés par type -------
flammarion <- df_book_Tls[df_book_Tls$Editeur=="Paris : Flammarion",] 
df_un_editeur <- ddply(flammarion, .(Année), function(x){
  table(x$Cat2)
}) 

# On enlève les colonnes vides
df_un_editeur <- df_un_editeur[, colSums(df_un_editeur != 0) > 0]

# Plop dataframe
df_un_editeur <- melt(df_un_editeur, id.vars="Année", variable.name = "Type", value.name = "Nombre")
ggplot(df_un_editeur, aes(Année,Nombre, col=Type)) + geom_line() + geom_point()

