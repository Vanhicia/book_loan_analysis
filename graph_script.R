library(ggplot2)
library(plyr)
library(reshape2)
library(fmsb)
library(stringr)

df_book_Tls <- read.csv("dataset_Toulouse.csv",  fileEncoding="UTF-8-BOM", header=TRUE, sep=";")
df_all_Paris <- read.csv("dataset_Paris.csv",  fileEncoding="UTF-8-BOM", header=TRUE, sep=";")

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


# Renommage des colonnes du dataframe --------------------------------

df_book_Tls <- rename(df_book_Tls, c(ANNEE = "Année", 
                                     Nbre.de.prêts = "Nb_prêts", 
                                     TITRE = "Titre", 
                                     AUTEUR = "Auteur", 
                                     Cat.1 = "Cat1", 
                                     Cat.2 = "Cat2"))


# Nettoyage des noms des auteurs : suppr infos sur date de naissance entre parenthèses
df_book_Tls$Auteur <- gsub("[(].*[)]","",df_book_Tls$Auteur)


# Nettoyage des éditeurs : suppr infos sur l'année de l'édition
#substr(x, nchar(x), nchar(x)-5)

#df_book_Tls$Année_édition <- sub(".+(?=([,][ ][1|2][0|9][0-9]{2}))","",df_book_Tls$Editeur)
df_book_Tls$Editeur <- gsub("[,][ ][0-9]{4}-?","",df_book_Tls$Editeur)


# Renommage et fusion des catégories similaires ----------------------

# Bébé (BB) + Très petit (TP) + Enfant (E) devient Enfant (E)
df_book_Tls$Cat1[df_book_Tls$Cat1 %in% c("BB", "TP", "E")] <- "E"

# Livre (LIV) + Livre (LV) devient Livre (LIV)
df_book_Tls$Cat2[df_book_Tls$Cat2 %in% c("LIV", "LV")] <- "LIV"

# CD/DVD ROM (CDVDROM) devient CD (CD)
df_book_Tls$Cat2[df_book_Tls$Cat2 %in% c("CDVDROM")] <- "CD"

# Livre CD (LIVCD) + Livre CD/DVD ROM (LIVCDVDR) devient Livre CD (LIVCD)
df_book_Tls$Cat2[df_book_Tls$Cat2 %in% c("LIVCD", "LIVCDVDR")] <- "LIVCD"

# Roman (ROM) + ??? (TE) devient ROM (ROM)
df_book_Tls$Cat2[df_book_Tls$Cat2 %in% c("ROM", "TE")] <- "ROM"


# Suppression des lignes vides -----
df_book_Tls <- na.omit(df_book_Tls) 


# Plot du nombre de prêts par catégorie au cours des années ----------

# On enleve les lignes sans catégorie 1
df_book_Tls <- df_book_Tls[df_book_Tls$Cat1 !="",]
df_total_par_cat_Tls <- ddply(df_book_Tls, .(Année,Cat1,Cat2), summarize, Total.nbre.prêts=sum(Nb_prêts))

# Plot dataframe 
ggplot(df_total_par_cat_Tls, aes(x=Année, y=Total.nbre.prêts, color=Cat2)) + geom_line() + facet_grid(.~Cat1)

# Top 10 des auteurs
auteurs <- ddply(df_book_Tls, .(Auteur,Année), summarize, Nb.prêts=sum(Nb_prêts))
auteurs <- auteurs[auteurs$Auteur != "-",]
auteurs2018 <- auteurs[auteurs$Année == 2018,]
top10auteurs2018 <- head(auteurs2018[order(auteurs2018$Nb.prêts, decreasing = TRUE),],10)
grepl("Sobral, Patrick", top10auteurs2018$Auteur)
ggplot(top10auteurs2018, aes(x=Auteur, y=Nb.prêts)) + geom_bar(stat="identity", show.legend = FALSE) + coord_flip() 
# TODO : parvenir à rassembler les lignes avec le m$eme auteur


# TODO : imprimé récent ou ancien ? faire comparaison entre année d'emprunt et l'année de l'édition


# Pour 2018 pour chaque éditeur, le nombre d'imprimés par type -------

# On garde que les emprunts de 2018
df_2018 <- df_book_Tls[df_book_Tls$Année=="2018",] 

# On trie par éditeur et type d'imprimés
df_2018_editeur <- df_2018[,c("Editeur","Cat2")]
df_2018_editeur <- ddply(df_2018_editeur, .(Editeur), function(x){
  table(x$Cat2)
}) 

# On enlève les colonnes vides
df_2018_editeur <- df_2018_editeur[, colSums(df_2018_editeur != 0) > 0]

# On remplace les noms des lignes par le nom des éditeurs et on enlève la colonne correspondante
df_2018_editeur_with_rownames <- data.frame(df_2018_editeur[,-1], row.names=df_2018_editeur[,1])

# On détermine les bornes du graphe radar
value_max = max(df_2018_editeur_with_rownames, na.rm=TRUE)
times = length(df_2018_editeur$Editeur)

# On ajoute les bornes dans le dataframe
df_2018_editeur_with_rownames <- rbind(rep(value_max,times) , rep(0,times) , df_2018_editeur_with_rownames)

# Plot le graphe radar
radarchart(df_2018_editeur_with_rownames)






# Chaque année pour un éditeur, le nombre d'imprimés par type -------
flammarion <- df_book_Tls[df_book_Tls$Editeur=="Paris : Flammarion",] 
df_editeur <- ddply(flammarion, .(Année), function(x){
  table(x$Cat2)
}) 

# On enlève les colonnes vides
df_editeur <- df_editeur[, colSums(df_editeur != 0) > 0]

# Plop dataframe
df_editeur <- melt(df_editeur, id.vars="Année", variable.name = "Type", value.name = "Nombre")
ggplot(df_editeur, aes(Année,Nombre, col=Type)) + geom_line() + geom_point()

