library(ggplot2)
library(plyr)
library(reshape2)
library(fmsb)
library(stringr)

df <- read.csv("dataset_Toulouse.csv",  fileEncoding="UTF-8-BOM", header=TRUE, sep=";")


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

df <- rename(df, c(ANNEE = "Année", 
                                     Nbre.de.prêts = "Nb_prêts", 
                                     TITRE = "Titre", 
                                     AUTEUR = "Auteur", 
                                     Cat.1 = "Cat1", 
                                     Cat.2 = "Cat2"))

# Nettoyage des noms des auteurs : supprimer infos sur date de naissance entre parenthèses
df$Auteur <- gsub("[(].*[)]","",df$Auteur)


# Nettoyage des éditeurs
# On enlève les adresses et pays des éditeurs
df$Editeur <- gsub(" ?[(].+[)] ?","",df$Editeur)

# On nettoie la colonne des éditeurs et harmonise les notations
df$Editeur <- gsub("[,][ ][0-9]{4}-?","",df$Editeur)
df$Editeur <- gsub("[.]","",df$Editeur)
df$Editeur <- gsub("[]],","] ;",df$Editeur)
df$Editeur <- gsub("[ ],"," ;",df$Editeur)
df$Editeur <- gsub(".+:","",df$Editeur)
df$Editeur <- trimws(df$Editeur, which = c("right"))
df$Editeur <- trimws(df$Editeur, which = c("left"))


# Renommage et fusion des catégories similaires ----------------------

# Renommage des catégories 1
df$Cat1 <- as.character(df$Cat1)
df$Cat1[df$Cat1=="A"] <- "Adulte"

# Bébé (BB) + Très petit (TP) + Enfant (E) devient Enfant (E)
df$Cat1[df$Cat1 %in% c("BB", "TP", "E")] <- "Enfant"

df$Cat1 <- as.factor(df$Cat1)


# Renommage des catégories 2
df$Cat2 <- as.character(df$Cat2)

# Livre (LIV) + Livre (LV) devient Livre
df$Cat2[df$Cat2 %in% c("LIV", "LV")] <- "Livre"

# CD/DVD ROM (CDVDROM) devient CD
df$Cat2[df$Cat2 %in% c("CDVDROM")] <- "CD"

# Livre CD (LIVCD) + Livre CD/DVD ROM (LIVCDVDR) devient Livre CD
df$Cat2[df$Cat2 %in% c("LIVCD", "LIVCDVDR")] <- "Livre CD"

# Roman (ROM) + E-book (TE) devient Roman (car le seul E-book du dataset correspond à un roman)
df$Cat2[df$Cat2 %in% c("ROM", "TE")] <- "Roman"

# ALB devient Album
df$Cat2[df$Cat2 == "ALB"] <- "Album"

# METLAN devient Langue
df$Cat2[df$Cat2 == "METLAN"] <- "Langue"

# LIVDOC devient Livre Documentaire
df$Cat2[df$Cat2 == "LIVDOC"] <- "Livre Documentaire"

# CONTE devient Conte
df$Cat2[df$Cat2 == "CONTE"] <- "Conte"

# PERIO devient Periodique
df$Cat2[df$Cat2 == "PERIO"] <- "Périodique"

# POL devient Polar
df$Cat2[df$Cat2 == "POL"] <- "Polar"

# SF devient Science Fiction
df$Cat2[df$Cat2 == "SF"] <- "Science Fiction"

df$Cat2 <- as.factor(df$Cat2)



# Suppression des lignes vides -----
df <- na.omit(df) 


# Plot du nombre de prêts par catégorie 1 au cours des années ----------
df_total_par_cat1_Tls <- ddply(df, .(Année,Cat1), summarize, Total_nbre_prêts=sum(Nb_prêts))

# On enleve les lignes sans catégorie 1
df_total_par_cat1_Tls <- df_total_par_cat1_Tls[df_total_par_cat1_Tls$Cat1 !="",]

# Plot dataframe 
# Ouvre le fichier pdf
pdf("df_total_par_cat1_Tls.pdf", width=7,height=4)
print(ggplot(df_total_par_cat1_Tls, aes(x=Année, y=Total_nbre_prêts, fill=Cat1)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ggtitle("Evolution du nombre de prêts par section (adulte/enfant)") +
  scale_fill_manual("Section", values=c("#999999", "#E69F00", "#56B4E9")) +  ylab("Nombre de prêts"))
# Ferme le ficher pdf
dev.off()   

# Plot du nombre de prêts par catégorie au cours des années ----------
df_total_par_cat_Tls <- ddply(df, .(Année,Cat1,Cat2), summarize, Total_nbre_prêts=sum(Nb_prêts))

# On enleve les lignes sans catégorie 1
df_total_par_cat_Tls <- df_total_par_cat_Tls[df_total_par_cat_Tls$Cat1 !="",]

# Plot dataframe 
# Ouvre le fichier pdf
pdf("df_total_par_cat_Tls.pdf", width=7,height=4)
print(ggplot(df_total_par_cat_Tls, aes(x=Année, y=Total_nbre_prêts, color=Cat2)) +
  geom_line() + facet_grid(.~Cat1) + labs(color="Type") +
  ggtitle("Evolution du nombre de prêts pour chaque type d'imprimés") +  ylab("Nombre de prêts"))
# Ferme le ficher pdf
dev.off() 

# On garde que les emprunts de 2018
df_2018 <- df[df$Année == "2018",] 

# Pour 2018, top 10 des auteurs
# On calcule le nombre de prêt par année et auteur
df_2018_auteurs <- ddply(df_2018, .(Auteur), summarize, Nb_prêts=sum(Nb_prêts))

# On enlève les auteurs vides
df_2018_auteurs <- df_2018_auteurs[df_2018_auteurs$Auteur != "-",]

# On enlève les espaces inutiles dans les noms des auteurs
df_2018_auteurs$Auteur <- trimws(df_2018_auteurs$Auteur, which = c("right"))

# On fusionne les lignes ayant les mêmes noms d'auteurs
df_2018_auteurs <- ddply(df_2018_auteurs, .(Auteur), summarize, Nb_prêts=sum(Nb_prêts))

# On récupère le top 10
df_2018_top10_auteurs <- head(df_2018_auteurs[order(df_2018_auteurs$Nb_prêts, decreasing = TRUE),],10)

# On l'affiche
# Ouvre le fichier pdf
pdf("df_2018_top10_auteurs.pdf", width=6,height=4)
print(ggplot(df_2018_top10_auteurs, aes(x= reorder(Auteur, Nb_prêts), y=Nb_prêts)) + 
  geom_bar(stat="identity", show.legend = FALSE) + coord_flip() +
  ggtitle("Top 10 des auteurs  en 2018") + xlab("Auteur") + ylab("Nombre de prêts"))
# Ferme le ficher pdf
dev.off() 

# Pour 2018 pour chaque éditeur, le nombre d'imprimés par type -------


# On trie par éditeur et type d'imprimés
df_2018_editeur_type <- ddply(df_2018, .(Editeur), function(x){
  table(x$Cat2)
}) 

# On enlève les colonnes vides
df_2018_editeur_type <- df_2018_editeur_type[, colSums(df_2018_editeur_type != 0) > 0]

# On remplace les noms des lignes par le nom des éditeurs et on enlève la colonne correspondante
df_2018_editeur_type_with_rownames <- data.frame(df_2018_editeur_type[,-1], row.names=df_2018_editeur_type[,1])

# On ajoute le nombre d'imprimés  pour effectuer le top 5 après
df_2018_top5_editeur <- df_2018_editeur_type_with_rownames
df_2018_top5_editeur$Nb_imprimés <- apply(df_2018_top5_editeur,1,sum)

# On détermine les bornes du graphe radar
value_max = max(df_2018_editeur_type_with_rownames, na.rm=TRUE)
times = nrow(df_2018_editeur_type)

# Plot le graphe radar
# Ouvre le fichier pdf
pdf("df_2018_editeur.pdf")
radarchart(rbind(rep(value_max,times) , rep(0,times) , df_2018_editeur_type_with_rownames), title="Nombre d'imprimés par catégorie pour chaque éditeur en 2018")
# Ferme le ficher pdf
dev.off() 


# TOP 5 des éditeurs en 2018 ----------------------------------------
# On remet la colonne Editeur
df_2018_top5_editeur <- tibble::rownames_to_column(df_2018_top5_editeur, "Editeur")

# On garde les 5 éditeurs dont les imprimés ont été les plus empruntés
df_2018_top5_editeur <- head(df_2018_top5_editeur[order(df_2018_top5_editeur$Nb_imprimés, decreasing = TRUE),],5)

# On remplace les noms des lignes par le nom des éditeurs et on enlève la colonne correspondante
df_2018_top5_editeur <- data.frame(df_2018_top5_editeur[,-1], row.names=df_2018_top5_editeur[,1])

# On enlève la colonne du nombre d'imprimés
df_2018_top5_editeur$Nb_imprimés <- NULL

# On détermine les bornes du graphe radar
value_max = max(df_2018_top5_editeur, na.rm=TRUE)
times = nrow(df_2018_top5_editeur)

# On ajoute les bornes dans le dataframe et on l'affiche
# Ouvre le fichier pdf
pdf("df_2018_top5_editeur.pdf")
layout(matrix(1:6, ncol=3)) 
lapply(1:5, function(i) { 
  radarchart(rbind(rep(value_max,times) , rep(0,times) , df_2018_top5_editeur[i,-1]), title = rownames(df_2018_top5_editeur)[i])
}) 
# Ferme le ficher pdf
dev.off() 

# Chaque année pour un éditeur, le nombre d'imprimés par type -------
df_flammarion <- df[df$Editeur=="Flammarion",] 
df_flammarion_type <- ddply(df_flammarion, .(Année), function(x){
  table(x$Cat2)
}) 

df_flammarion_prêts <- df_flammarion[ ,c("Année", "Editeur","Cat2","Nb_prêts")]
df_flammarion_prêts <- ddply(df_flammarion_prêts, .(Année, Editeur, Cat2), summarize, Nb_prêts=sum(Nb_prêts))

# On enlève les colonnes vides
df_flammarion_type <- df_flammarion_type[, colSums(df_flammarion_type != 0) > 0]

# On décompose la dataframe
df_flammarion_type <- melt(df_flammarion_type, id.vars="Année", variable.name = "Type", value.name = "Nombre")

# Plop dataframe
# Type
# Ouvre le fichier pdf
pdf("df_flammarion_type.pdf", width=8,height=4)
print(ggplot(df_flammarion_type, aes(Année,Nombre, col=Type)) + 
  geom_line() + geom_point() +
  ggtitle("Evolution du nombre d'imprimés par catégorie pour l'éditeur Flammarion") + xlab("Année") + ylab("Nombre d'imprimés"))
# Ferme le ficher pdf
dev.off() 

# Nombre de prêts
# Ouvre le fichier pdf
pdf("df_flammarion_prêts.pdf", width=8,height=4)
print(ggplot(df_flammarion_prêts, aes(Année,Nb_prêts, col=Cat2)) + 
        geom_line() + geom_point() + labs(color="Type") +
        ggtitle("Evolution du nombre de prêts par catégorie pour l'éditeur Flammarion") + xlab("Année") + ylab("Nombre d'imprimés"))
# Ferme le ficher pdf
dev.off() 