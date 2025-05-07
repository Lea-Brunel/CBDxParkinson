setwd("")

data <- read.csv('TOTAL_results_sondage_clean_accents.csv', header=T, sep = ";")

# séparer les données entre consommateurs et non consommateurs 
data_consommateurs <- data[data$Etes.vous.un.consommateur.régulier.de.CBD.sans.THC=='Oui',] #270 personnes
data_non_consommateurs <- data[data$Etes.vous.un.consommateur.régulier.de.CBD.sans.THC=='Non',] # 26 personnes

############################################
#     TRANSFORMER EN MATRICE BINAIRE       #
############################################
# télécharger les librairies nécessaires
library(dplyr)
library(tidyr)

######## partie NON CONSOMMATEURS ########

# Adjectifs pour décrire les consommateurs de CBD

# transformation des données : remplir une matrice binaire des adjectifs 

# Étape 1: Transformer les données en format long
data_long <- data_non_consommateurs %>%
  mutate(id_row = row_number()) %>%
  separate_rows(Quels.sont.les.adjectifs.qui.décrivent.le.mieux.la.façon.dont.vous.voyez.les.consommateurs.de.CBD.sans.THC....en.comparaison.avec.des.personnes.qui.n.en.consomment.pas., sep = ";") %>%
  mutate(Quels.sont.les.adjectifs.qui.décrivent.le.mieux.la.façon.dont.vous.voyez.les.consommateurs.de.CBD.sans.THC....en.comparaison.avec.des.personnes.qui.n.en.consomment.pas. = trimws(Quels.sont.les.adjectifs.qui.décrivent.le.mieux.la.façon.dont.vous.voyez.les.consommateurs.de.CBD.sans.THC....en.comparaison.avec.des.personnes.qui.n.en.consomment.pas.))  # Enlever les espaces en début/fin

# Étape 2: Créer une matrice binaire
data_binaire <- data_long %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = c(id_row, names(data_non_consommateurs)[names(data_non_consommateurs) != "Quels.sont.les.adjectifs.qui.décrivent.le.mieux.la.façon.dont.vous.voyez.les.consommateurs.de.CBD.sans.THC....en.comparaison.avec.des.personnes.qui.n.en.consomment.pas."]),
    names_from = Quels.sont.les.adjectifs.qui.décrivent.le.mieux.la.façon.dont.vous.voyez.les.consommateurs.de.CBD.sans.THC....en.comparaison.avec.des.personnes.qui.n.en.consomment.pas.,
    values_from = present,
    values_fill = 0
  ) %>%
  select(-id_row)


######## partie CONSOMMATEURS ########

# forme de consommation

data_long <- data_consommateurs %>%
  mutate(id_row = row_number()) %>%
  separate_rows(Sous.quelle.s..forme.s..consommez.vous.du.CBD.sans.THC.., sep = ";") %>%
  mutate(Sous.quelle.s..forme.s..consommez.vous.du.CBD.sans.THC.. = trimws(Sous.quelle.s..forme.s..consommez.vous.du.CBD.sans.THC..))  # Enlever les espaces en début/fin

data_intermediataire <- data_long %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = c(id_row, names(data_consommateurs)[names(data_consommateurs) != "Sous.quelle.s..forme.s..consommez.vous.du.CBD.sans.THC.."]),
    names_from = Sous.quelle.s..forme.s..consommez.vous.du.CBD.sans.THC..,
    values_from = present,
    values_fill = 0
  ) %>%
  select(-id_row)

# raisons d'avoir commencé 

data_long <- data_intermediataire %>%
  mutate(id_row = row_number()) %>%
  separate_rows(Pour.quelle.s..raison.s..avez.vous.commencé.à.en.consommer.., sep = ";") %>%
  mutate(Pour.quelle.s..raison.s..avez.vous.commencé.à.en.consommer.. = trimws(Pour.quelle.s..raison.s..avez.vous.commencé.à.en.consommer..))  

data_intermediataire2 <- data_long %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = c(id_row, names(data_intermediataire)[names(data_intermediataire) != "Pour.quelle.s..raison.s..avez.vous.commencé.à.en.consommer.."]),
    names_from = Pour.quelle.s..raison.s..avez.vous.commencé.à.en.consommer..,
    values_from = present,
    values_fill = 0
  ) %>%
  select(-id_row)

# impact sur la vie sociale

data_long <- data_intermediataire2 %>%
  mutate(id_row = row_number()) %>%
  separate_rows(Pensez.vous.que.le.fait.que.vous.soyez.un.consommateur.de.CBD.sans.THC.ait.impacté.votre.vie.sociale.., sep = ";") %>%
  mutate(Pensez.vous.que.le.fait.que.vous.soyez.un.consommateur.de.CBD.sans.THC.ait.impacté.votre.vie.sociale.. = trimws(Pensez.vous.que.le.fait.que.vous.soyez.un.consommateur.de.CBD.sans.THC.ait.impacté.votre.vie.sociale..)) 

data_consommateurs_binaire <- data_long %>%
  mutate(present = 1) %>%
  pivot_wider(
    id_cols = c(id_row, names(data_intermediataire2)[names(data_intermediataire2) != "Pensez.vous.que.le.fait.que.vous.soyez.un.consommateur.de.CBD.sans.THC.ait.impacté.votre.vie.sociale.."]),
    names_from = Pensez.vous.que.le.fait.que.vous.soyez.un.consommateur.de.CBD.sans.THC.ait.impacté.votre.vie.sociale..,
    values_from = present,
    values_fill = 0
  ) %>%
  select(-id_row)



# Pour exporter en CSV avec séparateur point-virgule 
write.csv2(data_binaire, "data_non_consommateurs.csv", row.names=FALSE)
write.csv2(data_consommateurs_binaire, "data_consommateurs.csv", row.names=FALSE)


         ##############################################
         #   Exploitation données non consommateurs   #
         ##############################################


data_non <- read.csv('data_non_consommateurs.csv', header=T, sep = ";")


######### Pensez vous que le CBD est une drogue pie chart #########

# installer le package
library(ggplot2)


# Étape 1: Calculer les fréquences
freq_table <- table(data_non$Pensez.vous.que.le.CBD.sans.THC.est.une.drogue) # transformer la variable réponse en table contenant les fréquences de chaque réponse
freq_df <- as.data.frame(freq_table)
names(freq_df) <- c("reponse", "count") # reponse est soit "oui" soit "non" et est associé à l'occurence (count) de cette réponse

# Étape 2: Calculer les pourcentages
freq_df$percentage <- freq_df$count / sum(freq_df$count) * 100 # ajoute une colonne au data frame avec les pourcentages de chaque réponse (occurence/total réponses)
freq_df$label <- paste0(freq_df$reponse, "\n", round(freq_df$percentage, 1), "%") # ajoute une colonne avec du texte pour le graphe (réponse, à la ligne, pourcentage)

# Étape 3: Créer le graphique en camembert 
pie_chart <- ggplot(freq_df, aes(x = "", y = count, fill = reponse)) + 
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) + # défini le texte qui sera présent sur les parts du camembert
  labs(title = "Pensez-vous que le CBD est une drogue ?",
       fill = "Réponses") +
  theme_minimal() +
  scale_fill_manual(values=c(c("#ECE4DF", "#8CB89F")))+ #régler les couleurs 
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# Afficher le graphique
print(pie_chart)

# Pour sauvegarder le graphique
ggsave("drogue_pie_chart.png", pie_chart, width =6,height=4) # sans fond


################################################################################

        ######## histogrammes des aspects sociaux #########
        #################### GENERAL ######################

# Étape 1: Calculer le pourcentage d'occurrence pour chaque adjectif
# Somme des 1 pour chaque colonne divisée par le nombre total de lignes

total_responses <- nrow(data_non) # nombre total de réponses

# Créer un dataframe pour les pourcentages qui contient 2 colonnes (une avec le nom de l'adjectif et l'autre avec le pourcentage)
adjectifs_pct <- data.frame(
  adjectif = character(), # spécifie la classe 
  pourcentage = numeric()
)

# Parcourir toutes les colonnes d'adjectifs et calculer leur pourcentage

for (col_name in names(data_non)) {                       # pour chaque colonne
  
  if (all(data_non[[col_name]] %in% c(0, 1, NA))) {       # Vérifier que c'est bien une colonne d'adjectifs (contenant des 0/1)
    count_ones <- sum(data_non[[col_name]], na.rm = TRUE) # compter les 1 dans la colonne (donc le nombre d'occurence)
    percentage <- (count_ones / total_responses) * 100    # calculer le pourcentage (occurence/total)
    
    adjectifs_pct <- rbind(adjectifs_pct,                 # ajouter une nouvelle ligne au dataframe avec le nom de l'adjectif et le pourcentage associé
                           data.frame(adjectif = col_name, 
                                      pourcentage = percentage))
  }
}

# Trier les adjectifs par pourcentage décroissant pour une meilleure visualisation
adjectifs_pct <- adjectifs_pct %>% arrange(desc(pourcentage))

data_clean <- adjectifs_pct[-(c(20,21,22)),] # enlever les colonnes qui ne correspondent pas à des adjectifs (sexe, age, pensez-vous que le CBD est une drogue)


# Étape 2: Créer le diagramme
barplot <- ggplot(data_clean, aes(x = reorder(adjectif, pourcentage), y = pourcentage)) +
  geom_bar(stat = "identity", fill = "#8CB89F") +
  geom_text(aes(label = sprintf("%.1f%%", pourcentage)), 
            vjust = -0.5, size = 3) +
  labs(title = "Pourcentage d'occurrence des adjectifs",
       x = "Adjectifs",
       y = "Pourcentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher le graphique
print(barplot)

# Sauvegarder le graphique
ggsave("adjectifs_barplot.png", barplot, width =10,height=6)

# Sauvegarder les pourcentages des adjectifs
write.csv2(data_clean, "pourcentages_adj.csv", row.names=FALSE) 



       ################ SELON PROCHE CBD ###################

# séparer les data non consommateurs selon leur proximité avec un consommateur
data_non_proche <- data_non[data_non$Êtes.vous.proche..ou.l.avez.vous.été..d.un.consommateur.régulier.de.CBD.sans.THC..=='Oui',]
data_non_pas_proche <- data_non[data_non$Êtes.vous.proche..ou.l.avez.vous.été..d.un.consommateur.régulier.de.CBD.sans.THC..=='Non',]

# pas proches de consommateur

total_responses <- nrow(data_non_pas_proche) # nombre de réponses total

adjectifs_pct <- data.frame(  # créer le data frame pour stocker les valeur
  adjectif = character(),
  pourcentage = numeric()
)

# Parcourir toutes les colonnes d'adjectifs et calculer leur pourcentage
for (col_name in names(data_non_pas_proche)) {
  
  # Vérifier que c'est bien une colonne d'adjectifs (contenant des 0/1)
  if (all(data_non_pas_proche[[col_name]] %in% c(0, 1, NA))) {
    count_ones <- sum(data_non_pas_proche[[col_name]], na.rm = TRUE)
    percentage <- (count_ones / total_responses) * 100
   
    # remplir le data frame
    adjectifs_pct <- rbind(adjectifs_pct, 
                           data.frame(adjectif = col_name, 
                                      pourcentage = percentage))
  }
}

# Trier les adjectifs par pourcentage décroissant 
adjectifs_pct <- adjectifs_pct %>% arrange(desc(pourcentage))

data_clean <- adjectifs_pct[-(c(20,21,22)),] # enlever les colonnes qui ne correspendent pas à des adjectifs (sexe, age, pensez-vous que le CBD est une drogue)

# Sauvegarder les pourcentages des adjectifs
write.csv2(data_clean, "pourcentages_adj_pas_proche.csv", row.names=FALSE)



          ##############################################
          #     Exploitation données consommateurs     #
          ##############################################

data_conso <- read.csv('data_consommateurs.csv', header=T, sep = ";")

######### pie chart - est ce que ça a un effet sur votre vie sociale #########

library(ggplot2)

# Étape 1: Calculer les fréquences
freq_table <- table(data_conso$Non..cela.n.a.aucun.effet.sur.ma.vie.sociale)
freq_df <- as.data.frame(freq_table)
names(freq_df) <- c("reponse", "count")

# changer les 0 et 1 en Oui et Non
freq_df$reponse<-as.character(freq_df$reponse) 
freq_df[1,1]<-"Oui"
freq_df[2,1]<-"Non"

# Étape 2: Calculer les pourcentages et ajouter la colonne de labels
freq_df$percentage <- freq_df$count / sum(freq_df$count) * 100
freq_df$label <- paste0(freq_df$reponse, "\n", round(freq_df$percentage, 1), "%")

# Étape 3: Créer le graphique en camembert
pie_chart <- ggplot(freq_df, aes(x = "", y = count, fill = reponse)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Pensez vous que le fait que vous consommiez du CBD ait eu un impact sur votre vie sociale ?",
       fill = "Réponses") + # nom de la légende
  theme_minimal() +
  scale_fill_manual(values=c(c("#ECE4DF", "#8CB89F")))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# Afficher le graphique
print(pie_chart)

# Pour sauvegarder le graphique
ggsave("effets_social_conso_pie_chart.png", pie_chart, width =6,height=4)


############ Bar graph des effets ###########

# Étape 1: Sélectionner les colonnes d'adjectifs à la main parmi toutes les colonnes

colonnes_selectionnees <- c("Cela.vous.a.rendu.e..plus.optimiste", "Cela.vous.a.isolé.e.", "Cela.vous.a.rendu.e..plus.renfermé.e.", "Cela.vous.a.rendu.e..plus.confiant.e.", "Cela.vous.a.rapproché.e..de.certaines.personnes", "Cela.vous.a.permis.de.rencontrer.de.nouvelles.personnes", "Cela.vous.a.permis.d.être.plus.ouvert.e.")

# Étape 2: Calculer les pourcentages pour ces colonnes (même méthode que pour les données précédentes)
total_responses <- nrow(data_conso)
adjectifs_pct <- data.frame(
  adjectif = character(),
  pourcentage = numeric()
)

for (col_name in colonnes_selectionnees) {
  count_ones <- sum(data_conso[[col_name]], na.rm = TRUE)
  percentage <- (count_ones / total_responses) * 100
  
  adjectifs_pct <- rbind(adjectifs_pct, 
                         data.frame(adjectif = col_name, 
                                    pourcentage = percentage))
}

adjectifs_pct <- adjectifs_pct %>% arrange(desc(pourcentage))

# sauvegarer les données
write.csv2(adjectifs_pct, "pourcentages_adj_consommateurs.csv", row.names=FALSE)

# Etape 3 : créer le barplot

barplot <- ggplot(adjectifs_pct, aes(x = reorder(adjectif, pourcentage), y = pourcentage)) +
  geom_bar(stat = "identity", fill = "#8CB89F") +
  geom_text(aes(label = sprintf("%.1f%%", pourcentage)), 
            vjust = -0.5, size = 3) +
  labs(title = "Pourcentage d'occurrence des adjectifs",
       x = "Adjectifs",
       y = "Pourcentage (%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Afficher le graphique
print(barplot)



######## INFO COMPLEMENTAIRES ########


# tranche d'âge des participants

total_responses <- nrow(data)
ages_pct <- data.frame(
  adjectif = character(),
  pourcentage = numeric(),
  occurence = numeric() # une colonne en plus que les fois précédentes et qui contient les occurences de chaque réponse
)

age <- unique(data$Votre.tranche.d.âge) # stocker les différentes valeurs que peut prendre la variable pour indexer dans la boucle

for (tranche in age){
  compte <- sum(data$Votre.tranche.d.âge==tranche)
  percentage <- (compte/total_responses)*100
  
  ages_pct <- rbind(ages_pct, 
                  data.frame(adjectif = tranche, 
                             pourcentage = percentage,
                             occurence = compte))
  } 

# sauvegarder les données
write.csv2(ages_pct, "pourcentages_tranches_dage.csv", row.names=FALSE)

ages_pct$label <- paste0(round(ages_pct$pourcentage, 1), "%") # ajouter une colonne de label pour afficher les % sur le graphe

# Créer le graphique
pie_chart <- ggplot(ages_pct, aes(x = "", y = occurence, fill = adjectif)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5)) +
  labs(title = "Tranches d'âge des répondants",
       fill = "Réponses") +
  theme_minimal() +
  #scale_fill_manual(values=c(c("#ECE4DF", "#8CB89F")))+
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank())

# Afficher le graphique
print(pie_chart)


# sexes des participants (même méthodologie que pour l'âge)

total_responses <- nrow(data)
sexe_pct <- data.frame(
  sexe = character(),
  pourcentage = numeric(),
  occurence = numeric()
)

sex <- unique(data$Vous.êtes..)

for (i in sex){
  compte <- sum(data$Vous.êtes..==i)
  percentage <- (compte/total_responses)*100
  
  sexe_pct <- rbind(sexe_pct, 
                    data.frame(sexe = i, 
                               pourcentage = percentage,
                               occurence = compte))
} 

# sauvegarder les données
write.csv2(sexe_pct, "pourcentages_sexes.csv", row.names=FALSE)

