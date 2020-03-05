setwd('~/BriefProjet')

library(dplyr)
library(tidyr)
library(ggplot2)
install.packages('funModeling')
library(funModeling)


entr <- read.csv('entreprise_2020_02_19_04_00.csv', encoding = "UTF-8")
av <- read.csv2('declaration_avantage_2020_02_19_04_00.csv', encoding = "UTF-8")
remu <- read.csv('declaration_remuneration_2020_02_19_04_00.csv', sep=";", encoding = "UTF-8")
conv <- read.csv2('declaration_convention_2020_02_19_04_00.csv', encoding = "UTF-8")

#elimination des doublons de chaque table
centr <- unite(entr, 'adresse', adresse_1, adresse_2, adresse_3, adresse_4, sep = "--", remove = TRUE)
cconv <- distinct(conv)
cav <- distinct(av)
cremu <- distinct(remu)

summary(centr)
summary(cremu)
summary(cconv)
summary(cav)

#nettoyage des valeurs négatives
conv_positif <- cconv %>%
  filter(conv_montant_ttc >=0)

summary(conv_positif$conv_montant_ttc)

#fonction pour connaître le taux de valeurs manquantes das chaque table
df_status(cremu)
df_status(cconv)
df_status(cav)

#recherche manuelle des valeurs manquantes
sapply(cremu,function(x) sum(x == "" | is.na(x) | x == 0))
sapply(cremu,function(x) sum(is.na(x)))
sapply(cremu,function(x) sum(x == "" | is.na(x) | x == 0)/452711*100)
sapply(cconv,function(x) sum(x == "" | is.na(x) | x == 0)/5055070*100)
sapply(cremu$benef_etablissement,function(x) sum(x == "Personnes morales assurant la formation initiale ou continue des professionnels de santÃ©" ))









#piecharts

#répartition territoriale des entreprises, France et Etranger

#on crée une mini-base de données pour le pie-chart
pie_pays <- centr %>%
  select(identifiant, pays) %>%
  mutate(Is_France = case_when(pays == 'FRANCE'~ 'France',
                               pays != 'FRANCE'~ 'Etranger',
                               TRUE ~ as.character(pays))
                               )%>%
  group_by(Is_France) %>%
  count() %>%
  arrange(desc(n))

pie_pays$fraction = pie_pays$n/sum(pie_pays3$n)*100
pie_pays$fraction

barplot_pays <- ggplot(data=pie_pays2, aes(x=Is_France, y=n)) +
  geom_bar(stat="identity", color="blue", fill="white") +
  geom_text(aes(label= n), vjust=-0.3, size=3.5) +
  theme_minimal()

piechart_pays <- ggplot(data=pie_pays2, aes(x="", y=Is_France, fill=Is_France)) +
  geom_bar(stat="identity", width = 1, color="blue") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position = "none") +
  scale_fill_brewer(palette="Set1") 
piechart_pays



#recherche des coefficients de correlation
cor_conv<-cor.test(cconv$conv_montant_ttc, as.numeric(cconv$categorie), method="pearson")
cor_conv<-cor.test(cconv$conv_montant_ttc, as.numeric(cconv$categorie), method="pearson")










#analyse temporelle des rémunérations
#on crée une base de données exploitable pour l'analyse
remu_temps <- cremu %>%
  select(remu_date, remu_montant_ttc, categorie) %>%
  mutate(annee = substr(remu_date, 7, 10)) %>%
  mutate(mois = substr(remu_date, 4, 5)) %>%
  mutate(semestre = case_when(mois == "01"|mois == "02"|mois == "03"|mois == "04"|mois == "05"|mois == "06" ~ "1",
                              mois == "07"|mois == "08"|mois == "09"|mois == "10"|mois == "11"|mois == "12" ~ "2",
                              TRUE ~ mois)
         )%>%
  filter(annee=="2010" | annee=="2011" | annee=="2012" | annee=="2013" | annee=="2014" |
           annee=="2015" | annee=="2016" | annee=="2017" | annee=="2018" | annee=="2019") %>%
  arrange(desc(annee), desc(mois))
  

#chart remuneration par annee  
remu_temps_plot2 <- ggplot(remu_temps)+
  geom_bar(mapping = aes(x = annee, y = remu_montant_ttc, color = "Blue"), stat = "identity")


#chart remunaration par année, empilé par semeste
remu_temps_plot3 <- ggplot(remu_temps, aes(x = annee, y = remu_montant_ttc, fill=semestre))+
  geom_bar(stat = "identity") +
  #geom_text(aes(y=remu_montant_ttc, label=remu_montant_ttc), vjust=1.6, 
            #color="white", size=3.5)+
  #geom_text(aes(label=remu_montant_ttc), vjust=-0.3, size=3.5)+
  scale_fill_brewer(palette="Paired")+
  theme_classic()

#chart remuneration par annee, barre double par semestre
remu_temps_plot4 <- ggplot(remu_temps, aes(x = annee, y = remu_montant_ttc, fill=semestre))+
  geom_bar(stat = "identity", position=position_dodge()) +
  scale_fill_brewer(palette="Paired")+
  theme_classic()


#courbes de densité
#grande echelle
densite_remu_temps <- ggplot(remu_temps) + 
  geom_density(aes(x = remu_montant_ttc, color = annee), bw = 100) +
  xlim(0,70000) +
  theme_classic()

#petite echelle
densite_remu_temps2 <- ggplot(remu_temps) + 
  geom_density(aes(x = remu_montant_ttc, color = annee), bw = 100) +
  xlim(0,10000) +
  theme_classic()





#charts de remuneration par categorie par mois, pour chaque annee entre 2010 et 2019

#2019
remu2019 <- remu_temps%>%
  filter(annee == 2019)


remu2019_plot <- ggplot(remu2019, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#2018
remu2018 <- remu_temps%>%
  filter(annee == 2018)

remu2018_plot <- ggplot(remu2018, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#2017
remu2017 <- remu_temps%>%
  filter(annee == 2017)

remu2017_plot <- ggplot(remu2017, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#2016
remu2016 <- remu_temps%>%
  filter(annee == 2016)

remu2016_plot <- ggplot(remu2016, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#2015
remu2015 <- remu_temps%>%
  filter(annee == 2015)

remu2015_plot <- ggplot(remu2015, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#2014
#avec GSK
remu2014 <- remu_temps%>%
  filter(annee == 2014)

remu2014_plot <- ggplot(remu2014, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()

#sans GSK
remu2014_ssGSK <- cremu %>%
  select(entreprise_identifiant, remu_date, remu_montant_ttc, categorie) %>%
  mutate(annee = substr(remu_date, 7, 10)) %>%
  mutate(mois = substr(remu_date, 4, 5)) %>%
  filter(annee == "2014") %>%
  filter(entreprise_identifiant != "OHBQOWOS") %>%
  arrange(desc(annee), desc(mois))
  
remu2014ssGSK_plot <- ggplot(remu2014_ssGSK, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#2013
remu2013 <- remu_temps%>%
  filter(annee == 2013)

remu2013_plot <- ggplot(remu2013, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#2012
remu2012 <- remu_temps%>%
  filter(annee == 2012)

remu2012_plot <- ggplot(remu2012, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#2011
remu2011 <- remu_temps%>%
  filter(annee == 2011)

remu2011_plot <- ggplot(remu2011, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#2010
remu2010 <- remu_temps%>%
  filter(annee == 2010)

remu2010_plot <- ggplot(remu2010, aes(x = mois, y = remu_montant_ttc, fill=categorie))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()


#CAS GSK

#base de données des rémunérations en juin 2014 (15 plus gros montants totaux par entreprise)
remu_entr <- cremu %>%
  select(denomination_sociale, remu_montant_ttc, remu_date, qualite) %>%
  mutate(annee = substr(remu_date, 7, 10)) %>%
  mutate(mois = substr(remu_date, 4, 5)) %>%
  filter(annee == '2014' & mois == '06') %>%
  group_by(denomination_sociale) %>%
  summarise(total_montant = sum(remu_montant_ttc)) %>%  
  arrange(desc(total_montant)) %>%
  head(15)

#base de données des rémunérations de la presse en juin 2014
remu_presse <- cremu %>%
  select(denomination_sociale, remu_montant_ttc, remu_date, categorie) %>%
  mutate(annee = substr(remu_date, 7, 10)) %>%
  mutate(mois = substr(remu_date, 4, 5)) %>%
  filter(annee == '2014' & mois == '06' & categorie =='Presse et média') %>%
  group_by(denomination_sociale) %>%
  summarise(total_montant = sum(remu_montant_ttc)) %>%  
  arrange(desc(total_montant))

#chart mettant en evidence les entreprises rémunérant la presse parmis les plus gros donneurs
is_entpres <- as.character(remu_presse$denomination_sociale)

remu_entr_plot <- ggplot(remu_entr, aes(x = reorder(denomination_sociale, -total_montant), 
                                        y = total_montant,
                                        fill=factor(ifelse(denomination_sociale %in% is_entpres, "Beneficiaire Presse", "Autre")))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("grey50", "blue")) +   
  theme(axis.text.x=element_text(angle = 90, hjust = 0))
remu_entr_plot


#meme chart que le précédent, allongé
remu_entr_plot_long <- ggplot(remu_entr, aes(x = reorder(denomination_sociale, total_montant), 
                                        y = total_montant,
                                        fill=factor(ifelse(denomination_sociale %in% is_entpres,"Beneficiaire Presse", "Autre")))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values=c("grey50", "blue")) +
  coord_flip() +
  theme(legend.position = "bottom", legend.title = element_blank(), 
        axis.text.x=element_text(angle = 90, hjust = 0))
remu_entr_plot_long














remu_presse <- ggplot(remu_entr, aes(x = denomination_sociale, y = n, fill=qualite))+
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Paired")+   # palette n'a pas assez de couleurs
  theme_classic()














#repartition des moyennes de chaque mois pour les annees 2012-2018
#moyenne de toutes les valeurs par mois

#nouvelle colonne moyenne
remu_mean <- remu_temps %>%
  group_by(mois) %>%
  mutate(moyenne_mois = mean(remu_montant_ttc))

remu_mean_plot <- ggplot(remu_mean, aes(x = mois, y = moyenne_mois, group = 1)) +
  geom_line(color = "steelblue", size = 5) + 
  geom_point(color = "red", size = 8) + 
  theme_classic()
remu_mean_plot

#avec agrégation  
moyenne_total_mois <- remu_mean_total %>%
  group_by(mois) %>%
  summarise(moyenne_total_mois = mean(total_mois))

remu_meantotal_plot <- ggplot(moyenne_total_mois, aes(x = mois, y = moyenne_total_mois, group = 1)) +
  geom_line(color = "steelblue", size = 4) + 
  geom_point(color = "red") + 
  theme_classic()
remu_meantotal_plot


#moyenne des totaux de chaque mois
#base de données
remu_mean_total <- remu_temps %>%
  filter(annee=='2012' | annee=='2013' | annee=='2014' |
           annee=='2015' | annee=='2016' | annee=='2017' | annee=='2018') %>%
  group_by(annee, mois) %>%
  summarise(total_mois = sum(remu_montant_ttc)) %>%
  group_by(mois) %>%
  summarise(moyenne_total_mois = mean(total_mois))

#diagramme points reliés
remu_meantotal_plot <- ggplot(remu_mean_total, aes(x = mois, y = moyenne_total_mois, group = 1)) +
  geom_line(color = "steelblue", size = 5) + 
  geom_point(color = "red", size = 8) + 
  theme_classic()
remu_meantotal_plot


