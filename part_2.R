a = 4
a
a = 5
a
a = [2,4]
a = 2,4
a = list(2,3)
a
a(1)
a[0]
a[1]
library(readxl)
resultats_pres2022 <- read_excel("E:/Licence_sig/13_R/resultats_pres2022.xlsx")
View(resultats_pres2022)
summary(resultats_pres2022$Part_Macron)
summary(resultats_pres2022$Part_abs)
moyenneMacron <- mean(resultats_pres2022$Part_Macron)
moyenneMacron #on appelle notre variable
paste("La moyenne des parts de vote pour Macron est de ", round(moyenneMacron, digits=2), "%")
paste("La moyenne des parts de vote pour Macron est de ", round(moyenneMacron, digits=2),"%")
moyenneAbs <- mean(resultats_pres2022$Part_abs)
paste("L'abstention moyenne est de ",round(moyenneAbs, digits = 2), "%" )
paste("L'abstention moyenne est de ",round(moyenneAbs, digits=2),"%" )
paste("L'abstention moyenne est de",round(moyenneAbs, digits=2),"%" )
Requete1 <- resultats_pres2022 [resultats_pres2022$Part_LePen > 10,]
View(Requete1) # permet de voir la nouvelle requete
count(Requete1)
install.packages("dplyr")
count(Requete1)
library(dplyr)
count(Requete1)
Requete2 <- resultats_pres2022[resultats_pres2022$Part_LePen > 30 $ resultats_pres2022$Part_LePen < 60 ,]
Requete2 <- resultats_pres2022[resultats_pres2022$Part_LePen > 30 & resultats_pres2022$Part_LePen < 60 ,]
View(Requete2) #visualisation de la requete2
count(Requete2)
voteJadot2040 = resultats_pres2022[resultats_pres2022$Part_Jadot >=20 & resultats_pres2022$Part_Jadot <= 40 ,]
voteJadot2040 = resultats_pres2022[resultats_pres2022$Part_Jadot >20 & resultats_pres2022$Part_Jadot < 40 ,]
voteRochefort <- resultats_pres2022 [resultats_pres2022$Libelle == 'Rochefort' & resultats_pres2022$Code_dep == '17' ,]
View(voteRochefort)
voteSaintes <- resultats_pres2022 [resultats_pres2022$Libelle == 'Saintes' & resultats_pres2022$Code_dep == '17' ,]
write.csv(voteSaintes, fileEncoding = utf8)
write.csv(voteSaintes,file = "E:\Licence_sig\13_R\voteSaintes.csv", fileEncoding = utf8)
write.csv(voteSaintes,file = E:\Licence_sig\13_R\voteSaintes.csv, fileEncoding = utf8)
write.csv(voteSaintes,file = 'E:/Licence_sig/13_R/voteSaintes.csv', fileEncoding = utf8)
write.csv(voteSaintes,file = 'E:/Licence_sig/13_R/voteSaintes.csv', fileEncoding = "UTF-8")
voteCandidats1_4 <- resultats_pres2022[, c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")]
View(voteCandidats1_4)
typeof(voteCandidats1_4)
sapply(voteCandidat1_4, class)
sapply(voteCandidats1_4, class)
voteSaintes_candidats1_3 <- voteSaintes[, c("Code_BV","Part_Macron", "Part_LePen", "Part_Melenchon")]
View(voteSaintes_candidats1_3)
ecartTypeMacron = sd(voteSaintes_candidats1_3&Part_Macron)
ecartTypeMacron <- sd(voteSaintes_candidats1_3&Part_Macron)
ecartTypeMacron <- sd(voteSaintes_candidats1_3[2])
voteSaintes_candidats1_3[2]
sd(voteSaintes_candidats1_3[2])
sd(voteSaintes_candidats1_3)
ecartTypeMacron = voteSaintes_candidats1_3[2]
sd(ecartTypeMacron)
ecartTypeMacro
ecartTypeMacron
sd(double(ecartTypeMacron)
q
ecartTypeMacron = sd(as.double(voteSaintes_candidats1_3[2]))
ecartTypeMacron = sd(voteSaintes_candidats1_3as.double([2]))
ecartTypeMacron = sd(voteSaintes_candidats1_3[as.double(2]))
ecartTypeMacron = sd(voteSaintes_candidats1_3[as.double(2)])
ecartTypeMacron = voteSaintes_candidats1_3[sd(2)]
ecartTypeMacron = sd(voteSaintes_candidats1_3[2], na.rm = True)
ecartTypeMacron = sd(voteSaintes_candidats1_3[2], na.rm = TRUE)
ecartTypeMacron <- sd(voteSaintes_candidats1_3)
ecartTypeMacron <- sd(voteSaintes_candidats1_3[2])
ecartTypeMacron <- sd(voteSaintes_candidats1_3[[2]])
ecartTypeLePen <- sd(voteSaintes_candidats1_3[[3]])
ecartTypeMelenchon <- sd(voteSaintes_candidats1_3[[4]])
ecartTypeLePen
ecartTypeMacron
ecartTypeMelenchon
voteCandidats1_4$ColonneTest <- NA
voteCandidats1_4$ColonneTest <- voteCandidats1_4$Part_Macron +voteCandidats1_4$Part_Pecresse
view(voteCandidats1_4)
View(voteCandidats1_4)
voteCandidats1_4$ColonneTest <- rowSums(voteCandidats1_4[, c('Part_Macron','Part_Pecresse')], na.rm = TRUE)
voteCandidats1_4$ColonneTest <- ifelse(voteCandidats1_4$Part_Macron > 25, voteCandidats1_4$Part_Macron + voteCandidats1_4$Part_Pecresse, NA)
install.packages("tidyr")
Transpo <- voteCandidats1_4 %>% pivot_longer( cols = c(Part_Macron, Part_LePen, Part_Macron))
Transpo <- voteCandidats1_4 %>% pivot_longer( cols = c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse), names_to = "Candidat", values_to = "Parts_vote")
library(tidyr)
Transpo <- voteCandidats1_4 %>% pivot_longer( cols = c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse), names_to = "Candidat", values_to = "Parts_vote")
View(Transpo)
voteSaintes_candidats1_3$colonne <- ifelse(voteSaintes_candidats1_3$Part_Macron > 30, voteSaintes_candidats1_3$colonne = MacronSup30, NA)
voteSaintes_candidats1_3$colonne <- ifelse(voteSaintes_candidats1_3$Part_Macron > 30, "MacronSup30", NA)
View(voteSaintes_candidats1_3)
sapply(voteCandidats1_4, mean)
tapply(resultats_pres2022$Part_abs, resultats_pres2022$Libelle_dep, mean)
dataList <- c("Part_Macron", "Part_LePen", "Part_Melenchon")
for(i in dataList){
print(i)
}
for(i in dataList){
print(summary(resultats_pres2022[,i]))
}
for(i in dataList){
moy <- round(mean(as.numeric(unlist(resulttas_pres2022[,i]))), digits = 2)
}
for(i in dataList){
moy <- round(mean(as.numeric(unlist(resultas_pres2022[,i]))), digits = 2)
print(paste("Moyenne", i, ": ", moy, " %"))
}
for(i in dataList){
moy <- round(mean(as.numeric(unlist(resultays_pres2022[,i]))), digits = 2)
print(paste("Moyenne", i, ": ", moy, " %"))
}
for(i in dataList){
moy <- round(mean(as.numeric(unlist(resultays_pres2022[,i]))), digits = 2)
print(paste("Moyenne", i, ": ", moy, " %"))
}
for(i in dataList){
moy <- round(mean(as.numeric(unlist(resultats_pres2022[,i]))), digits = 2)
print(paste("Moyenne", i, ": ", moy, " %"))
}
votearthaud_Vaucluse <- resultats_pres2022[ resultats_pres2022$Libelle == 'Vaucluse' & resultats_pres2022$Code_dep == '84', c("Part_Arthaud")]
votearthaud_Vaucluse <- resultats_pres2022[ resultats_pres2022$Libelle == 'Vaucluse' & resultats_pres2022$Code_dep == '84',]
votearthaud_Vaucluse <- resultats_pres2022[resultats_pres2022$Code_dep == '84', c("Part_Arthaud")]
tapply(votearthaud_vaucluse$Part_Arthaud, max)
View(votearthaud_Vaucluse)
votearthaud_Vaucluse <- resultats_pres2022[resultats_pres2022$Code_dep == '84', c("Libelle","Part_Arthaud")]
View(votearthaud_Vaucluse)
tapply(votearthaud_Vaucluse$Part_Arthaud,votearthaud_Vaucluse$Libelle, max)
install.packages("ggplot2")
library(ggplot2)
col1 <- c("Macron", "Le Pen", "Melenchon", "Pecresse")
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_Pecresse))
data <- data.frame(group=col1, value=col2)
ggplot(data = data, aes(x=reorder(group, -value), y=value, fill=group)) + geo_bar(stat="identity") +
geom_text(aes(label=round(value, digits=2)), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
col1 <- c("Macron", "Le Pen", "Melenchon", "Pecresse")
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_Pecresse))
data <- data.frame(group=col1, value=col2)
ggplot(data = data, aes(x=reorder(group, -value), y=value, fill=group)) + geom_bar(stat="identity") +
geom_text(aes(label=round(value, digits=2)), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
col1 <- c("Macron", "Le Pen", "Melenchon", "Pecresse")
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_Pecresse))
data <- data.frame(group=col1, value=col2)
ggplot(data = data, aes(x=reorder(group, -value), y=value, fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", satrt=0) +
geom_text(aes(label=round(value, digits=2)), position = position_stack(vjust=0.5)+ scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
q
col1 <- c("Macron", "Le Pen", "Melenchon", "Pecresse")
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_Pecresse))
data <- data.frame(group=col1, value=col2)
ggplot(data = data, aes(x=reorder(group, -value), y=value, fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", satrt=0) +
geom_text(aes(label=round(value, digits=2)), position = position_stack(vjust=0.5)+ scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
q
col1 <- c("Macron", "Le Pen", "Melenchon", "Pecresse")
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_Pecresse))
data <- data.frame(group=col1, value=col2)
ggplot(data , aes(x="", y=value, fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", satrt=0) +
geom_text(aes(label=round(value, digits=2)), position = position_stack(vjust=0.5)+ scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
q
View(voteRochefort)
col1 <- c("Macron", "Le Pen", "Melenchon", "Pecresse")
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_Pecresse))
data <- data.frame(group=col1, value=col2)
ggplot(data , aes(x="", y=value, fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", satrt=0) +
geom_text(aes(label=round(value, digits=2)), position = position_stack(vjust=0.5))+ scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
col1 <- c("Macron", "Le Pen", "Melenchon", "Pecresse")
col2 <- c(mean(voteCandidats1_4$Part_Macron), mean(voteCandidats1_4$Part_LePen), mean(voteCandidats1_4$Part_Melenchon), mean(voteCandidats1_4$Part_Pecresse))
data <- data.frame(group=col1, value=col2)
ggplot(data , aes(x="", y=value, fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", start=0) +
geom_text(aes(label=round(value, digits=2)), position = position_stack(vjust=0.5))+ scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
voteGauche_LR <- resultats_pres2022[resultats_pres2022$Libelle == 'La Rochelle' & resultats_pres2022$Code_dep == '17', c("Part_Hidalgo", "Part_Jadot", "Part_Melenchon")]
col1 <- c("Hidalgo", "Jadot", "Melenchon")
col2 <- c(mean(voteGauche_LR$Part_Hidalgo), mean(voteGauche_LR$Part_Jadot), mean(voteGauche_LR$Part_Melenchon))
data <- data.frame(group=col1, value=col2)
ggplot(data , aes(x="", y=value, fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", start=0) +
geom_text(aes(label=round(value, digits=2)), position = position_stack(vjust=0.5))+ scale_fill_manual(values = c("#F5B", "#Af5","#F00"))
col1 <- c("Hidalgo", "Jadot", "Melenchon")
col2 <- c(mean(voteGauche_LR$Part_Hidalgo), mean(voteGauche_LR$Part_Jadot), mean(voteGauche_LR$Part_Melenchon))
data <- data.frame(group=col1, value=col2)
ggplot(data , aes(x="", y=value, fill=group)) + geom_bar(stat="identity", width=1) + geom_col() + coord_polar("y", start=0) +
geom_text(aes(label=round(value, digits=2)), position = position_stack(vjust=0.5))+ scale_fill_manual(values = c("#F5B", "#AD5","#F00"))
voteRochefort_Candidats1_4 <- voteRochefort[, c("Code_BV", "Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")]
reformat <- voteRochefort_Candidats1_4 %>% pivot_longer(cols=c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse), names_to='candidats', values_to='parts')
ggplot(data = reformat, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Rochefort") +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
theme(axis.left.x = element_text(angle=90))+
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Rochefort") +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
theme(axis.text.x = element_text(angle=90))+
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
voteRoyan_Candidats1_4 <- resultats_pres2022[resultats_pres2022$Libelle == 'Royan' & resultats_pres2022$Code_dep == '17', c("Code_BV", "Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse")]
reformat_royan <- voteRoyan_Candidats1_4 %>% pivot_longer(cols=c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse), names_to='candidats', values_to='parts')
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Rochefort") +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
theme(axis.text.x = element_text(angle=90))+
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
theme(axis.text.x = element_text(angle=90))+
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
theme(axis.text.x = element_text(angle=90))+
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill="")) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x="", y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
save.image("E:/Licence_sig/13_R/exercice.RData")
load("E:/Licence_sig/13_R/exercice.R")
reformat_royan <- voteRoyan_Candidats1_4 %>% pivot_longer(cols=c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse))
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
reformat_royan <- voteRoyan_Candidats1_4 %>% pivot_longer(cols=c(Part_Macron, Part_LePen, Part_Melenchon, Part_Pecresse), names_to='candidats', values_to='parts')
ggplot(data = reformat_royan, aes(x=candidats, y=parts, fill=candidats)) +
geom_bar(stat = "identity", width = 1) +
coord_polar(theta = "y") +
facet_wrap(~Code_BV) +
ggtitle("Part de vote dans les BV de Royan") +
scale_fill_manual(values = c("#0a3895", "#b831f3","#f33157","#6091f6"))
savehistory(file="E:/Licence_sig/13_R/code.R")
install.packages("RPostgres")
install.packages("RPostgreSQL")
install.packages("DBI")
library(RPostgres)
library(RPostgreSQL)
db <- "postrges"
db_host <- "localhost"
db_port <- "5432"
db_user <- "postgres"
db_pass <- "Nucleaire27!"
conn <- dbConnect(RPostgres::Postgres(),dbname = db, host = db_host, port = db_port, user = db_user, password = db_pass)
conn
db <- "postgres"
conn <- dbConnect(RPostgres::Postgres(),dbname = db, host = db_host, port = db_port, user = db_user, password = db_pass)
conn
db <- "Site_Pres"
conn <- dbConnect(RPostgres::Postgres(),dbname = db, host = db_host, port = db_port, user = db_user, password = db_pass)
conn
requete <- dbGetQuery(conn, 'SELECT * FROM demande.form;')
savehistory(file="E:/Licence_sig/13_R/code.R")
View(requete)
requeteSuppr <- dbPostQuery(conn, "DELETE FROM demande.form WHERE prenonom = 'Enzo Marais")
requeteSuppr <- dbGetQuery(conn, "DELETE FROM demande.form WHERE prenonom = 'Enzo Marais")
requeteSuppr <- dbGetQuery(conn, "DELETE FROM demande.form WHERE prenonom =`Enzo Marais`;")
requeteSuppr <- dbGetQuery(conn, "DELETE FROM demande.form WHERE prenonom ='Enzo Marais';")
requeteSuppr <- dbPostQuery(conn, "DELETE FROM demande.form WHERE prenonom ='Enzo Marais';")
requeteSuppr <- dbExecute(conn, "DELETE FROM demande.form WHERE prenonom ='Enzo Marais';")
requete <- dbGetQuery(conn, 'SELECT * FROM demande.form;')
View(requete)
install.packages("rmarkdown")
library(rmarkdown)
savehistory(file="E:/Licence_sig/13_R/code.R")
clear
library(readxl)
barometre_sncf_2019 <- read_excel("E:/Licence_sig/13_R/barometre_sncf_2019.xlsx")
View(barometre_sncf_2019)
gare_123 <- barometre_sncf_2019[barometre_sncf_2019$Typologie_gare == '123']
gare_123 <- barometre_sncf_2019[barometre_sncf_2019$Typologie_gare == '123',]
View(gare_123)
gare_123 <- barometre_sncf_2019[barometre_sncf_2019$Typologie_gare == '123' & barometre_sncf_2019$Agence == 'Nelle Aquitaine',]
View(gare_123)
gare_123$note <- mean(c(gare_123$Infos, gare_123$Deplacement, gare_123$Proprete, gare_123$Agreable, gare_123$Satisfaction)])
gare_123$note <- mean(c(gare_123$Infos, gare_123$Deplacement, gare_123$Proprete, gare_123$Agreable, gare_123$Satisfaction))
gare_123$note <- mean(gare_123$Infos, gare_123$Deplacement, gare_123$Proprete, gare_123$Agreable, gare_123$Satisfaction)
gare_123$note <- rowMeans(gare_123[, c("Infos", "Deplacement", "Proprete", "Agreable", "Services")])
print(summary(gare_123$note))
gare_BN <- gare_123[gare_123$note > '7.5']
gare_BN <- gare_123[gare_123$note > 7.5]
gare_BN <- gare_123[gare_123$note > 7.5,]
gare_BN <- gare_123[gare_123$note > 8.1,]
write.csv(gare_BN, file='E:/Licence_sig/13_R/gare_bn.csv', fileEncoding = "UTF-8")
col1 <- c("Infos", "Deplacement", "Proprete", "Agreable", "Satisfaction", "Services")
col2<-c(gare_123$Infos, gare_123$Deplacement, gare_123$Proprete, gare_123$Agreable, gare_123$Satisfaction, gare_123$Services)
data<- data.frame(group=col1, value=col2)
ggplot(data=data, aes(x=reorder(group, -value), y=value, fill=group)) + geom_bar(stat="identity") +
geom_text(aes(label=value), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
library(ggplot2)
ggplot(data=data, aes(x=reorder(group, -value), y=value, fill=group)) + geom_bar(stat="identity") +
geom_text(aes(label=value), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=data, aes(x=reorder(group, value), y=value, fill=group)) + geom_bar(stat="identity") +
geom_text(aes(label=value), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
reformat <- gare_123 %>% pivot_longer(cols = c(Infos, Deplacement, Proprete, Agreable, Satisfaction, Services), names_to = 'notes', values_to='parts')
library(tidyr)
reformat <- gare_123 %>% pivot_longer(cols = c(Infos, Deplacement, Proprete, Agreable, Satisfaction, Services), names_to = 'notes', values_to='parts')
ggplot(data=data, aes(x=reorder(group, value), y=value, fill=group)) + geom_bar(stat="identity") +
geom_text(aes(label=value), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=parts, y=notes, fill=parts)) + geom_bar(stat="identity", width=1) +
geom_text(aes(label=value), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=data, aes(x=reorder(group, value), y=value, fill=group)) + geom_bar(stat="identity") +
geom_text(aes(label=notes), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=parts, y=notes, fill=parts)) + geom_bar(stat="identity", width=1) +
geom_text(aes(label=notes), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=parts, y=notes, fill=parts)) + geom_bar(stat="identity", width=1) +
geom_text(aes(label=parts), vjust=1.6, color="white", size=3.5) + scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=parts, y=notes, fill=parts)) + geom_bar(stat="identity", width=1) +
scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=nots, y=parts, fill=notes)) + geom_bar(stat="identity", width=1) +
scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=notes, y=parts, fill=notes)) + geom_bar(stat="identity", width=1) +
scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=notes, y=parts, fill=notes)) + geom_bar(stat="identity", width=1) +
scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=notes, y=parts, fill=notes)) + geom_bar(stat="identity", width=1) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
theme(axis.text.x = element_text(angle=90))+
scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=parts, y=notes, fill=notes)) + geom_bar(stat="identity", width=1) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
theme(axis.text.x = element_text(angle=90))+
scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
View(reformat)
ggplot(data=reformat, aes(x=parts, y=notes, fill=notes)) + geom_bar(stat="identity", width=1) +
facet_wrap(~Gare) +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
theme(axis.text.x = element_text(angle=90))+
scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
ggplot(data=reformat, aes(x=parts, y=notes, fill=notes)) + geom_bar(stat="identity", width=1) +
facet_wrap(~Gare) +
ggtitle("Note par Gare et par type de notation") +
theme(plot.title = element_text(family="Trebuchet MS", face="bold", size = 20, hjust = 0, color="#555555")) +
theme(axis.text.x = element_text(angle=90))+
scale_fill_manual(values = c("#0a3895", "#ffb547", "#973afa", "#82ccf3", "#c14d25", "#71f0bc"))
savehistory(file='E:/Licence_sig/13_R/final.R')
install.packages("sf")
library(sf)
testSIG = read_sf('E:/Licence_sig/13_R/larochelle.shp')
View(testSIG)
library(readxl)
resultats_pres2022 <- read_excel("E:/Licence_sig/13_R/resultats_pres2022.xlsx")
View(resultats_pres2022)
Part_Macron <- resultats_pres2022$Part_Macron
Part_LePen <- resultats_pres2022$Part_LePen
cor(Part_Macron, Part_LePen)
Part_Jadot <- resultats_pres2022$Part_Jadot
Part_Mel <- resultats_pres2022$Part_Melenchon
cor(Part_Jadot, Part_Mel)
Part_Pec <- resultats_pres2022$Part_Pecresse
cor(Part_Macron, Part_Pex)
cor(Part_Macron, Part_Pec)
round(cor(Part_Macron, Part_Pec), digits=2)
# Définition des variables x et y et représentation de la droite
ggplot(resultats_pres2022, aes(x=Part_Macron,
y=Part_Jadot)) + geom_point(size=0.5) +
scale_x_continuous(breaks = seq(1, 50, by = 5)) +
scale_y_continuous(breaks = seq(1, 50, by = 5)) +
stat_smooth(method = "lm",
col = "#C42126",
se = FALSE,
size = 1)
library(ggplot2)
library(ggplot2)
# Définition des variables x et y et représentation de la droite
ggplot(resultats_pres2022, aes(x=Part_Macron,
y=Part_Jadot)) + geom_point(size=0.5) +
scale_x_continuous(breaks = seq(1, 50, by = 5)) +
scale_y_continuous(breaks = seq(1, 50, by = 5)) +
stat_smooth(method = "lm",
col = "#C42126",
se = FALSE,
size = 1)
tabMultiVarie <- resultats_pres2022[, c("Code_dep", "Code_com", "Part_Macron", "Part_LePen", "Part_Melenchon",
"Part_Pecresse", "Part_Jadot", "Part_Roussel", "Part_Zemmour", "Part_Hidalgo")]
matriceCor <- tabMultiVarie[, c("Part_Macron", "Part_LePen", "Part_Melenchon", "Part_Pecresse", "Part_Jadot",
"Part_Roussel", "Part_Zemmour", "Part_Hidalgo")]
cor (matriceCor)
install.packages("FactoMineR")
library(FactoMineR)
# On sélectionne les variables quantitatives de la matrice et on réalise l’ACP là-dessus
resultat=PCA(tabMultiVarie[,3:10], graph=TRUE)
resultat # On affiche le résultat
resultat$eig
resultat$var$coord
plot.PCA(resultat, axes=c(1, 2), choix="var")
# On crée une variable coords qui comprend les coordonnées de chaque
observation
# On crée une variable coords qui comprend les coordonnées de chaque observation
coords_axe1 <- resultat$ind$coord[ , c("Dim.1")]
# On ajoute au tableau de données “tabMultiVarie” les résultats des
observations (= jointure)
# On crée une variable coords qui comprend les coordonnées de chaque observation
coords_axe1 <- resultat$ind$coord[ , c("Dim.1")]
# On ajoute au tableau de données “tabMultiVarie” les résultats des observations (= jointure)
tabMultiVarie <- cbind(tabMultiVarie, coords_axe1)
View(tabMultiVarie)
# On exporte le contenu de “matrice2” dans un nouveau fichier Excel (nécessite l’installation du package “writexl” si ce n’est pas déjà fait)
write_xlsx(tabMultiVarie, "E:/Licence_sig/13_R/export.xlsx")
install.packages("writexl")
library(writexl)
install.packages("writexl")
install.packages("writexl")
# On crée une variable coords qui comprend les coordonnées de chaque observation
coords_axe1 <- resultat$ind$coord[ , c("Dim.1")]
# On ajoute au tableau de données “tabMultiVarie” les résultats des observations (= jointure)
tabMultiVarie <- cbind(tabMultiVarie, coords_axe1)
View(tabMultiVarie)
# On exporte le contenu de “matrice2” dans un nouveau fichier Excel (nécessite l’installation du package “writexl” si ce n’est pas déjà fait)
write_xlsx(tabMultiVarie, "E:/Licence_sig/13_R/export.xlsx")
savehistory('E:/Licence_sig/13_R/part_2.R')
