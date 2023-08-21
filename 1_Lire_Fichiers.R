#############################################################################
#R version 3.5
# Information sur la session
# sessionInfo()
# Program 
#############################################################################
# attention certains fichiers Insee sont xls ou xlsx selon les mois 
# jusqu'à septembre 2022 modification des noms de colones faites à la main en xls
# après commandes dans le programmes


library(tidyverse)
library(data.table)
library(zoo)
library(rdbnomics)
library(lubridate)
library(readxl)

rm(list = ls())

setwd("//intra/partages/AU_AMIC/LPR_2022_SAMIC/6-Entrées")

#############################################################################
# Load data : une table par date
#############################################################################

#############################################################################
#import tf106
#############################################################################
tf106_3digit  <- read_xlsx("//intra/partages/AU_AMIC/LPR_2022_SAMIC/Programme/TF106_3digit.xlsx", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))

tf106_3digit2 <- tf106_3digit %>%  filter(str_length(IDENT) ==4) %>%
  filter(!IDENT %in% c("0321","0322","0431","0432","0441","0442","0443","0444","0511","0512","0513","0531","0532","0533","0611","0612","0613","0731","0732","0733","0734","0735","0736",
                       "0911","0912","0913","0914","0915","0921","0922","0923","0931","0932","0933","0934","0935","1211","1212","1213"))  

tf106_2digit<-tf106_3digit %>%
  filter(str_length(IDENT) ==3 )  %>%
  filter(IDENT %in% c("032","043","044","051","053","061","073","091","092","093","121"))  

tf106_23digit<-rbind(tf106_2digit,tf106_3digit2)
tf106_23digit

#############################################################################
#2021 #ajouter la pondération 2020 et l'indice 2020_12 #nombre de colone
#############################################################################

#2019 # Indice_2019_12

ipc_2020_12 <- read_xlsx("Indices_detaillés_2020_12.xlsx", col_types =c("text","text","numeric","numeric","numeric"))
ipc_2019_12_ext<-subset(ipc_2020_12,select = c(IDENT, Indice_2019_12))


ipc_2021_01 <- read_xls("Indices_detaillés_2021_01.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_01 <- merge(ipc_2021_01  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_02 <- read_xls("Indices_detaillés_2021_02.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_02 <- merge(ipc_2021_02  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_03 <- read_xls("Indices_detaillés_2021_03.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_03 <- merge(ipc_2021_03  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_04 <- read_xls("Indices_detaillés_2021_04.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_04 <- merge(ipc_2021_04  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_05 <- read_xls("Indices_detaillés_2021_05.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_05 <- merge(ipc_2021_05  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_06 <- read_xls("Indices_detaillés_2021_06.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_06 <- merge(ipc_2021_06  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_07 <- read_xls("Indices_detaillés_2021_07.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_07 <- merge(ipc_2021_07  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_08 <- read_xls("Indices_detaillés_2021_08.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_08 <- merge(ipc_2021_08  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_09 <- read_xls("Indices_detaillés_2021_09.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_09 <- merge(ipc_2021_09  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_10 <- read_xls("Indices_detaillés_2021_10.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_10 <- merge(ipc_2021_10  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_11 <- read_xls("Indices_detaillés_2021_11.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_11 <- merge(ipc_2021_11  ,ipc_2019_12_ext,by="IDENT")

ipc_2021_12 <- read_xls("Tableau_Données_Détaillées_2021-12.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2021_12 <- merge(ipc_2021_12  ,ipc_2019_12_ext,by="IDENT")


# Indice202012
ipc_2020_12_ext<-subset(ipc_2021_01,select = c(IDENT, Indice_2020_12))

#############################################################################
#2022
#############################################################################
#Ponderation2021 Indice202112
ipc_2021_12_ext<-subset(ipc_2021_12,select = c(IDENT, Pondération2021,Indice_2021_12))
ipc_2021_12_pond<-subset(ipc_2021_12,select = c(IDENT, Pondération2021))


ipc_2022_01 <- read_xls("Tableau_Données_Détaillées_2022-01.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2022_01 <- merge(ipc_2022_01  ,ipc_2020_12_ext,by="IDENT")
ipc_2022_02 <- read_xls("Tableau_Données_Détaillées_2022-02.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2022_02 <- merge(ipc_2022_02  ,ipc_2020_12_ext,by="IDENT")

ipc_2022_03 <- read_xls("Tableau_Données_Détaillées_2022-03v2.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2022_03 <- merge(ipc_2022_03  ,ipc_2020_12_ext,by="IDENT")

ipc_2022_04 <- read_xls("Tableau_Données_Détaillées_2022-04.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2022_04 <- merge(ipc_2022_04  ,ipc_2020_12_ext,by="IDENT")

ipc_2022_05 <- read_xls("Tableau_Données_Détaillées_2022-05.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2022_05 <- merge(ipc_2022_05  ,ipc_2020_12_ext,by="IDENT")

ipc_2022_06 <- read_xls("Indices_detaillés_2022_06.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2022_06 <- merge(ipc_2022_06  ,ipc_2020_12_ext,by="IDENT")

#pb ipc_tf106_2022_10_w 'Pondération2021' introuvable

ipc_2022_07 <- read_xls("Indices_detaillés_2022_07.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2022_07 <- merge(ipc_2022_07  ,ipc_2020_12_ext,by="IDENT")

ipc_2022_08 <- read_xls("Indices_detaillés_2022_08.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2022_08 <- merge(ipc_2022_08  ,ipc_2020_12_ext,by="IDENT")

ipc_2022_09 <- read_xls("Indices_detaillés_2022_09.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"))
ipc_2022_09 <- merge(ipc_2022_09  ,ipc_2020_12_ext,by="IDENT")

ipc_2022_10 <- read_xls("Tableau_Données_Détaillées_2022-10.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),.name_repair = function(col){ gsub(" ", "", col) } )
names(ipc_2022_10) <- sub("-", "_", names(ipc_2022_10))
names(ipc_2022_10) <- sub("Indice", "Indice_", names(ipc_2022_10)) 
ipc_2022_10 <-merge(ipc_2022_10 ,ipc_2021_12_ext,by="IDENT") 
ipc_2022_10 <- merge(ipc_2022_10  ,ipc_2020_12_ext,by="IDENT")
glimpse(ipc_2022_10)


ipc_2022_11 <- read_xls("Tableau_Données_Détaillées_2022-11.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),.name_repair = function(col){ gsub(" ", "", col) } )
names(ipc_2022_11) <- sub("Indice", "Indice_", names(ipc_2022_11)) 
names(ipc_2022_11) <- sub("-", "_", names(ipc_2022_11))
ipc_2022_11 <-merge(ipc_2022_11 ,ipc_2021_12_ext,by="IDENT")
ipc_2022_11 <- merge(ipc_2022_11  ,ipc_2020_12_ext,by="IDENT")

ipc_2022_12 <- read_xls("Tableau_Données_Détaillées_2022-12.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),.name_repair = function(col){ gsub(" ", "", col) } )

#Ponderation2022 Indice202112
ipc_2022_12_ext<-subset(ipc_2022_12,select = c(IDENT, Pondération2022,Indice_2022_12))
ipc_2022_12_pond<-subset(ipc_2022_12,select = c(IDENT, Pondération2022))
ipc_2022_12 <- merge(ipc_2022_12  ,ipc_2020_12_ext,by="IDENT")




#############################################################################
#2023
#############################################################################

ipc_2023_01 <- read_xlsx("Tableau_Données_Détaillées_2023-01.xlsx", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),.name_repair = function(col){ gsub(" ", "", col) })
ipc_2023_02 <- read_xlsx("Tableau_Données_Détaillées_2023-02.xlsx", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),.name_repair = function(col){ gsub(" ", "", col) })
names(ipc_2023_02) <- sub("Indice", "Indice_", names(ipc_2023_02)) 
ipc_2023_02 <- merge(ipc_2023_02 ,ipc_2022_12_pond,by="IDENT") 
ipc_2023_02 <- merge(ipc_2023_02 ,ipc_2021_12_ext,by="IDENT")
names(ipc_2023_02) <- sub("-", "_", names(ipc_2023_02))

ipc_2023_03 <- read_xlsx("Tableau_Données_Détaillées_2023-03.xlsx", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),.name_repair = function(col){ gsub(" ", "", col) })
names(ipc_2023_03) <- sub("Indice", "Indice_", names(ipc_2023_03)) 
ipc_2023_03 <- merge(ipc_2023_03 ,ipc_2022_12_pond,by="IDENT") 
ipc_2023_03 <- merge(ipc_2023_03 ,ipc_2021_12_ext,by="IDENT")
names(ipc_2023_03) <- sub("-", "_", names(ipc_2023_03))

ipc_2023_04 <- read_xls("Tableau_Données_Détaillées_2023-04.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),.name_repair = function(col){ gsub(" ", "", col) })

ipc_2023_05 <- read_xls("Tableau_Données_Détaillées_2023-05.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),.name_repair = function(col){ gsub(" ", "", col) })
names(ipc_2023_05) <- sub("Indice", "Indice_", names(ipc_2023_05)) 
ipc_2023_05 <-merge(ipc_2023_05 ,ipc_2022_12_ext,by="IDENT")
ipc_2023_05 <-merge(ipc_2023_05 ,ipc_2021_12_ext,by="IDENT")
names(ipc_2023_05) <- sub("-", "_", names(ipc_2023_05))


ipc_2023_06 <- read_xls("Tableau_Données_Détaillées_2023-06.xls", col_types =c("text","text","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric"),.name_repair = function(col){ gsub(" ", "", col) })
names(ipc_2023_06) <- sub("Indice", "Indice_", names(ipc_2023_06)) 
ipc_2023_06 <-merge(ipc_2023_06 ,ipc_2022_12_ext,by="IDENT")
ipc_2023_06 <-merge(ipc_2023_06 ,ipc_2021_12_ext,by="IDENT")
names(ipc_2023_06) <- sub("-", "_", names(ipc_2023_06))

#############################################################################
## MAJ ICI
#############################################################################