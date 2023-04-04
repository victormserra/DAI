#Diretório, vírgulas e pacotes

setwd("C:/Users/03599284121/Desktop/QS Subject")

library(tidyverse)
library(readxl)
library(janitor)
library(ggbump)
library(openxlsx)

options(OutDec = ",")

#Pega tabela geral

QSSub_AH <- read_excel("Ranking QS Subject Dados Gerais.xlsx",sheet = "Arts and Humanities")
QSSub_LSM <- read_excel("Ranking QS Subject Dados Gerais.xlsx",sheet = "Life Sciences and Medicine")
QSSub_ET <- read_excel("Ranking QS Subject Dados Gerais.xlsx",sheet = "Engineeting & Technology")
QSSub_NS <- read_excel("Ranking QS Subject Dados Gerais.xlsx",sheet = "Natural Sciences")
QSSub_SS <- read_excel("Ranking QS Subject Dados Gerais.xlsx",sheet = "Social Sciences and Management")

# Arts. & Hum

## verifica cada variável

AH_dados_ano <- QSSub_AH %>% count(Ano)
AH_dados_universidade <- QSSub_AH %>% count(Instituição)
AH_dados_paises <- QSSub_AH %>%  count(Pais)
AH_dados_ranking <- QSSub_AH %>% count(Colocação)


##Dados para Universidade brasileira

AH_dados_gerais <- QSSub_AH          
AH_dados_gerais <- AH_dados_gerais %>% filter(Pais =="Brazil")

AH_Nacional<-  AH_dados_gerais %>%  group_by(Ano) %>% arrange(desc(`Overall Score`)) %>% mutate(`national rank` = row_number())



##Dados para Universidades Federais

AH_Federal <- AH_Nacional %>% mutate(federal_rank = ifelse(str_detect(Instituição, "Federal") | Instituição == "University of Brasília" | Instituição == "Universidade de Brasília", 1, 0))
AH_Federal <- AH_Federal %>% filter(federal_rank == 1) %>% group_by(Ano) %>% arrange(desc(`Overall Score`)) %>%  mutate(federal_rank = row_number())                                  

# Engineeting & Technology

## verifica cada variável

ET_dados_ano <- QSSub_ET %>% count(Ano)
ET_dados_universidade <- QSSub_ET %>% count(Instituição)
ET_dados_paises <- QSSub_ET %>%  count(Pais)
ET_dados_ranking <- QSSub_ET %>% count(Colocação)


##Dados para Universidade brasileira

ET_dados_gerais <- QSSub_ET
ET_dados_gerais <- ET_dados_gerais %>% filter(Pais =="Brazil")


ET_Nacional<-  ET_dados_gerais %>%  group_by(Ano) %>% arrange(desc(`Overall Score`)) %>% mutate(`national rank` = row_number())



##Dados para Universidades Federais

ET_Federal <- ET_Nacional %>% mutate(federal_rank = ifelse(str_detect(Instituição, "Federal") | Instituição == "University of Brasília" | Instituição == "Universidade de Brasília", 1, 0))
ET_Federal <- ET_Federal %>% filter(federal_rank == 1) %>% group_by(Ano) %>% arrange(desc(`Overall Score`)) %>%  mutate(federal_rank = row_number())                                  

# "Life & Med"

## verifica cada variável

LSM_dados_ano <- QSSub_LSM %>% count(Ano)
LSM_dados_universidade <- QSSub_LSM %>% count(Instituição)
LSM_dados_paises <- QSSub_LSM %>%  count(Pais)
LSM_dados_ranking <- QSSub_LSM %>% count(Colocação)


##Dados para Universidade brasileira

LSM_dados_gerais <- QSSub_LSM         
LSM_dados_gerais <- LSM_dados_gerais %>% filter(Pais =="Brazil")

LSM_Nacional<-  LSM_dados_gerais %>%  group_by(Ano) %>% arrange(desc(`Overall Score`)) %>% mutate(`national rank` = row_number())



##Dados para Universidades Federais

LSM_Federal <- LSM_Nacional %>% mutate(federal_rank = ifelse(str_detect(Instituição, "Federal") | Instituição == "University of Brasília" | Instituição == "Universidade de Brasília", 1, 0))
LSM_Federal <- LSM_Federal %>% filter(federal_rank == 1) %>% group_by(Ano) %>% arrange(desc(`Overall Score`)) %>%  mutate(federal_rank = row_number())                                  

# Natural Sciences

## verifica cada variável

NS_dados_ano <- QSSub_NS %>% count(Ano)
NS_dados_universidade <- QSSub_NS %>% count(Instituição)
NS_dados_paises <- QSSub_NS %>%  count(Pais)
NS_dados_ranking <- QSSub_NS %>% count(Colocação)

##Dados para Universidade brasileira

NS_dados_gerais <- QSSub_NS         
NS_dados_gerais <- NS_dados_gerais %>% filter(Pais =="Brazil")

NS_Nacional<-  NS_dados_gerais %>%  group_by(Ano) %>% arrange(desc(`Overall Score`)) %>% mutate(`national rank` = row_number())

##Dados para Universidades Federais

NS_Federal <- NS_Nacional %>% mutate(federal_rank = ifelse(str_detect(Instituição, "Federal") | Instituição == "University of Brasília" | Instituição == "Universidade de Brasília", 1, 0))
NS_Federal <- NS_Federal %>% filter(federal_rank == 1) %>% group_by(Ano) %>% arrange(desc(`Overall Score`)) %>%  mutate(federal_rank = row_number())                                  

# Social Sciences and Management

## verifica cada variável

SS_dados_ano <- QSSub_SS %>% count(Ano)
SS_dados_universidade <- QSSub_SS %>% count(Instituição)
SS_dados_paises <- QSSub_SS %>%  count(Pais)
SS_dados_ranking <- QSSub_SS %>% count(Colocação)

##Dados para Universidade brasileira

SS_dados_gerais <- QSSub_SS          
SS_dados_gerais <- SS_dados_gerais %>% filter(Pais =="Brazil")

SS_Nacional<-  SS_dados_gerais %>%  group_by(Ano) %>% arrange(desc(`Overall Score`)) %>% mutate(`national rank` = row_number())



##Dados para Universidades Federais

SS_Federal <- SS_Nacional %>% mutate(federal_rank = ifelse(str_detect(Instituição, "Federal") | Instituição == "University of Brasília" | Instituição == "Universidade de Brasília", 1, 0))
SS_Federal <- SS_Federal %>% filter(federal_rank == 1) %>% group_by(Ano) %>% arrange(desc(`Overall Score`)) %>%  mutate(federal_rank = row_number())                                  

# Dados para evolução nos Anos

AH_dados_ano <- QSSub_AH %>% count(Ano)
AH_Nacional_Ano <- AH_Nacional %>% count(Ano)
AH_Federal_Ano <- AH_Federal %>% count(Ano)

ET_dados_ano <- QSSub_ET %>% count(Ano)
ET_Nacional_Ano <- ET_Nacional %>% count(Ano)
ET_Federal_Ano <- ET_Federal %>% count(Ano)

LSM_dados_ano <- QSSub_LSM %>% count(Ano)
LSM_Nacional_Ano <- LSM_Nacional %>% count(Ano)
LSM_Federal_Ano <- LSM_Federal %>% count(Ano)

NS_dados_ano <- QSSub_NS %>% count(Ano)
NS_Nacional_Ano <- NS_Nacional %>% count(Ano)
NS_Federal_Ano <- NS_Federal %>% count(Ano)

SS_dados_ano <- QSSub_SS %>% count(Ano)
SS_Nacional_Ano <- SS_Nacional %>% count(Ano)
SS_Federal_Ano <- SS_Federal %>% count(Ano)

# salvar

list_of_datasets <- list("Arts. & Hum" = QSSub_AH,
                         "Arts. & Hum Nacional" = AH_Nacional,
                         "Arts. & Hum Federais" = AH_Federal,
                         "Eng. & Tech" = QSSub_ET,
                         "Eng. & Tech Nacional" = ET_Nacional,
                         "Eng. & Tech Federais" = ET_Federal,
                         "Life & Med" = QSSub_LSM,
                         "Life & Med Nacional" = LSM_Nacional,
                         "Life & Med Federais" = LSM_Federal,
                         "Natural Sciences" = QSSub_NS,
                         "Natural Sciences Nacional" = NS_Nacional,
                         "Natural Sciences Federais" = NS_Federal,
                         "Social Sciences" = QSSub_SS,
                         "Social Sciences Nacional" = SS_Nacional,
                         "Social Sciences Federais" = SS_Federal)

write.xlsx(list_of_datasets, file = "QS_Subject_Final_27032023.xlsx")

list_of_datasets_AH <- list("Arts. & Hum" = QSSub_AH,
                         "Arts. & Hum Nacional" = AH_Nacional,
                         "Arts. & Hum Federais" = AH_Federal)

write.xlsx(list_of_datasets_AH, file = "QS_Subject_AH_Final_27032023.xlsx")

list_of_datasets_ET <- list("Eng. & Tech" = QSSub_ET,
                         "Eng. & Tech Nacional" = ET_Nacional,
                         "Eng. & Tech Federais" = ET_Federal)

write.xlsx(list_of_datasets_ET, file = "QS_Subject_ET_Final_27032023.xlsx")

list_of_datasets_LSM <- list("Life & Med" = QSSub_LSM,
                         "Life & Med Nacional" = LSM_Nacional,
                         "Life & Med Federal" = LSM_Federal)

write.xlsx(list_of_datasets_LSM, file = "QS_Subject_LSM_Final_27032023.xlsx")


list_of_datasets_NS <- list("Natural Sciences" = QSSub_NS,
                         "Natural Sciences Nacional" = NS_Nacional,
                         "Natural Sciences Federais" = NS_Federal)

write.xlsx(list_of_datasets_NS, file = "QS_Subject_NS_Final_27032023.xlsx")


list_of_datasets_SS <- list("Social Sciences" = QSSub_SS,
                         "Social Sciences Nacional" = SS_Nacional,
                         "Social Sciences Federais" = SS_Federal)

write.xlsx(list_of_datasets_SS, file = "QS_Subject_SS_Final_27032023.xlsx")

list_of_datasets_Ano <- list("AH_ANo" = AH_dados_ano, 
                             "AH_Ano_Nacional" = AH_Nacional_Ano,
                             "AH_Ano_Federal" = AH_Federal_Ano,
                             "ET_ANo" = ET_dados_ano, 
                             "ET_Ano_Nacional" = ET_Nacional_Ano,
                             "ET_Ano_Federal" = ET_Federal_Ano,
                             "LSM_ANo" = LSM_dados_ano, 
                             "LSM_Ano_Nacional" = LSM_Nacional_Ano,
                             "LSM_Ano_Federal" = LSM_Federal_Ano,
                             "NS_ANo" = NS_dados_ano, 
                             "NS_Ano_Nacional" = NS_Nacional_Ano,
                             "NS_Ano_Federal" = NS_Federal_Ano,
                             "SS_ANo" = SS_dados_ano, 
                             "SS_Ano_Nacional" = SS_Nacional_Ano,
                             "SS_Ano_Federal" = SS_Federal_Ano)
                             
write.xlsx(list_of_datasets_Ano, file = "THE_Evolução_Ano__27032023.xlsx")

