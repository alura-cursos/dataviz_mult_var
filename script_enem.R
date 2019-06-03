##### 1. INTRODUÇÃO, IMPORTAÇÃO E SELEÇÃO DOS DADOS ####

##### 1.1 INTRODUÇÃO ####

##### 1.2 IMPORTANDO E MERGE DOS DADOS ####
getwd()
setwd("alura/dataviz_mult_var")
setwd("Github/alura/dataviz_mult_var")

library(data.table)
library(dplyr)
library(ggplot2)

enem_2010 <- fread("enem_2010.csv",encoding = 'UTF-8')

enem_2011 <- fread("enem_2011.csv",encoding = 'UTF-8')

enem_2012 <- fread("enem_2012.csv",encoding = 'UTF-8')

enem_2013 <- fread("enem_2013.csv",encoding = 'UTF-8')

enem_2014 <- fread("enem_2014.csv",encoding = 'UTF-8')

enem_2015 <- fread("enem_2015.csv",encoding = 'UTF-8')

enem_2016 <- fread("enem_2016.csv",encoding = 'UTF-8')

enem_2017 <- fread("enem_2017.csv",encoding = 'UTF-8')


## SIMULANDO ERRO DE MERGE
merge_enem <- rbind(enem_2010,enem_2011,enem_2012,enem_2013,enem_2014,enem_2015,enem_2016,enem_2017)

## MERGE CORRETO DE TODOS OS CJ DE DADOS 
merge_enem <- rbind(enem_2010,enem_2011,enem_2012,enem_2013,enem_2014,enem_2015,enem_2016,enem_2017, fill = T)

## apagando conj de dados que não serão mais utilizado. Recomendável para não consumir tanta memória do RSTUDIO
rm(enem_2010,enem_2011,enem_2012,enem_2013,enem_2014,enem_2015,enem_2016,enem_2017)


##### 1.3 SELEÇÃO DOS DADOS ####

view(merge_enem)

## criando vetor com os nome das colunas desejada
colunas <- c("NUMERO_INSCRICAO","ANO","CO_MUNICIPIO_RESIDENCIA","MUNICIPIO_RESIDENCIA",
             "UF_RESIDENCIA","UF_ESCOLA","IDADE","SEXO","SITUACAO_CONCLUSAO","BRAILLE","MUNICIPIO_PROVA",
             "UF_PROVA","PRESENCA_CIENCIAS_NATUREZA","PRESENCA_CIENCIAS_HUMANAS","PRESENCA_LINGUAGENS_CODIGOS",
             "PRESENCA_MATEMATICA","NOTA_CIENCIAS_NATUREZA","NOTA_CIENCIAS_HUMANAS",
             "NOTA_LINGUAGENS_CODIGOS","NOTA_MATEMATICA","TIPO_LINGUA","STATUS_REDACAO","NOTA_REDACAO")

## selecionando algumas colunas
enem <- merge_enem %>% 
          select_(.dots = colunas)

##### 1.4 CONHECENDO A BASE DE DADOS ####
View(enem)

##verificando as colunas
str(enem)