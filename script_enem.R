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

##### 2. PRE-PROCESSAMENTO DOS DADOS ####

##### 2.1 normalizando valores da coluna SEXO ####

##verificando valores da coluna SEXO
table(enem$SEXO)

##normalizando valores da coluna SEXO
enem$SEXO <- gsub("1","FEMININO",enem$SEXO)
enem$SEXO <- gsub("^F$","FEMININO",enem$SEXO)
enem$SEXO <- gsub("0|^M$","MASCULINO",enem$SEXO)

table(enem$SEXO)

##### 2.2 normalizando coluna TIPO_LINGUA ####

##verificando valores da coluna TIPO_LINGUA
table(enem$TIPO_LINGUA)

##corrigindo valores diferentes da coluna TIPO_LINGUA
enem$TIPO_LINGUA <- gsub("0","INGLÊS",enem$TIPO_LINGUA)
enem$TIPO_LINGUA <- gsub("1","ESPANHOL",enem$TIPO_LINGUA)
table(enem$TIPO_LINGUA)

##VERIFICANDO coluna UF_PROVA
table(enem$UF_PROVA)
length(table(enem$UF_PROVA))

##### 2.3 normalizando valores da coluna SITUACAO_CONCLUSAO ####

##verificando coluna SITUACAO_CONCLUSAO
table(enem$SITUACAO_CONCLUSAO)

##convertendo valor da coluna SITUACAO_CONCLUSAO
enem$SITUACAO_CONCLUSAO <- gsub("1","CONCLUIDO", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("2","CONCLUIRÁ NO ANO", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("3","CONCLUIRÁ APÓS(ANO)", enem$SITUACAO_CONCLUSAO)
enem$SITUACAO_CONCLUSAO <- gsub("4","NÃO CONC. NÃO CURSANDO", enem$SITUACAO_CONCLUSAO)

table(enem$SITUACAO_CONCLUSAO)

##### 2.4 normalizando valores das colunas NOTAS (CH,CN,LC,MT,REDACAO) ####

##verificando a coluna NOTA_CIENCIAS_HUMANAS
summary(enem$NOTA_CIENCIAS_HUMANAS)

#convertendo string em numero
as.numeric('4')
as.numeric('2.5')
as.numeric('A')
as.numeric('.')

enem$NOTA_CIENCIAS_NATUREZA  <- as.numeric(enem$NOTA_CIENCIAS_NATUREZA)
enem$NOTA_CIENCIAS_HUMANAS   <- as.numeric(enem$NOTA_CIENCIAS_HUMANAS)
enem$NOTA_LINGUAGENS_CODIGOS <- as.numeric(enem$NOTA_LINGUAGENS_CODIGOS)
enem$NOTA_MATEMATICA         <- as.numeric(enem$NOTA_MATEMATICA)
enem$NOTA_REDACAO            <- as.numeric(enem$NOTA_REDACAO)

summary(enem$NOTA_CIENCIAS_HUMANAS)

#convertendo string em número com DPLYR
#enem <- enem %>% mutate(NOTA_CIENCIAS_HUMANAS = as.numeric(NOTA_CIENCIAS_HUMANAS),
 #                       NOTA_CIENCIAS_NATUREZA = as.numeric(NOTA_CIENCIAS_NATUREZA),
  #                      NOTA_LINGUAGENS_CODIGOS = as.numeric(NOTA_LINGUAGENS_CODIGOS),
   #                     NOTA_MATEMATICA = as.numeric(NOTA_MATEMATICA),
    #                    NOTA_REDACAO = as.numeric(NOTA_REDACAO))

summary(enem$NOTA_CIENCIAS_HUMANAS)
summary(enem$NOTA_LINGUAGENS_CODIGOS)
summary(enem$NOTA_MATEMATICA)

str(enem)
