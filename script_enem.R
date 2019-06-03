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

##### 3. GRÁFICO DE COLUNAS ####

##### 3.1 GRÁFICO DE SEXO E TIPO_LINGUA ####

##grafico de barra de quantidade de registros por TIPO_LINGUA
ggplot(data = enem) + 
  geom_bar(aes(x = TIPO_LINGUA), stat = 'count')

##desabilitando notação científica
options(scipen = 9999)

##excluindo registros que possuem '.' na coluna TIPO_LINGUA
tp_lingua_sexo <- enem %>% 
                    filter(TIPO_LINGUA != '.') %>% 
                    select_(.dots = c('SEXO','TIPO_LINGUA'))

##grafico de barra de quantidade de registros por SEXO/TIPO_LINGUA com barras sobrepostas
ggplot(data = tp_lingua_sexo) + 
  geom_bar(aes(x = SEXO, fill = TIPO_LINGUA), stat = 'count')


##grafico de colunas sobre a quantidade de registros por SEXO/TIPO_LINGUA com barras ao lado
plot_idioma_sexo <- ggplot(data = tp_lingua_sexo) + 
                      geom_bar(aes(x = SEXO, fill = TIPO_LINGUA), stat = 'count',position =  position_dodge())

plot_idioma_sexo

##### 3.2 Personalizando Gráficos de Colunas (plot_idioma_sexo) ####

## inserindo título e alterando rótulos dos eixos X e Y
p <- plot_idioma_sexo + 
  ggtitle("Idioma por Sexo") +
  ylab("Quantidade") + xlab("Sexo")
p

## alterando título legenda e cor layout do gráfico
p <- p + theme_linedraw() + 
  theme(plot.title = element_text(hjust = 0.5))
p

plot_idioma_sexo <- p

plot_idioma_sexo

##### 3.3 GRÁFICO COLUNA DAS COLUNAS UF_PROVA E SITUACAO_CONCLUSAO ####

## GRÁfico de colunas para UF_PROVA
ggplot(data = enem) + geom_bar(aes(x = UF_PROVA))

## eliminando registros que possuem vazio na coluna UF_PROVA
uf_prova <- enem %>% 
              filter(UF_PROVA != '') %>% 
              select_(.dots = c('UF_PROVA','SITUACAO_CONCLUSAO'))

## simulando erro no AES(x)
ggplot(data = uf_prova) + 
  geom_bar(aes(x = uf_prova))

## grafico de barras UF_PROVA/SITUACAO_CONCLUSAO
ggplot(data = uf_prova) + 
  geom_bar(aes(x = UF_PROVA,fill = SITUACAO_CONCLUSAO))


## grafico de colunas da coluna UF_PROVA/SITUACAO_CONCLUSAO COM FACET_GRID
plot_uf_conclusao <- ggplot(data = uf_prova) + 
                      geom_bar(aes(x = UF_PROVA,fill = SITUACAO_CONCLUSAO),position = position_dodge()) +
                      facet_grid(SITUACAO_CONCLUSAO~.)

plot_uf_conclusao

##### 3.4 Personalizando Gráficos de Colunas (plot_uf_conclusao) ####

## inserindo título, alterando rótulos eixos X e Y
p <- plot_uf_conclusao + 
      ggtitle("Situação Escolar por Estado ") + 
      ylab("Quantidade") + xlab("Estado")
p

## alterando título da legenda e cor layout do gráfico
 p + theme_linedraw() + 
     labs(fill = 'Situação') + 
     theme(plot.title = element_text(hjust = 0.5))

plot_uf_conclusao <- p

plot_uf_conclusao

##### 4. GRÁFICO DE BARRAS (HORIZON) E PIRAMIDE####

##### 4.1 GRÁfico de barras MEDIA IDADE POR UF_PROVA ####

##VERIFICANDO COLUNA IDADE
summary(enem$IDADE)

## FILTRANDO REGISTROS COM PROBLEMA NA COLUNA IDADE
idade_uf <- enem %>% 
  filter(!is.na(IDADE) )
  
summary(idade_uf$IDADE)

## FILTRANDO REGISTROS COM PROBLEMA NA COLUNA UF_PROVA selecionando apenas as colunas UF_PROVA, IDADE
idade_uf <- idade_uf %>% 
            filter(UF_PROVA != '') %>% 
            select_(.dots = c('UF_PROVA','SEXO','IDADE'))

## calculando media idade por UF_PROVA e salvando em um novo conjunto de dados
media_idade_uf <- idade_uf %>%
                  group_by(UF_PROVA) %>%
                  summarise(media = mean(IDADE))

## media idade por UF sem ordenação
ggplot(data = media_idade_uf) + 
  geom_bar(aes(x = UF_PROVA, y = media), position = position_dodge(), stat = 'identity') + 
  coord_flip()

## media idade por UF com ordenação
ggplot(data = media_idade_uf) + geom_bar(aes(x = reorder(UF_PROVA,media), y = media), position = position_dodge(), stat = 'identity') + 
                      coord_flip()

##### 4.2 GRÁfico de barras (PIRAMIDE) MEDIA IDADExSEXO/UF_PROVA ####

## calculando media idade por UF_PROVA e SEXO e salvando novo conj de dados
media_idade_sexo_uf <- idade_uf %>%
                        group_by(UF_PROVA,SEXO) %>%
                        summarise(media = mean(IDADE))

## media idade por SEXO/UF 
ggplot(data = media_idade_sexo_uf,aes(x = UF_PROVA, y = media,fill = SEXO) ) + 
  geom_bar(position = position_dodge(), stat = 'identity') + 
  coord_flip()

## media idade por SEXO/UF no formato 'piramide'
plot_piram_idade <- ggplot(data = media_idade_sexo_uf,
                           aes(x = reorder(UF_PROVA,-media), y = ifelse(SEXO =='MASCULINO', -media,media),fill = SEXO)) + 
                      geom_bar( stat = 'identity') + 
                      coord_flip() + 
                      scale_y_continuous(labels = abs)

plot_piram_idade

##### 4.3 Personalizando Gráficos de barras (plot_piram_idade) ####

p <- plot_piram_idade + ggtitle("Media de Idade por UF e Sexo") + 
      ylab("Média Idade") + xlab("Estado") + 
      theme_bw() + 
      theme(plot.title = element_text(hjust = 0.5))

## definindo cores para cada tipo de categoria (masculino e feminino)
p <- p + scale_fill_manual(values = c("hotpink","dodgerblue3"))
p

## inserindo rótulos nas barras 
#p <- 
p + geom_text(aes(label = round(media,digits = 2), # definindo o texto
                       hjust = 0.5 #posição horizonta (eixo x) do texto
                      ), 
                      size = 4.5, # tamanho do texto
                      colour = 'black', # cor do texto
                      fontface = 'bold' # tipo do texto
                      )


plot_piram_idade <- p
plot_piram_idade

##### 5. GRÁficos de pontos (scatter plot) ####

##### 5.1 GRÁFICO SCATTER IDADE/MEDIA NOTA ####

##filtrando registros que tem número na coluna NOTA_CIENCIAS_HUMANAS
notas_ciencias_humanas <- enem %>% 
                            filter(!is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(IDADE) & IDADE > 17)

##calculando media da NOTA_CIENCIAS_HUMANAS por IDADE 
notas_ciencias_humanas_idade <- notas_ciencias_humanas %>% 
                                  group_by(IDADE) %>% 
                                  summarise(media_nota_ciencias_humanas = mean(NOTA_CIENCIAS_HUMANAS))

##grafico de pontos para IDADE e media da NOTA_CIENCIAS_HUMANAS
ggplot(data = notas_ciencias_humanas_idade) + 
  geom_point(aes(x = IDADE, y = media_nota_ciencias_humanas))


#### ---------------------------#

## NOTA_MATEMATICA
##filtrando registros que tem número na coluna NOT_CN
notas_mt <- enem %>% 
              filter(!is.na(NOTA_MATEMATICA) & !is.na(IDADE) & IDADE > 17)

##calculando media da NOTA_CIENCIAS_HUMANAS por IDADE 
notas_matematica_idade <- notas_mt %>% 
  group_by(IDADE) %>% 
  summarise(media_nota_matematica = mean(NOTA_MATEMATICA))

##grafico de pontos para IDADE e media da NOTA_MATEMATICA
ggplot(data = notas_matematica_idade) + 
  geom_point(aes(x = IDADE, y = media_nota_matematica))



##### 5.2 GRÁFICO SCATTER IDADE/MEDIA NOTA - II ####
View(notas_ciencias_humanas_idade)
View(notas_matematica_idade)

## merge data frame pelas linhas 
notas_ciencias_humanas_matematica_idade <- merge(notas_ciencias_humanas_idade,notas_matematica_idade,by = 'IDADE',all = T)

#fazendo uma transposição linhas x colunas da tabela
#install.packages("reshape2")
library(reshape2)
notas_ciencias_humanas_matematica_idade <- melt(notas_ciencias_humanas_matematica_idade,id.vars = 'IDADE')

plot_scatter_mt_ch <- ggplot(data = notas_ciencias_humanas_matematica_idade) + 
  geom_point(aes(IDADE, value,color = variable))

plot_scatter_mt_ch

##### 5.3 Personalizando Gráficos de pontos (plot_scatter_mt_ch) ####

p <- plot_scatter_mt_ch + ggtitle('Média Notas por Idade e Matéria') + xlab('Idade') + ylab('Nota (média)')

##alterando layout do gráfico
p <- p + theme_bw()

## alterando título e rótulos da legenda
p <- p + scale_color_manual(name = 'Matéria',values = c('blue','red'), labels = c('Ciência\nHumanas','Matemática'))
p

plot_scatter_mt_ch <- p
plot_scatter_mt_ch

##### 6. GRÁFICO DE LINHA ####

##### 6.1 GRÁFICO DE LINHA COM MEDIAS DAS NOTAS POR ANO ####

media_anos <- enem %>% filter(!is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(NOTA_CIENCIAS_NATUREZA) &
                                !is.na(NOTA_MATEMATICA) & !is.na(NOTA_LINGUAGENS_CODIGOS) & 
                                !is.na(NOTA_REDACAO)) %>% 
              group_by(ANO) %>% 
              summarise(media_cn = mean(NOTA_CIENCIAS_NATUREZA), media_ch = mean(NOTA_CIENCIAS_HUMANAS),
                                          media_mt = mean(NOTA_MATEMATICA), media_lc = mean(NOTA_LINGUAGENS_CODIGOS),
                                          media_red = mean(NOTA_REDACAO))
View(media_anos)

## GRÁfico de linhas com média de notas por ano
ggplot(data = media_anos) + 
  geom_line(aes(x = ANO, y = media_cn), color = "green") +
  geom_line(aes(x = ANO, y = media_ch), color = "blue")

## fazendo uma transposicao no conjunto de dados
media_anos_2 <- melt(data = media_anos,id.vars = 'ANO')

View(media_anos_2)

plot_line_notas <- ggplot(data = media_anos_2) + 
  geom_line(aes(x = ANO, y = value, color = variable))

plot_line_notas

##### 6.2 Personalizando Gráfico de linhas (plot_line_notas) ####

## adicionando pontos nas coordenadas exatas
p <- plot_line_notas + ggtitle('Média Nota por Matéria') + ylab("Média") +
  geom_point(aes(ANO, value, color = variable), size = 3)

## inserindo labels (media) em cada ponto
p <- p + geom_text(aes(x = ANO, y = value, color = variable,
                       label = round(value,digits = 2), # definindo o texto
                       hjust = -0.15, #posição horizonta (eixo x) do texto,
                       vjust = 0.2
)#fim aes()
)#fim geom_text

## Alterando título e nomes na legenda 
p <- p + scale_color_discrete(name = 'Matérias', labels = c("Ciênc. Natureza","Ciênc. Humanas","Matemát.","Letras/Códig.","Redação")) +
  theme_bw()

plot_line_notas <- p
plot_line_notas

##### 7. GRÁFICO DE BOLHAS ####

##### 7.1 GRÁFICO DE BOLHAS MEDIAS NOTAS MT, CH, REDACAO/UF_PROVA ####

##filtrando registros que tem valores NA nas colunas das NOTAS e IDADE, e filtrando IDADE maior que 14,
## selecionando apenas algumas UF_PROVA, e calculando média das NOTAS por UF
notas_matematica_redacao <- enem %>% 
                            filter(!is.na(NOTA_MATEMATICA) & !is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(NOTA_REDACAO) & 
                                   !is.na(IDADE) & IDADE > 17 & UF_PROVA %in% c('CE','DF','MG','RS')) %>%
                              group_by(IDADE,UF_PROVA) %>% 
                              summarise(media_nota_matematica = mean(NOTA_MATEMATICA),
                                        media_nota_ciencias_humanas = mean(NOTA_CIENCIAS_HUMANAS),
                                        #media_nota_lc = mean(NOTA_LINGUAGENS_CODIGOS),
                                        media_nota_reda = mean(NOTA_REDACAO))

##GRÁfico de bolha por IDADE, media NOTA_MATEMATICA e media_redacao
plot_bolhas_uf_notas <- ggplot(data = notas_matematica_redacao) + 
                          geom_point(aes(x = media_nota_ciencias_humanas, y = media_nota_matematica,color = UF_PROVA,size = media_nota_reda), alpha = .5)

plot_bolhas_uf_notas

##### 7.2 Personalizando Gráfico de bolhas (plot_bolhas_uf_notas) ####

## alterando rótulos eixos e legenda
p <- plot_bolhas_uf_notas + ggtitle("Médias Matemát, CH e redação")+
      xlab('Média Nota Ciênc. Hum.') + ylab('Média Nota Matemát.') +
      labs(color = 'UF Prova',size = 'Média Nota Redação')

## alterando layout gráfico
p <- p + theme_bw() + theme(legend.position = 'bottom')

## alterando posição da legenda
p

plot_bolhas_uf_notas <- p
plot_bolhas_uf_notas

##### 8. GRÁFICO DE CAIXA (BOXPLOT) ####

##### 8.1 BOXPLOT PARA NOTA_REDACAO/UF_PROVA ####

## boxplot NOTA_REDACAO geral (com erro)
ggplot(data = enem) + 
  geom_boxplot(aes(y = NOTA_REDACAO))

## filrando registros coluna NOTA_REDACAO com NA
notas_redacao_uf <- enem %>% 
                      filter(UF_PROVA!= '' & !is.na(NOTA_REDACAO) )

## boxplot NOTA_REDACAO por UF_PROVA
plot_box_uf_redacao <- ggplot(data = notas_redacao_uf) + 
                        geom_boxplot(aes(x = UF_PROVA,y = NOTA_REDACAO))
plot_box_uf_redacao

##### 8.2 Personalizando Gráfico boxplot (plot_box_uf_redacao) ####
dados <- plot_box_uf_redacao$data

dados <- dados %>%
          mutate(filial = if_else(UF_PROVA %in% c('CE','DF','MG','RS'), T, F )) %>%
          select_(.dots = c("UF_PROVA","NOTA_REDACAO", "filial"))

## destacando alguns estados, e mudando a cor e tamanho dos outliers
plot_box_uf_redacao
p <- ggplot(data = dados)  + 
      geom_boxplot(aes(x = UF_PROVA, y = NOTA_REDACAO,fill = filial), outlier.colour = 'red',outlier.size = 3.5)

## alterando rótulos X e Y, e layout 
p <- p + xlab('UF Prova') + ylab('Nota Redação') + 
  theme_bw()

## destacando UFs que a instituição tem filiais e removendo legenda
p <- p + scale_fill_manual(name = '',values = c('chocolate3','chartreuse3'), labels = c('Sem Filial','Com Filial'))

plot_box_uf_redacao <- p
plot_box_uf_redacao


##### 9. COMBINANDO GRÁFICO ####

##### 9.1 COMBINANDO GRÁFICO BARRAS COM LIMITES DE ERRO ####
media_redacao <- enem %>% 
                  filter(UF_PROVA != '' & !is.na(NOTA_REDACAO) ) %>%
                         #& !is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(NOTA_CIENCIAS_HUMANAS) & !is.na(NOTA_LINGUAGENS_CODIGOS) & !is.na(NOTA_MATEMATICA)) %>%
                  mutate(media_nacional = mean(NOTA_REDACAO)) %>%
                  group_by(UF_PROVA,media_nacional) %>% 
                  summarise(media_uf = mean(NOTA_REDACAO))

plot_bar_erro <- ggplot(data = media_redacao,aes(x = reorder(UF_PROVA,media_uf), y = media_uf)) +
                  geom_errorbar(aes(ymin=media_nacional/2, ymax = media_nacional),size = 1) +
                  geom_bar(stat = 'identity') +
                  coord_flip()

plot_bar_erro



##### 9.1 Personalizando Gráfico barras com valor de 'erro' (plot_bar_erro) ####

## acessando os dados que montam o gráfico
dados <- plot_bar_erro$data
dados

## criando nova coluna para indicar se a UF_PROVA faz parte das unidades da instituição 
dados <- dados %>%
          mutate(filial = if_else(UF_PROVA %in% c('CE','DF','MG','RS'), T, F ))
View(dados)

## plotando o gráfico novamente alterando rótulos, título...
p <- ggplot(data = dados,aes(x = reorder(UF_PROVA,media_uf), y = media_uf)) +
  geom_errorbar(aes(ymin=media_nacional/2, ymax = media_nacional),size = 0.8) +
  geom_bar(aes(fill = filial),stat = 'identity') +
  coord_flip() + 
  guides(fill=FALSE) +
  ggtitle('Média Nota Redação por UF') + xlab('UF Prova') + ylab('Média Redacao') +
  theme_bw()

plot_bar_erro <- p
plot_bar_erro

##### 10 Inserindo todos os gráficos em uma única janela ####

##### 10.1 Configurando o layout da página ####
plot_idioma_sexo
plot_uf_conclusao
plot_piram_idade
plot_scatter_mt_ch
plot_line_notas
plot_bolhas_uf_notas 
plot_box_uf_redacao
plot_bar_erro

library(gridExtra)
library(grid)

## inserindo todos os gráficos na mesma página (visualização ruim)
grid.arrange(plot_idioma_sexo,plot_uf_conclusao,plot_piram_idade,plot_scatter_mt_ch,plot_line_notas,plot_bolhas_uf_notas ,plot_box_uf_redacao,plot_bar_erro)


##### 10.2 Inserindo gráficos na página configurada ####

## criando layout com 2 linhas e 4 colunas
lay <- rbind(c(1,2,3,4),
             c(5,6,7,8))
lay

grid.arrange(plot_idioma_sexo,plot_uf_conclusao,
             plot_piram_idade,plot_scatter_mt_ch,
             plot_line_notas,plot_bolhas_uf_notas,
             plot_box_uf_redacao,plot_bar_erro,
             layout_matrix = lay)


## criando layout com 2 linhas e 2 colunas e mesclando colunas
lay <- rbind(c(1,2),
             c(3,3))
lay

grid.arrange(plot_box_uf_redacao, plot_scatter_mt_ch, plot_line_notas, layout_matrix = lay)


## plotagem simulando erro
grid.arrange(plot_box_uf_redacao,plot_scatter_mt_ch,
             plot_line_notas,plot_idioma_sexo,
             plot_piram_idade,plot_bar_erro,
             layout_matrix = lay)

grid.arrange(plot_box_uf_redacao,plot_scatter_mt_ch,
             plot_line_notas,plot_idioma_sexo,
             plot_piram_idade,plot_bar_erro)

## criando layout com 4 linhas e 2 colunas e mesclando colunas
lay <- rbind(c(1,2),
             c(3,3),
             c(4,5),
             c(6,6))
lay
grid.arrange(plot_box_uf_redacao,plot_scatter_mt_ch,
             plot_line_notas,plot_idioma_sexo,
             plot_piram_idade,plot_bar_erro,
             layout_matrix = lay)


## criando layout com 2 linhas e 2 colunas e mesclando colunas
lay <- rbind(c(1,1),
             c(2,3))

grid.arrange(plot_line_notas,
             plot_box_uf_redacao,plot_bar_erro,
             layout_matrix = lay)



