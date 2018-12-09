
library(arules)
library(dplyr)

##LENDO DATAFRAMES
df_2012 = read.csv('../Desktop/DADOS_NT/dados2012.csv', header = TRUE, sep = ";")
df_2013 = read.csv('../Desktop/DADOS_NT/dados2013.csv', header = TRUE, sep = ";")
df_2014 = read.csv('../Desktop/DADOS_NT/dados2014.csv', header = TRUE, sep = ";")
df_2015 = read.csv('../Desktop/DADOS_NT/dados2015.csv', header = TRUE, sep = ";")
df_2016 = read.csv('../Desktop/DADOS_NT/dados2016.csv', header = TRUE, sep = ";")
df_2017 = read.csv('../Desktop/DADOS_NT/dados2017.csv', header = TRUE, sep = ";")


#CRIANDO TRANSAÇÕES PARA O APRIORI
dataset2012 <- read.transactions('../Desktop/DADOS_NT/DADOS_MORTE_30-11/DADOS_2012.csv', format='basket', sep=';');
dataset2013 <- read.transactions('../Desktop/DADOS_NT/DADOS_MORTE_30-11/DADOS_2013.csv', format='basket', sep=';');
dataset2014 <- read.transactions('../Desktop/DADOS_NT/DADOS_MORTE_30-11/DADOS_2014.csv', format='basket', sep=';');
dataset2015 <- read.transactions('../Desktop/DADOS_NT/DADOS_MORTE_30-11/DADOS_2015.csv', format='basket', sep=';');
dataset2016 <- read.transactions('../Desktop/DADOS_NT/DADOS_MORTE_30-11/DADOS_2016.csv', format='basket', sep=';');
dataset2017 <- read.transactions('../Desktop/DADOS_NT/DADOS_MORTE_30-11/DADOS_2017.csv', format='basket', sep=';');

#DEFININDO OS VALORES MÍNIMOS DE SUPORTE E CONFIANÇA
suporte <- 0.1;
confianca <- 0.8;

#SUMARIZANDO UMA TRANSAÇÃO
summary(dataset2012);

#DEMONSTRANDO OS ITENS MAIS FREQUENTES DE UMA TRANSAÇÃO, DEFININDO topN = x, PARA OS x PRIMEIROS
itemFrequencyPlot(dataset2012, topN = 10, xlab = 'Variáveis', ylab = 'Freqência')

#CRIANDO AS REGRAS DE ASSOCIAÇÃO PARA CADA ANO
rules2012 <- apriori(data = dataset2012, parameter = list(support = suporte, confidence = confianca))
#rules2012 <- apriori (data=dataset2012, parameter=list(supp=0.1,conf = 0.8 ),
                    #  appearance = list(default="lhs", rhs=list("domingo")),
                     #control = list (verbose=F))
rules2013 <- apriori(data = dataset2013, parameter = list(support = suporte, confidence = confianca))
rules2014 <- apriori(data = dataset2014, parameter = list(support = suporte, confidence = confianca))
rules2015 <- apriori(data = dataset2015, parameter = list(support = suporte, confidence = confianca))
rules2016 <- apriori(data = dataset2016, parameter = list(support = suporte, confidence = confianca))
rules2017 <- apriori(data = dataset2017, parameter = list(support = suporte, confidence = confianca))

rules_lift2012 <- sort(rules2012, by="lift", decreasing=TRUE) # 'high-lift' rules.
rules_lift2013 <- sort(rules2013, by="lift", decreasing=TRUE) # 'high-lift' rules.
rules_lift2014 <- sort(rules2014, by="lift", decreasing=TRUE) # 'high-lift' rules.
rules_lift2015 <- sort(rules2015, by="lift", decreasing=TRUE) # 'high-lift' rules.
rules_lift2016 <- sort(rules2016, by="lift", decreasing=TRUE) # 'high-lift' rules.
rules_lift2017 <- sort(rules2017, by="lift", decreasing=TRUE) # 'high-lift' rules.


r2012 <- as(rules_lift2012, "data.frame")
r2013 <- as(rules_lift2013, "data.frame")
r2014 <- as(rules_lift2014, "data.frame")
r2015 <- as(rules_lift2015, "data.frame")
r2016 <- as(rules_lift2016, "data.frame")
r2017 <- as(rules_lift2017, "data.frame")


#CRIANDO ARQUIVOS CSV PARA AS REGRAS GERADAS
write.table(r2012, file = "../Desktop/DADOS_NT/DADOS_MORTE_30-11/RESULTADOS/REGRAS_2012.csv", sep = ",",
            row.names = TRUE, col.names = NA)
write.table(r2013, file = "../Desktop/DADOS_NT/DADOS_MORTE_30-11/RESULTADOS/REGRAS_2013.csv", sep = ",",
            row.names = TRUE, col.names = NA)
write.table(r2014, file = "../Desktop/DADOS_NT/DADOS_MORTE_30-11/RESULTADOS/REGRAS_2014.csv", sep = ",",
            row.names = TRUE, col.names = NA)
write.table(r2015, file = "../Desktop/DADOS_NT/DADOS_MORTE_30-11/RESULTADOS/REGRAS_2015.csv", sep = ",",
            row.names = TRUE, col.names = NA)
write.table(r2016, file = "../Desktop/DADOS_NT/DADOS_MORTE_30-11/RESULTADOS/REGRAS_2016.csv", sep = ",",
            row.names = TRUE, col.names = NA)
write.table(r2017, file = "../Desktop/DADOS_NT/DADOS_MORTE_30-11/RESULTADOS/REGRAS_2017.csv", sep = ",",
            row.names = TRUE, col.names = NA)

#PLOTANDO GRÁFICOS DE HISTOGRAMAS PARA DADOS DOS DATAFRAMES
#hist(df_2012$IDADE, xlim=c(0,100), ylim=c(0,0.05), freq = FALSE ,
    # breaks=10, main="Histograma de idades - 2012", xlab="Faixa de idades")
idades_gerais <- c(df_2012$IDADE, df_2013$IDADE, df_2014$IDADE, df_2015$IDADE, df_2016$IDADE, df_2017$IDADE)

plot(density(df_2012$IDADE, adjust=2), lty="dashed", col="green", ylim=c(0,0.048), xlim=c(0,90), xlab="Idades", ylab="Densidade", main = "Representação das idades das vítimas (2012 - 2014)", lwd=2)
lines(density(df_2013$IDADE, adjust=2), lty="dashed", col="yellow", lwd=2)
lines(density(df_2014$IDADE, adjust=2), lty="dashed", col="blue", lwd=2) 
curve(expr = dnorm(x,mean=mean(idades_gerais),sd= sd(idades_gerais, na.rm= FALSE)), add = TRUE, col="red")

legend(55, 0.04, legend=c("2012", "2013", "2014", "Curva Normal"),
       col=c("green", "yellow", "blue", "red"), lty=c(2,2,2,1), cex=0.8)

plot(density(df_2015$IDADE, adjust=2), lty="dashed", col="green", ylim=c(0,0.048), xlim=c(0,90), xlab="Idades", ylab="Densidade", main = "Representação das idades das vítimas (2015 - 2017)", lwd=2)
lines(density(df_2016$IDADE, adjust=2), lty="dashed", col="yellow", lwd=2)
lines(density(df_2017$IDADE, adjust=2), lty="dashed", col="blue", lwd=2)
curve(expr = dnorm(x,mean=mean(idades_gerais),sd= sd(idades_gerais, na.rm= FALSE)), add = TRUE, col="black")

legend(55, 0.04, legend=c("2015", "2016", "2017", "Curva Normal"),
       col=c("green", "yellow", "blue", "red"), lty=c(2,2,2,1), cex=0.8)

#CRIANDO UM VETOR COM TODAS AS IDADES

curve(expr = dnorm(x,mean=mean(idades_gerais),sd= sd(idades_gerais, na.rm= FALSE)), add = TRUE, col="red")


##CRIANDO FILTSOS DE HORÁRIOS MAIS VIOLENTOS
df_geral <- read.csv('../Desktop/DADOS_NT/dados_att2.csv', header = TRUE, sep = ";")

filtro_2012 <- filter(df_geral, df_geral$ANO == 2012 & 
                        df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO" &
                        df_geral$GENERO == "MASCULINO" & df_geral$ZONA == "Norte")

plot(filtro_2012$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2012", col="blue")


##GERANDO UM FILTRO NAS HORAS PARA AS REGRAS DE 2013

filtro_2013 <- filter(df_geral, df_geral$ETNIA =="PARDA" &
                        df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO" &
                        df_geral$TURNO == "NOTURNO" & df_geral$FAIXA == "ADULTO" &
                        df_geral$GENERO =="MASCULINO" & df_geral$ANO == 2013)

plot(filtro_2013$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2013", col="blue")

##GERANDO UM FILTRO NAS HORAS PARA AS REGRAS DE 2014

filtro_2014 <- filter(df_geral, df_geral$ZONA == "Norte" &
                        df_geral$TURNO == "NOTURNO" &
                        df_geral$GENERO =="MASCULINO" & df_geral$ANO == 2014)

plot(filtro_2014$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2014", col="blue")

##GERANDO FILTRO PARA 2015
filtro_2015 <- filter(df_geral, df_geral$ANO == 2015 & 
                        df_geral$TURNO == "NOTURNO" &
                        df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO" &
                        df_geral$ETNIA == "PARDA" & df_geral$GENERO == "MASCULINO")


plot(filtro_2015$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2015", col="blue")


filtro_2016 <- filter(df_geral, df_geral$ANO == 2016 & 
                        df_geral$FAIXA == "ADULTO" & df_geral$ZONA == "Norte" 
                        & df_geral$ETNIA == "PARDA" & df_geral$GENERO == "MASCULINO"
                        & df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO"
                        & df_geral$TURNO == "NOTURNO")

##GERANDO FILTRO PARA 2016
plot(filtro_2016$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2016", col="blue")


##GERANDO FILTRO PARA 2017
filtro_2017 <- filter(df_geral, df_geral$ANO == 2017 & 
                        df_geral$TURNO == "NOTURNO" & df_geral$ZONA == "Oeste" 
                      & df_geral$ETNIA == "PARDA" & df_geral$GENERO == "MASCULINO"
                      & df_geral$INSTRUMENTO.UTILIZADO == "ARMA DE FOGO" &df_geral$FAIXA == "ADULTO")


da<- sort(filtro_2017$HORARIO.APROXIMADO, decreasing = TRUE)
##GERANDO FILTRO PARA 2017
barplot(filtro_2017$HORARIO.APROXIMADO, xlim=c(0, 24), xlab = "Horário do dia",
     ylab = "Frequência", main= "Horários - regras 2017", col="blue")
#------------------------------------------------------------------------------------------------------#
##CRIANDO VARIAVEL PARA A FREQUÊNCIA DE DIAS DA SEMANA E BAIRROS
#2012
  ##CRIANDO VARIAVEL PARA 2012
dia_da_semana_2012.freq <- table(filtro_2012$DIA.DA.SEM)
barplot(dia_da_semana_2012.freq[order(dia_da_semana_2012.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2012")
  
  ##APRESENTA OS BAIRROS MAIS FREQÊNTES PARA A REGRA
bairro_2012.freq <- table(filtro_2012$BAIRRO.DA.OCORRENCIA)
#barplot(bairro_2012.freq[order(bairro_2012.freq, decreasing = T)], xlab = "Bairro", ylab = "Quantidade", col = "blue", main = "Ano 2012")
bairro_2012.freq


##2013
dia_da_semana_2013.freq <- table(filtro_2013$DIA.DA.SEM)
barplot(dia_da_semana_2013.freq[order(dia_da_semana_2013.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2013")


bairro_2013.freq <- table(filtro_2013$BAIRRO.DA.OCORRENCIA)
bairro_2013.freq


#2014

dia_da_semana_2014.freq <- table(filtro_2014$DIA.DA.SEM)
barplot(dia_da_semana_2014.freq[order(dia_da_semana_2014.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2014")


bairro_2014.freq <- table(filtro_2014$BAIRRO.DA.OCORRENCIA)
bairro_2014.freq


#2015

dia_da_semana_2015.freq <- table(filtro_2015$DIA.DA.SEM)
barplot(dia_da_semana_2015.freq[order(dia_da_semana_2015.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2015")


bairro_2015.freq <- table(filtro_2015$BAIRRO.DA.OCORRENCIA)
bairro_2015.freq

#2016

dia_da_semana_2016.freq <- table(filtro_2016$DIA.DA.SEM)
barplot(dia_da_semana_2016.freq[order(dia_da_semana_2016.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2016")


bairro_2016.freq <- table(filtro_2016$BAIRRO.DA.OCORRENCIA)
bairro_2016.freq

#2017

dia_da_semana_2017.freq <- table(filtro_2017$DIA.DA.SEM)
barplot(dia_da_semana_2017.freq[order(dia_da_semana_2017.freq, decreasing = T)], xlab = "Dia da semana", ylab = "Quantidade", col = "blue", main = "Ano 2017")


bairro_2017.freq <- table(filtro_2017$BAIRRO.DA.OCORRENCIA)
bairro_2017.freq




#_________________________________________________________________________________________________
#quantile(df$IDADE)



