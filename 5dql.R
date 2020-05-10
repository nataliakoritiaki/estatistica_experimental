#------------------------------------------------------------------------
# Quadrado Latino                                                       -
#------------------------------------------------------------------------

rm(list=ls())

#------------------------------------------------------------------------
# Considere um experimento, cujo objetivo foi estudar o efeito da idade -
# de castracao no desenvolvimento e producao de suinos, avaliando-se o  -
# peso dos leitoes. Quatro tratamentos foram estudados:                 -
# A - castracao aos 56 dias de idades;                                  -
# B - inteiros (nao castrados).                                         -
# C - castracao aos 7 dias de idade;                                    -
# D - castracao aos 21 dias de idade;                                   -
# Duas causas de variacao que podem afetar o peso final dos animais sao:-
# o numero de leitoes de cada cria (leitegada) e o peso inicial de cada -
# animal. Essas duas causas de variacao podem ser controladas utilizando-
# se o delineamento em Quadrado Latino, sendo que a variacao entre      -
# leitegadas foi controlada pelas linhas do quadrado e a variacao dos   -
# pesos dos leitoes dentro das leitegadas foi isolada pelas colunas.    -
# Na planilha Google se encontram os ganhos de peso, em quilogramas, ao -
# final do experimento (252 dias), nos respectivos tratamento.          -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Chamar os dados do Google Planilhas                                   -
#------------------------------------------------------------------------

library(gsheet)
dados <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1V29K10MpirTGQt-2MojolqxhuyStpqqA1Ur899iNSlU/edit?usp=sharing')
str(dados)
dados$tratamento <- as.factor(dados$tratamento)
dados$linhas <- as.factor(dados$linhas)
dados$colunas <- as.factor(dados$colunas)
attach(dados)

#------------------------------------------------------------------------
# Estatistica descritiva:                                               -
#------------------------------------------------------------------------

# Geral
(soma = with(dados, sum(resposta)))
(media = with(dados, mean(resposta)))
(variancia = with(dados, var(resposta)))
(desvio = with(dados, sd(resposta)))
(CV = desvio / media * 100)

# Por tratamento, linha e coluna
(somatrat = tapply(resposta, tratamento, sum))
(somalinhas = tapply(resposta, linhas, sum))
(somacolunas = tapply(resposta, colunas, sum))
(mediastrat = tapply(resposta, tratamento, mean))
(mediaslinh = tapply(resposta, linhas, mean))
(mediascol = tapply(resposta, colunas, mean))

(varianciastrat = with(dados, tapply(resposta, tratamento, var)))
(desviostrat = with(dados, tapply(resposta, tratamento, sd)))
(cvtrat = desviostrat/mediastrat * 100)

#------------------------------------------------------------------------
# Graficos:                                                             -
#------------------------------------------------------------------------

par(mai=c(1,1,.2,.2))
boxplot(resposta ~ tratamento, xlab='Tratamentos', names=c('A','B','C','D'),
        ylab="Ganho de Peso (kg)", las=1, col='LightYellow') 
points(mediastrat, pch='+', col='red', cex=1.5)

boxplot(resposta ~ linhas, xlab='Leitegadas', names=c('1','2','3','4'),
        ylab="Ganho de Peso (kg)", las=1, col='LightYellow') 
points(mediaslinh, pch='+', col='red', cex=1.5)

boxplot(resposta ~ colunas, xlab='Pesos', names=c('G1','G2','G3','G4'),
        ylab="Ganho de Peso (kg)", las=1, col='LightYellow') 
points(mediascol, pch='+', col='red', cex=1.5)

#------------------------------------------------------------------------
# Analise de variancia                                                  -
#------------------------------------------------------------------------

mod = with(dados, aov(resposta ~ colunas + linhas + tratamento))
summary(mod)

#------------------------------------------------------------------------
# Pressupostos                                                          -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Normalidade dos res?duos:                                             -
# H0: ha normalidade dos residuos;                                      -
# H1: nao ha normalidade dos residuos.                                  -
#------------------------------------------------------------------------

# Teste de Shapiro-Wilk
shapiro.test(mod$res)

#------------------------------------------------------------------------
# Homogeneidade de vari?ncias:                                          -
# H0: ha homogeneidade de variancia;                                    -
# H1: nao ha homogeneidade de variancias.                               -
#------------------------------------------------------------------------

# Teste de Bartlett
bartlett.test(resposta, tratamento)
bartlett.test(resposta, colunas)
bartlett.test(resposta, linhas)

#------------------------------------------------------------------------
# Teste de Aditividade:                                                 -
# H0: os efeitos principais sao aditivos;                               -
# H1: os efeitos principais nao sao aditivos.                           -
#------------------------------------------------------------------------

require(asbio)
tukey.add.test(resposta, tratamento, linhas)
tukey.add.test(resposta, tratamento, colunas)
tukey.add.test(resposta, linhas, colunas)

#------------------------------------------------------------------------
# Independencia dos erros                                               -
#------------------------------------------------------------------------

plot(mod$res, las=1, pch=19, col='red', ylab='Residuos')

#------------------------------------------------------------------------
# Usando o pacote ExpDes.pt e easyanova                                 -
#------------------------------------------------------------------------

library(ExpDes.pt)
dql(tratamento, colunas, linhas, resposta, mcomp = "tukey") 

# ? necess?rio adequar os dados:
dados.t = with(dados, (data.frame(tratamento, linhas, colunas, resposta)))
require(easyanova)
mod.1 = ea1(dados.t, design=3, plot=2)       # plot=1,2,3
names(mod.1)
mod.1

#------------------------------------------------------------------------
# FIM                                                                   -
#------------------------------------------------------------------------