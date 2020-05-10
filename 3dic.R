#------------------------------------------------------------------------
# INTEIRAMENTE CASUALIZADO                                              -
#------------------------------------------------------------------------

rm(list=ls())

#------------------------------------------------------------------------
# Um trabalho foi realizado com com 300 aves machos, para comparar as   -
# linhagens Cobb, Ross e Hubbard, dispostos em delineamento inteiramente- 
# casualizado (DIC) com cinco repeticoes em cada tratamento e 20 aves em- 
# cada unidade experimental. Foi realizado o acompanhamento diario, com -
# pesagens, calculos do ganho medio diario de peso (GMP), consumo de    -
# racao (CR) e da conversao alimentar semanal (CA). Os animais foram    -
# distribuidos aleatoriamente entre os tratamentos e nas unidades       -
# experimentais.                                                        -
# Apos o experimento os resultados obtidos foram os que se apresentam   -
# na planilha Google.                                                   -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Chamar os dados do Google Planilhas                                   -
#------------------------------------------------------------------------

library(gsheet)
dados <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1DZlH9M2VKDkP81JHKv8TAy7zviPIqRIboOUOH-IMTtU/edit?usp=sharing')
str(dados)
attach(dados)

#------------------------------------------------------------------------
# Estatistica descritiva:                                               -
#------------------------------------------------------------------------

# Geral
(soma_CR = with(dados, sum(CR)))
(media_CR = with(dados, mean(CR)))
(variancia_CR = with(dados, var(CR)))
(desvio_CR = with(dados, sd(CR)))
(CV_CR = desvio_CR / media_CR * 100)

# Por tratamento
(somas_CR = with(dados, tapply(CR, tratamento, sum)))
(medias_CR = with(dados, tapply(CR, tratamento, mean)))
(variancias_CR = with(dados, tapply(CR, tratamento, var)))
(desvios_CR = with(dados, tapply(CR, tratamento, sd)))
(cv_CR = desvios_CR/medias_CR * 100)

#------------------------------------------------------------------------
# Graficos:                                                             -
#------------------------------------------------------------------------

with(dados, stripchart(CR ~ tratamento, pch=20, vertical='T', las=1, 
                       ylab='Consumo de Racao (g)', 
                       xlab='Linhagem' , 
                       cex=1.5, col='blue'))

require(car)
with(dados, Boxplot(CR ~ tratamento, las=1, xlab='Linhagem', 
                    ylab='Consumo de Racao (g)', col='lightyellow'))
points(medias_CR, pch='+', cex=1.8, col='red')

#------------------------------------------------------------------------
# Analise de variancia                                                  -
#------------------------------------------------------------------------

mod = with(dados, aov(CR ~ tratamento))
summary(mod)

qf(0.95,2,12) #valor critico de F para nivel de 5%
qf(0.99,2,12) #valor critico de F para nivel de 1%

#------------------------------------------------------------------------
# Pressupostos                                                          -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Normalidade dos residuos:                                             -
# H0: ha normalidade dos residuos;                                      -
# H1: nao ha normalidade dos residuos.                                  -
#------------------------------------------------------------------------

# Teste de Shapiro-Wilk
shapiro.test(mod$res)

#------------------------------------------------------------------------
# Homogeneidade de variancias:                                          -
# H0: ha homogeneidade de variancia;                                    -
# H1: nao ha homogeneidade de variancias.                               -
#------------------------------------------------------------------------

# Teste de Bartlett
with(dados, bartlett.test(CR ~ tratamento))

#------------------------------------------------------------------------
# Independencia dos erros                                               -
#------------------------------------------------------------------------

plot(mod$res, las=1, pch=19, col='red', ylab='Residuos')

#------------------------------------------------------------------------
# Usando o pacote ExpDes.pt e easyanova                                 -
#------------------------------------------------------------------------

library(ExpDes.pt)
dic(tratamento, CR, mcomp = "tukey") 

# E necessario adequar os dados:
dados.t = with(dados, (data.frame(tratamento, CR)))
library(easyanova)
mod.1 = ea1(dados.t, design=1, plot=2)       # plot=1,2,3
names(mod.1)
mod.1

#------------------------------------------------------------------------
# FIM                                                                   -
#------------------------------------------------------------------------