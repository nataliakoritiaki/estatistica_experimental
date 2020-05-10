rm(list=ls())

#------------------------------------------------------------------------
# Casualizacao                                                          -
#------------------------------------------------------------------------

rm(list=ls())

animal <- sample(1:10)
trat <- factor(rep(c('A','B'), each=5))
data.frame(animal, trat)

#------------------------------------------------------------------------
# Conjunto de Dados                                                     -
#------------------------------------------------------------------------

library(gsheet)
dados <- gsheet2tbl('https://docs.google.com/spreadsheets/d/13_JObv8B1d5hRSstmUkmegXRcZXPez-ec1CRfMD0sOc/edit?usp=sharing')
attach(dados)

#------------------------------------------------------------------------
# Somatorias                                                            -
#------------------------------------------------------------------------

(soma <- with(dados, sum(resposta)))
(soma_quadrados <- with(dados, sum(resposta^2)))
(quadrado_soma <- soma^2)

#------------------------------------------------------------------------
# Medidas Descritivas                                                   -
#------------------------------------------------------------------------

# Geral

# Media:
(media <- with(dados, mean(resposta)))

# Variancia:
(variancia <- with(dados, var(resposta)))

# Desvio-padrao:
(desvio <- with(dados, sd(resposta)))

# Erro-padrao:
n <- length(resposta)
(erro <- sqrt(n))

# Coeficiente de variacao:
(CV <- desvio/media*100)

# Por Tratamento

# Medias:
(medias <- with(dados, tapply(resposta, trat, mean)))

# Variancias:
(variancias <- with(dados, tapply(resposta, trat, var)))

# Desvios-padrao:
(desvios <- with(dados, tapply(resposta, trat, sd)))

# Erros-padrao:
n <- length(resposta)
(erros = desvios / sqrt(n))

# Coeficientes de variacao:
(CVs = desvios / m?dias * 100)

#------------------------------------------------------------------------
# Grafico de Box-Plot                                                   -
#------------------------------------------------------------------------

# install.packages('car', dep=T)
library(car)
Boxplot(resposta ~ trat, names=c('A', 'B'),
        ylab='Vari?vel Resposta', xlab='Tratamentos', 
        las=1, col='LightYellow')
points(medias, pch="+", col=2, cex=1.5)

#------------------------------------------------------------------------
# Pressupostos                                                          -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Normalidade dos residuos:                                             -
# H0: ha normalidade dos residuos;                                      -
# H1: nao ha normalidade dos residuos.                                  -
#------------------------------------------------------------------------

# Teste de Shapiro-Wilk
mod = with(dados, aov(resposta ~ trat + trat1))
shapiro.test(mod$res)

#------------------------------------------------------------------------
# Homogeneidade de variancias:                                          -
# H0: ha homogeneidade de variancia;                                    -
# H1: nao ha homogeneidade de variancias.                               -
#------------------------------------------------------------------------

# Teste de Bartlett
(dados$trat_i = with(dados, interaction(trat, trat1)))
with(dados, bartlett.test(resposta ~ trat_i))

#------------------------------------------------------------------------
# Teste de Aditividade:                                                 -
# H0: os efeitos principais sao aditivos;                               -
# H1: os efeitos principais nao sao aditivos.                           -
#------------------------------------------------------------------------

require(asbio)
tukey.add.test(resposta, trat, trat1)

#------------------------------------------------------------------------
# Independencia dos erros                                               -
#------------------------------------------------------------------------

plot(mod$res, las=1, pch=19, col='red', ylab='Residuos')
