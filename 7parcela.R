#------------------------------------------------------------------------
# PARCELA SUBDIVIDIDA                                                   -
#------------------------------------------------------------------------

rm(list=ls())

#------------------------------------------------------------------------
# Com a finalidade de estudar tres racoes (A, B e C), em seis blocos    -
# casualizados, cada parcela constituida por dois animais. Em uma       -
# determinada fase do ensaio, os bovinos, dentro de cada parcela,       -
# passaram a receber, por sorteio, um tipo de suplemento mineral (M ou  -
# P).                                                                   -
# Os ganhos de pesos (kg) individuais, ao final do experimento s?o      -
# apresentados na planilha Google.                                      -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Chamar os dados do Google Planilhas                                   -
#------------------------------------------------------------------------

library(gsheet)
dados <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1kFz2PfNTD3zBqhUdOGhGeF-af8wPyimMNlNgJ51pYB4/edit?usp=sharing')
str(dados)
attach(dados)

#------------------------------------------------------------------------
# Estatistica descritiva:                                               -
#------------------------------------------------------------------------

# Medias
(mediag = mean(resp))
(mediast = tapply(resp, racao, mean))
(mediasb = tapply(resp, bloco, mean))
(mediasup = tapply(resp, supl, mean))
(mediastb = tapply(resp, list(racao, bloco), mean))
(mediastsup = tapply(resp, list(racao, supl), mean))
(mediasbsup = tapply(resp, list(bloco, supl), mean))
(mediastbsup = tapply(resp, list(racao, bloco, supl), mean))

# Desvios-padrao
(desviog = sd(resp))
(desviost = tapply(resp, racao, sd))
(desviosb = tapply(resp, bloco, sd))
(desviossup = tapply(resp, supl,sd))
(desviostb = tapply(resp, list(racao, bloco), sd))
(desviostsup = tapply(resp, list(racao, supl), sd))
(desviosbsup = tapply(resp, list(bloco, supl), sd))

#------------------------------------------------------------------------
# Gr?ficos:                                                             -
#------------------------------------------------------------------------

par(mai=c(1,1,.2,.2))
boxplot(resp ~ racao, names=c("A", "B", "C"),
        ylab="Ganhos de Peso (kg)", xlab="Racoes",
        las=1, col='LightYellow')
points(mediast, pch="+", col=2, cex=1.5)

par(mai=c(1,1,.2,.2))
boxplot(resp ~ supl, names=c("M", "P"),
        ylab="Ganhos de Peso (kg)", xlab="Suplementos minerais",
        las=1, col='LightYellow')
points(mediasup, pch="+", col=2, cex=1.5)

par(mai=c(1,1,.2,.2))
interaction.plot(racao, supl, resp, las=1, xlab='Tratamentos', 
                 ylab='', col=1:6, bty='l')
mtext('Ganhos de Peso (kg)', side=2, line=3.5)

par(mai=c(1,1,.2,.2))
interaction.plot(supl, racao, resp, las=1, xlab='Suplementos', 
                 ylab='', col=1:6, bty='l')
mtext('Ganhos de Peso (kg)', side=2, line=3.5)

#------------------------------------------------------------------------
# An?lise de vari?ncia                                                  -
#------------------------------------------------------------------------

mod = aov(resp ~ bloco + racao*supl + Error(bloco/racao), data = dados)
names(mod)
summary(mod)

#------------------------------------------------------------------------
# Pressupostos                                                          -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Normalidade dos residuos:                                             -
# H0: ha normalidade dos residuos;                                      -
# H1: nao ha normalidade dos residuos.                                  -
#------------------------------------------------------------------------

# Teste de Shapiro-Wilk
require(dae)
res = resid.errors(mod)
shapiro.test(res)

#------------------------------------------------------------------------
# Pressupostos                                                          -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Homogeneidade de variancias:                                          -
# H0: ha homogeneidade de variancia;                                    -
# H1: nao ha homogeneidade de variancias.                               -
#------------------------------------------------------------------------

bartlett.test(resp, racao)
bartlett.test(resp, bloco)
bartlett.test(resp, supl)

#------------------------------------------------------------------------
# Usando o pacote ExpDes.pt e easyanova                                             -
#------------------------------------------------------------------------

#ExpDes.pt
library(ExpDes.pt)
psub2.dbc(racao,supl,bloco,resp)

#easyanova
# ? necessario adequar os dados:
dados.t = with(dados, (data.frame(racao, bloco, supl, resp)))
library(easyanova)
mod.1 = ea2(dados.t, design=5)
mod.1

#------------------------------------------------------------------------
# FIM                                                                   -
#------------------------------------------------------------------------