#------------------------------------------------------------------------
# CASUALIZADO EM BLOCOS                                                 -
#------------------------------------------------------------------------

rm(list=ls())

#------------------------------------------------------------------------
# Com a finalidade de estudar os efeitos da raca no peso               -
# corporal de cordeiros ao nascimento, considerou-se um experimento     -
# casualizados em blocos com dois tipos de parto (blocos - simples e    -
# gemelar) e cinco racas (tratamentos - Santa Ines, Texel, Ile de       - 
# France, Hampshire Down, Corriedale). Os pesos ao nascer (kg) sao      -
# apresentados na planilha do Google.                                   -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Chamar os dados do Google Planilhas                                   -
#------------------------------------------------------------------------

library(gsheet)
dados <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1YSVt-VBllIoPB5ZALGBaCK-InKxXY7mJA128_HXRHaE/edit?usp=sharing')
str(dados)
dados$tratamento <- as.factor(dados$tratamento)
dados$blocos <- as.factor(dados$blocos)
dados$resposta <- as.numeric(dados$resposta)
attach(dados)

# Blocos:
# 1 - nascidos de partos simples;
# 2 - nascidos de partos gemelares;

# Tratamentos:
# 1 - Santa Ines;
# 2 - Texel;
# 3 - Ile de France;
# 4 - Hampshire Down;
# 5 - Corriedale.

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
(somablocos = tapply(resposta, blocos, sum))
(mediastrat = tapply(resposta, tratamento, mean))
(mediasblocos = tapply(resposta, blocos, mean))

(varianciastrat = with(dados, tapply(resposta, tratamento, var)))
(desviostrat = with(dados, tapply(resposta, tratamento, sd)))
(cvtrat = desviostrat/mediastrat * 100)

#------------------------------------------------------------------------
# Graficos:                                                             -
#------------------------------------------------------------------------

par(mai=c(1,1,.2,.2))
boxplot(resposta ~ tratamento, xlab='Racas', 
        names=c('SI','TX','IF','HD', 'CR'),
        ylab="Peso (kg)", las=1, col='LightYellow') 
points(mediastrat, pch='+', col='red', cex=1.5)

boxplot(resposta ~ blocos, xlab='Tipo de Parto', 
        names=c('Simples','Gemelar'),
        ylab="Peso (kg)", las=1, col='LightYellow') 
points(mediasblocos, pch='+', col='red', cex=1.5)

#------------------------------------------------------------------------
# Analise de Variancia                                                  -
#------------------------------------------------------------------------

mod = with(dados, aov(resposta ~ tratamento + blocos))
summary(mod)

#------------------------------------------------------------------------
# Pressupostos                                                          -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Normalidade dos residuos:                                             -
# H0: ha normalidade dos residuos;                                      -
# H1: nao h? normalidade dos residuos.                                  -
#------------------------------------------------------------------------

shapiro.test(mod$res)

#------------------------------------------------------------------------
# Homogeneidade de vari?ncias:                                          -
# H0: ha homogeneidade de variancia;                                    -
# H1: nao ha homogeneidade de variancias.                               -
#------------------------------------------------------------------------

bartlett.test(resposta, tratamento)
bartlett.test(resposta, blocos)

#------------------------------------------------------------------------
# Teste de Aditividade:                                                 -
# H0: os efeitos principais sao aditivos;                               -
# H1: os efeitos principais nao sao aditivos.                           -
#------------------------------------------------------------------------

require(asbio)
tukey.add.test(resposta, tratamento, blocos)

#------------------------------------------------------------------------
# Independencia dos erros                                               -
#------------------------------------------------------------------------

plot(mod$res, las=1, pch=19, col='red', ylab='Residuos')

#------------------------------------------------------------------------
# Usando o pacote ExpDes.pt e easyanova                                 -
#------------------------------------------------------------------------

library(ExpDes.pt)
dbc(tratamento, blocos, resposta, mcomp = "tukey") 

# ? necessario adequar os dados:
dados.PN = with(dados, (data.frame(tratamento, blocos, resposta)))
require(easyanova)
mod = ea1(dados.PN, design=2)
names(mod)
mod

#------------------------------------------------------------------------
# FIM                                                                   -
#------------------------------------------------------------------------