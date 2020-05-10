#------------------------------------------------------------------------
# Fatorial                                                              -
#------------------------------------------------------------------------

rm(list=ls())

#------------------------------------------------------------------------
# Com a finalidade de estudar o desenvolvimento das mudas de eucalipto, -
# um pesquisador testou duas especies (E1 = Eucalyptus citriodora e E2 =- 
# Eucalyptus grandis) plantadas em tres tipos de recipientes (R1 = saco -
# plastico pequeno, R2 = saco plastico grande e R3 = laminado). As      -
# medias das mudas, em centimetro (cm), aos 80 dias de idade sao        - 
# apresentados na planilha do Google.
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Chamar os dados do Google Planilhas                                   -
#------------------------------------------------------------------------

library(gsheet)
dados <- gsheet2tbl('https://docs.google.com/spreadsheets/d/1uJ0Ie40G98KyrUVCsUlDCm046gaOlDp6cSs-ARLLzuk/edit?usp=sharing')
str(dados)
(dados$trat = with(dados, interaction(tratamento01, tratamento02)))
attach(dados)
dados

#------------------------------------------------------------------------
# Estatistica descritiva:                                               -
#------------------------------------------------------------------------

# Geral
(soma = with(dados, sum(resp)))
(media = with(dados, mean(resp)))
(variancia = with(dados, var(resp)))
(desvio = with(dados, sd(resp)))
(CV = desvio / media * 100)

# Por fatores
(medias = tapply(resp, list(tratamento01, tratamento02), mean))
(medias.trat01 = tapply(resp, tratamento01, mean))
(medias.trat02 = tapply(resp, tratamento02, mean))

(vars = tapply(resp, list(tratamento01, tratamento02), var))
(var.trat01 = tapply(resp, tratamento01, var))
(var.trat02 = tapply(resp, tratamento02, var))

(desvios = tapply(resp, list(tratamento01, tratamento02), sd))
(desvios.trat01 = tapply(resp, tratamento01, sd))
(desvios.trat02 = tapply(resp, tratamento02, sd))

#------------------------------------------------------------------------
# Graficos:                                                             -
#------------------------------------------------------------------------

par(mai=c(1,1,.2,.2))
boxplot(resp ~ tratamento02, names=c("E1", "E2"),
        ylab="Alturas medias (cm)", xlab="Especies",
        las=1, col='LightYellow')
points(medias.trat02, pch="+", col=2, cex=1.5)

par(mai=c(1,1,.2,.2))
boxplot(resp ~ tratamento01, names=c("R1", "R2", "R3"),
        ylab="Alturas medias (cm)", xlab="Recipientes",
        las=1, col='LightYellow')
points(medias.trat01, pch="+", col=2, cex=1.5)

par(mai=c(1,1,.2,.2))
interaction.plot(tratamento01, tratamento02, resp, las=1, xlab='Recipientes',
         ylab='Alturas medias (cm)', col=c('red','blue'), bty='l',
         trace.label = deparse(substitute(Especies)), lwd=2.5)

par(mai=c(1,1,.2,.2))
interaction.plot(tratamento02, tratamento01, resp, las=1, xlab='Especies',
         ylab='Alturas medias (cm)', col=c('red','blue'), bty='l',
         trace.label = deparse(substitute(Recipientes)), lwd=2.5)

#------------------------------------------------------------------------
# Analise de variancia                                                  -
#------------------------------------------------------------------------

mod = with(dados, aov(resp ~ tratamento01 + tratamento02 + tratamento01*
                              tratamento02))
summary(mod)

fat.medias = model.tables(mod, ty="means")
fat.medias

# ou

mod02 = with(dados, aov(resp ~ tratamento01*tratamento02))
summary(mod02)

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

par(mai=c(1, 1, .5, .2))
plot(mod, which=c(2:2), pch=19, col='red', las=1)

library(hnp)
hnp(mod,print.on = T)

#------------------------------------------------------------------------
# Homogeneidade de variancias:                                          -
# H0: ha homogeneidade de variancia;                                    -
# H1: n?o h? homogeneidade de variancias.                               -
#------------------------------------------------------------------------

# Teste de Bartlett
with(dados, bartlett.test(mod$res ~ trat))

#------------------------------------------------------------------------
# Desdobramento                                                         -
#------------------------------------------------------------------------

#------------------------------------------------------------------------
# Especies dentro de cada Recipiente                                    -
#------------------------------------------------------------------------

fat_trat01_trat02 = with(dados, aov(resp ~ tratamento01/tratamento02))
summary(fat_trat01_trat02, split=list("tratamento01:tratamento02"=list(r1=1,r2=2,r3=3)))

#------------------------------------------------------------------------
# Recipientes dentro de cada Especie                                    -
#------------------------------------------------------------------------

fat_trat02_trat01 = with(dados, aov(resp ~ tratamento02/tratamento01))
summary(fat_trat02_trat01, split=list("tratamento02:tratamento01"=list(e1=c(1,3), e2=c(2,4))))

#------------------------------------------------------------------------
# Usando o pacote ExpDes.pt e easyanova                                             -
#------------------------------------------------------------------------

#ExpDes.pt
library(ExpDes.pt)
fat2.dic(tratamento01, tratamento02, resp, quali=c(TRUE, TRUE), mcomp="tukey",
         fac.names=c("Recipientes", "Especies"), sigT = 0.05, sigF = 0.05)

#easyanova
# ? necessario adequar os dados:
dados.t = with(dados, (data.frame(tratamento01, tratamento02, resp)))
library(easyanova)
mod.1 = ea2(dados.t, design=1)
mod.1

#------------------------------------------------------------------------
# FIM                                                                   -
#------------------------------------------------------------------------