#------------------------------------------------------------------------
# Comandos de ajuda                                                     -
#------------------------------------------------------------------------

help.start()
help(mean)
?(mean)

#------------------------------------------------------------------------
# Limpar dados armazenados                                              -
#------------------------------------------------------------------------

rm(list=ls())

#------------------------------------------------------------------------
# Instalar Pacotes                                                      -
#------------------------------------------------------------------------

install.packages('gsheet', dependencies = T)

#------------------------------------------------------------------------
# Ativar Pacotes                                                        -
#------------------------------------------------------------------------

library(gsheet)
require(gsheet)

#------------------------------------------------------------------------
# Entrada de Dados                                                      -
#------------------------------------------------------------------------

#Vetores:
resposta <- c(35,19,31,15,30,40,35,46,41,33)
croqui <- expand.grid(rep=1:5, trat=LETTERS[1:2])
data.frame(croqui, resposta)

dados_1 <- data.frame(croqui, resposta)
attach(dados_1)

#Abrir uma Planilha:
dados_2 <- edit(data.frame())
attach(dados_2)

#Google Sheet:
library(gsheet)
dados_3 <- gsheet2tbl('https://docs.google.com/spreadsheets/d/13_JObv8B1d5hRSstmUkmegXRcZXPez-ec1CRfMD0sOc/edit?usp=sharing')
attach(dados_3)
