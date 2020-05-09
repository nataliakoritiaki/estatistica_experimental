#Comandos de ajuda
help.start()
help(mean)
?(mean)

# Limpar dados armazenados
rm(list=ls())

#Instalar Pacotes
#install.packages('gsheet')

#Ativar Pacotes
library(gsheet)
require(gsheet)

#Vetores
resposta <- c(35,19,31,15,30,40,35,46,41,33)
croqui <- expand.grid(rep=1:5, trat=LETTERS[1:2])
data.frame(croqui, resposta)

#Dados
dados <- data.frame(croqui, resposta)
attach(dados)

dados_2 <- edit(data.frame())
attach(dados_2)
