rm(list = ls())

library(tidyverse)
library(lubridate)
library(tidyquant)
library(highcharter)
library(googledrive)
library(googlesheets4)
library(Quandl) #necessario para baixar os dados do BCB
# register api key
Quandl::Quandl.api_key('rF7oAYXPLfgdxZ8Y7sCP')

# lendo o arquivos das funcoes
load("data/status.rda")

status <- status[,-6]

setores <- as.factor(status$Subsetor) %>%
  unique()

subsetores <- new.env()

sink("data/subsertores.R")
for (i in seq(1,length(setores),1)){
  cat(
    paste0('subsetores$S',i,' <- status[status$Subsetor == setores[',i,'],]')
  )
  cat("\n")
}
sink()

source("data/subsertores.R")

#-------------------- AUTENTICAR no GOOGLE DRIVE 
#logar na conta do GDrive
drive_auth(email = "maxvlemes@gmail.com",
           scopes = c(
             'https://www.googleapis.com/auth/drive',
             'https://www.googleapis.com/auth/spreadsheets'))

gs4_auth(token = drive_token())

#ler o caminho um folder
momentum <- drive_get("momentum")

# ----------------------- 1a vez criar a planilha, depois baixá-la
# #criar uma planilha
ss4 <- googlesheets4::gs4_create(
  paste0("Subsetores-", format(Sys.Date(), "%B-%Y")),
  sheets = "Serviços Médico - Hospitalares, Análises e Diagnósticos"
  )


for (i in 1:length(setores)){
#escrever em uma sheet (SS)
sheet_write(subsetores[[paste0("S",i)]], ss=ss4, sheet = paste0(setores[i]))
}

# copiar a planilha para o folder
drive_mv(
  paste0("Subsetores-", format(Sys.Date(), "%B-%Y")),
  path = momentum
)


