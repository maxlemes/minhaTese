rm(list = ls())

library(portfolioBacktest)
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
source("myFunctions.R")

# montando a composição do IBOV
file <- list.files(path="data-raw/", pattern=".csv")

i = 2

file <- file[i]

df <- read.table(paste0("data-raw/",file), sep = ";", dec = ",", header = FALSE, fill = TRUE)

if (i==1){
  df <- df[-c(1,nrow(df)-1,nrow(df)),]
} else {
  df <- df[3:94,]
}

df$yahoo <- paste0(df[,1], ".SA")

df <- df[,c(ncol(df),5)] 

colnames(df)[2] <- c("Peso")

df$Peso <- as.numeric(sub(",", ".", df$Peso, fixed = TRUE))

df$Peso <- df$Peso/100

# colnames(df)[2] <- c("PesoJan")
# dff <- df

# colnames(df)[2] <- c("PesoMai")

df <- full_join(dff, df)

df <- df %>% arrange(yahoo)

df[is.na(df)] <- 0

# retirando os tickers antigos
df <- df[!(df$yahoo %in% c("GNDI3.SA", "LAME4.SA", "BIDI11.SA")),]

# # Salvando os dados
save(df, file = "data/Pesos.rda")

# Escolhendo os ativos
load("data/Pesos.rda")

# Baixando as cotações dos ativos
ibov_symbols <- as.character(c(df$yahoo))

ibov_precos <- stockDataDownload(stock_symbols = ibov_symbols, 
                          from = "2015-12-28", 
                          to   = "2022-07-01")

# # Salvando os dados
save(ibov_precos, file = "data/ibov_precos.rda")
