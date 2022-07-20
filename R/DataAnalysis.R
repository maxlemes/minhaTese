rm(list = ls())

library(portfolioBacktest)
library(tidyverse)
library(lubridate)
library(tidyquant)
library(highcharter)
library(googledrive)
library(googlesheets4)
# 
# library(Quandl) #necessario para baixar os dados do BCB
# # register api key
# Quandl::Quandl.api_key('rF7oAYXPLfgdxZ8Y7sCP')

# lendo o arquivos das funcoes
# source("myFunctions.R")


# Escolhendo os ativos pelos pesos do IBOV
load("data/Pesos.rda")

# Escolhendo as com maior percenutal no IBOV
# df <- df %>%
#   arrange(-PesoJan) %>%
#   .[1:50,] %>%
#   arrange(yahoo)

# Escolhendo os ativos
symbols <- c("ABEV3.SA", "B3SA3.SA", "BBAS3.SA", "BBDC4.SA", "CIEL3.SA","ITSA4.SA", "ITUB4.SA", "PETR4.SA", "SANB11.SA", "VALE3.SA")

df <- df[df$yahoo %in% symbols,]%>%
     arrange(yahoo)

df$EW <- 1/nrow(df)
df <- df[,c(1,4)]

# Lendo os precos dos ativos
load("data/ibov_precos.rda")

# filtrando os dados que interessam
precos <- ibov_precos$adjusted
# filtrando os ativos de interesse
precos <- precos[,names(precos) %in% df$yahoo]

# ordenando as colunas
precos <- precos[,order(names(precos))]

# Calculando os retornos diários
ibov_ret <- na.omit(Return.calculate(precos, method = c("discrete", "log")))

datas <- format(seq(as.Date("2020-05-01"), as.Date("2022-07-01"), by="months"), format="%Y-%m-%d")

for (i in 1:(length(datas)-12)){
    
    # definindo o periodo  ----------------------------------------
    data_ini <- date(datas[i])
    data_fim <- date(datas[i+12])
    
    R <- ibov_ret
    R <- R[index(R) >= data_ini]
    R <- R[index(R) <  data_fim]
    
    # Calculando a matriz de covariancia
    R_cov <- cov(R)
    
    # # Salvando os dados
    if (i < 10){
      write.csv(R_cov, file = paste0("../../Numericaltests/max/dCov/iCov0",i,".csv"), row.names=FALSE)
    }
    else {
      write.csv(R_cov, file = paste0("../../Numericaltests/max/dCov/iCov",i,".csv"), row.names=FALSE)
    }
    
    # Calculando a Volatilidade
    vol <- StdDev(R)
    
    # # Salvando os dados
    if (i < 10){
      write.csv(vol, file = paste0("../../Numericaltests/max/dStdev/Stdev0",i,".csv"), row.names=FALSE)
    }
    else {
      write.csv(vol, file = paste0("../../Numericaltests/max/dStdev/Stdev",i,".csv"), row.names=FALSE)
    }
}

# Ir para o MatLab

# montando os rebalanceamentos do RPP
file <- list.files(path="data-raw/pesos/", pattern=".csv")

# Capturando os pesos do Matlab
for (i in 1:(length(datas)-12)){
  p <- read.csv(paste0("data-raw/pesos/",file[i]), header = FALSE)
  
  # organizando em colunas
  p <- pivot_longer(p,1:ncol(p),values_to = "peso")
  
  # arrendondando a soma para 1
  p[,2] <- p[,2]/colSums(p[,2])
  
  df[,ncol(df)+1] <- p[,2]
  colnames(df)[ncol(df)] <- gsub(".csv", "", file[i])
}

# Criando um xts com a primeira entrada
data_ini <- date(datas[13])
R <- ibov_ret
R <- R[index(R) >= data_ini]
RPP <- R[1,1:2]
colnames(RPP) <- c("X1.N", "RPP")
RPP[,1:2] <- 1

# Calculando o Retorno Acumulado rebalanceamento mensal
for (i in 1:(length(datas)-13)){

  # definindo o periodo  ----------------------------------------
  data_ini <- date(datas[i+12])
  data_fim <- date(datas[i+13])
  
  R <- ibov_ret
  R <- R[index(R) >= data_ini-1]
  R <- R[index(R) < data_fim]
  
  # Equal Weigth Porfolio - EWP
  we <- df$EW
  # we <- runif(nrow(df))
  # we <- we/sum(we)
  # Calculando o retorno acumulado
  # rebalance_on em um período maior que os dados garante o não rebalanceamento
  # no caso de dad
  ew <- Return.portfolio(R, weights = we,rebalance_on = "months", wealth.index = T)
  ew <- ew/ew[[1]]
  ew <- ew*RPP[,1][[nrow(RPP)]]
  colnames(ew) <- "1/N"
  ERP <- ew
  
  # Risk Parity Portfolio - RPP
  wp <- df[,2+i]
  rp <- Return.portfolio(R, weights = wp,rebalance_on = "months", wealth.index = T)
  rp <- rp/rp[[1]]
  rp <- rp*RPP[,2][[nrow(RPP)]]
  
  ERP$RPP <- rp
  
  ERP <- ERP[-1,]
  
  RPP <- rbind(RPP, ERP)
  
  print(i)
}

colnames(RPP)[1] <- "EWP"

port_cumulative_ret <- cumprod(1 + RPP)


rt <- RPP

bt <- Return.portfolio(R, weights = we, rebalance_on = "months",  wealth.index = T)


# Salvando os dados
save(RPP, file = "data/RPP.rda")


# ------ Definindo as contribuiçoes de Risco ---------
for (i in 1:(length(datas)-12)){ #+13

  # definindo o periodo  ----------------------------------------
  data_ini <- date(datas[i]) # +1
  data_fim <- date(datas[i+12])      # +13
  
  R <- ibov_ret
  R <- R[index(R) >= data_ini]
  R <- R[index(R) < data_fim]
  
  # Calculando a matriz de covariancia
  R_cov <- cov(R)
  
  # definindo os pesos do período
  pesos <- df[,2+i]
  
  # Volatilidade do portfólio
  sigma <- pesos %*% R_cov %*% pesos
  sigma <- sqrt(sigma)
  
  # Risk contribution
  v <- R_cov %*% pesos
  dg <- sqrt(252)*(pesos * v )/sigma[[1]]
  dg <- dg  %>%
    as.data.frame() %>%
    rownames_to_column(var = 'yahoo') %>%
    pivot_longer(cols = -yahoo, names_to = 'group2') %>%
    .[,c(1,3)]
  
  df[,ncol(df)+1] <- dg[,2]
  
  if (i < 10){
    colnames(df)[ncol(df)] <- paste0("RPP0",i)
  }
  else{
    colnames(df)[ncol(df)] <- paste0("RPP",i)
  }
  
  # Pesos Iguais
  pesos <- df$EW
  
  # Volatilidade do portfólio
  sigma <- pesos %*% R_cov %*% pesos
  sigma <- sqrt(sigma)
  
  # Risk contribution
  v <- R_cov %*% pesos
  dg <- sqrt(252)*(pesos * v )/sigma[[1]]
  dg <- dg  %>%
    as.data.frame() %>%
    rownames_to_column(var = 'yahoo') %>%
    pivot_longer(cols = -yahoo, names_to = 'group2') %>%
    .[,c(1,3)]
  
  df[,ncol(df)+1] <- dg[,2]
  
  if (i < 10){
    colnames(df)[ncol(df)] <- paste0("EWP0",i)
  }
  else{
    colnames(df)[ncol(df)] <- paste0("EWP",i)
  }
  print(i)
}

# Salvando os dados
save(df, file = "data/RiskContributions.rda")

