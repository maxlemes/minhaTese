rm(list = ls())

library(tidyverse)
library(lubridate)
library(tidyquant)

# IBOV em 2017
ibov17 <- c("ABEV3.SA", "B3SA3.SA", "BBAS3.SA", "BBDC4.SA", "BBSE3.SA", 
            "BRAP4.SA", "BRDT3.SA", "BRFS3.SA", "BRKM5.SA", "BRML3.SA", "BTOW3.SA", 
            "CCRO3.SA", "CIEL3.SA", "CMIG4.SA", "CSAN3.SA", "CSNA3.SA", "CVCB3.SA", 
            "CYRE3.SA", "ECOR3.SA", "EGIE3.SA", "ELET6.SA", "EMBR3.SA", 
            "ENBR3.SA", "EQTL3.SA", "ESTC3.SA", "FLRY3.SA", "GGBR4.SA", "GOAU4.SA", 
            "GOLL4.SA", "HYPE3.SA", "IGTA3.SA", "ITSA4.SA", "ITUB4.SA", "JBSS3.SA", 
            "KLBN11.SA", "KROT3.SA", "LAME4.SA", "LOGG3.SA,", "LREN3.SA", "MGLU3.SA", 
            "MRFG3.SA", "MRVE3.SA", "MULT3.SA", "NATU3.SA", "PCAR4.SA", "PETR4.SA", 
            "QUAL3.SA", "RADL3.SA", "RAIL3.SA", "RENT3.SA", "SANB11.SA", "SBSP3.SA", 
            "SMLS3.SA", "SUZB3.SA", "TAEE11.SA", "TIMP3.SA", "UGPR3.SA", "USIM5.SA", 
            "VALE3.SA", "VIVT4.SA", "VVAR3.SA", "WEGE3.SA")

ibov17 <- c("ABEV3.SA",  "B3SA3.SA",  "BBAS3.SA",  "BBDC4.SA",  "BBSE3.SA",  "BRAP4.SA", 
            "BRFS3.SA",  "BRKM5.SA", "BRML3.SA",  "CCRO3.SA",  "CIEL3.SA", "CMIG4.SA", 
            "CSAN3.SA",  "CSNA3.SA",  "CVCB3.SA",  "CYRE3.SA",  "ECOR3.SA",  "EGIE3.SA", 
            "ELET6.SA",  "EMBR3.SA",  "ENBR3.SA",  "EQTL3.SA",  "FLRY3.SA",  "GGBR4.SA", 
            "GOAU4.SA",  "GOLL4.SA",  "HYPE3.SA",  "ITSA4.SA",  "ITUB4.SA",  "JBSS3.SA", 
            "KLBN11.SA", "LREN3.SA",  "MGLU3.SA",  "MRFG3.SA",  "MRVE3.SA",  "MULT3.SA", 
            "PETR4.SA",  "QUAL3.SA",  "RADL3.SA",  "RAIL3.SA",  "RENT3.SA",  "SANB11.SA", 
            "SBSP3.SA",  "SUZB3.SA",  "TAEE11.SA", "USIM5.SA",  "VALE3.SA",  "WEGE3.SA") 


df <- tibble(
  yahoo =ibov17
)

tickers <- ibov17

# data.env <- new.env()
getSymbols(tickers,
           src = "yahoo",
           from = "2015-12-28", 
           to   = "2022-07-02",
           #    env=data.env,
           reload.Symbols = FALSE,
           verbose = TRUE)

# Prices
prices <- map(tickers,function(x) Ad(get(x)))
prices <- reduce(prices,merge)
colnames(prices) <- tickers

# Returns
returns <- map(tickers,function(x) dailyReturn(Ad(get(x))))
returns <- reduce(returns,merge)
#returns <- na.fill(returns, fill=0.0)
colnames(returns) <- tickers

# Filtrando Jan16-Jun22
prices <- prices[-c(1:3, nrow(prices)),]
returns <- returns[-c(1:3, nrow(returns)),]


# # Salvando os dados
save(returns, file = "data/returns.rda")

# Salvando as planilhas
write.csv(prices, file = "data-raw/prices.csv", row.names=FALSE)
write.csv(returns, file = "data-raw/returns.csv", row.names=FALSE)


# IBOV em 2017
# Ambev (ABEV3): 5,138%
# B3 (B3SA3): 4,154%
# Banco do Brasil (BBAS3): 4,468%
# Bradesco (BBDC3): 1,742%
# Bradesco (BBDC4): 9,119%
# BB Seguridade (BBSE3): 1,317%
# Bradespar (BRAP4): 0,391%
# Petrobras Distribuidora (BRDT3): 0,546%
# BRF (BRFS3): 1,236%
# Braskem (BRKM5): 0,894%
# BRMalls (BRML3): 0,765%
# B2W Digital (BTOW3): 0,490%
# CCR (CCRO3):  1.025%
# Cielo (CIEL3): 0,770%
# Cemig (CMIG4): 0,843%
# Cosan (CSAN3): 0,436%
# Companhia Siderúrgica Nacional (CSNA3): 0,402%
# CVC Brasil (CVCB3): 0,573%
# Cyrella Brazil Realty (CYRE3): 0,282%
# EcoRodovias (ECOR3): 0,141%
# ENGIE Brasil (EGIE3): 0,717%
# Eletrobras (ELET3): 0,626%
# Eletrobras (ELET6): 0,548%
# Embraer (EMBR3): 0,901%
# EDP Brasil (ENBR3): 0,339%
# Equatorial Energia (EQTL3): 1,092%
# Estacio Participacoes (ESTC3): 0,569%
# Fleury S.A. (FLRY3): 0,432%
# Gerdau (GGBR4): 0,984%
# Gerdau (GOAU4): 0,265%
# Gol (GOLL4): 0,233%
# Hypera (HYPE3): 0,814%
# Iguatemi (IGTA3): 0,248%
# Itaú SA (ITSA4): 3,855%
# Itaú Unibanco (ITUB4): 10,502%
# JBS (JBSS3): 1,482%
# Klabin SA (KLBN11): 0,738%
# Kroton (KROT3): 1,064%
# Lojas Americanas (LAME4): 0,908%
# LOG Commercial Properties (LOGG3): 0,026%
# Lojas Renner (LREN3): 1,996%
# Magazine Luiza (MGLU3): 0,745%
# Marfrig (MRFG3): 0,155%
# MRV (MRVE3): 0,274%
# Multiplan (MULT3): 0,440%
# Natura (NATU3): 0,505%
# Pão de Açúcar (PCAR4): 0,943%
# Petrobras (PETR3): 5,143%
# Petrobras (PETR4): 7,061%
# Qualicorp (QUAL3): 0,234%
# Raia Drogasil (RADL3): 0,825%
# Rumo S.A. (RAIL3): 1,406%
# Localiza (RENT3): 1,085%
# Santander (SANB11): 1,183%
# Sabesp (SBSP3): 0,836%
# Smiles (SMLS3): 0,176%
# Suzano Papel (SUZB3): 1,792%
# Taesa S.A. (TAEE11): 0,366%
# TIM (TIMP3): 0,618%
# Ultrapar (UGPR3): 1,891%
# Usiminas (USIM5): 0,320%
# Vale (VALE3): 8,585%
# Telefônica Brasil (VIVT4): 1,287%
# Via Varejo (VVAR3): 0,146%
# WEG (WEGE3): 0,912%


# IBOV em 2017
# ABEV3.SA, B3SA3.SA, BBAS3.SA, BBDC3.SA, BBDC4.SA, BBSE3.SA, BRAP4.SA, BRDT3.SA, 
# BRFS3.SA, BRKM5.SA, BRML3.SA, BTOW3.SA, CCRO3.SA, CIEL3.SA, CMIG4.SA, CSAN3.SA, 
# CSNA3.SA, CVCB3.SA, CYRE3.SA, ECOR3.SA, EGIE3.SA, ELET3.SA, ELET6.SA, EMBR3.SA, 
# ENBR3.SA, EQTL3.SA, ESTC3.SA, FLRY3.SA, GGBR4.SA, GOAU4.SA, GOLL4.SA, HYPE3.SA, 
# IGTA3.SA, ITSA4.SA, ITUB4.SA, JBSS3.SA, KLBN11.SA, KROT3.SA, LAME4.SA, LOGG3.SA, 
# LREN3.SA, MGLU3.SA, MRFG3.SA, MRVE3.SA, MULT3.SA, NATU3.SA, PCAR4.SA, PETR3.SA, 
# PETR4.SA, QUAL3.SA, RADL3.SA, RAIL3.SA, RENT3.SA, SANB11.SA, SBSP3.SA, SMLS3.SA, 
# SUZB3.SA, TAEE11.SA, TIMP3.SA, UGPR3.SA, USIM5.SA, VALE3.SA, VIVT4.SA, VVAR3.SA, 
# WEGE3.SA


