rm(list = ls())

library(tidyverse)
library(lubridate)
library(tidyquant)


# montando a composição do IBOV
file <- list.files(path="data-raw/", pattern=".csv")

i = 1

file <- file[i]

df <- read.table(paste0("data-raw/",file), sep = ";", dec = ",", header = FALSE, fill = TRUE)

if (i==1){
  df <- df[-c(1,nrow(df)-1,nrow(df)),]
} else {
  df <- df[3:94,]
}
# 

df$yahoo <- paste0(df[,1], ".SA")
df <- df[,c(2,1,6,5)]
colnames(df)[c(1,2,4)] <- c("Nome", "Ticker", "Peso")

 df$Peso <- as.numeric(sub(",", ".", df$Peso, fixed = TRUE))
# 
 df$Peso <- df$Peso/100

# colnames(df)[2] <- c("PesoJan")
# dff <- df

# colnames(df)[2] <- c("PesoMai")
# df <- full_join(dff, df)

df <- df %>% arrange(yahoo)
#df[is.na(df)] <- 0

# retirando os tickers antigos
 df <- df[!(df$yahoo %in% c("PETR3.SA", "GNDI3.SA", "OMGE3.SA", "PINE3.SA", "RLOG3.SA", "TESA3.SA")),]

 # # Salvando os dados
 save(df, file = "data/df.ibov.rda")
 
# # Salvando os dados
save(df, file = "data/Pesos.rda")

# Escolhendo os ativos
# load("data/Pesos.rda")

options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)

tickers <- as.character(c(df$yahoo))
# tickers <- listadas
# 
# loadSymbols(Symbols = df$yahoo, 
#             env = parent.frame(),
#             reload.Symbols = FALSE,
#             verbose = FALSE,
#             warnings = TRUE,
#             src = "yahoo",
#             symbol.lookup = TRUE,
#             auto.assign = getOption('loadSymbols.auto.assign',TRUE),
#             ...)

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

tickers <- tickers[!(tickers %in% c("GNDI3.SA", "OMGE3.SA", "PINE3.SA", "RLOG3.SA", "TESA3.SA"))]

# Returns
returns <- map(tickers,function(x) dailyReturn(Ad(get(x))))
returns <- reduce(returns,merge)
colnames(returns) <- tickers
ibov_ret <- returns
ibov_ret <- na.fill(ibov_ret, fill=0.0)

# # Salvando os dados
save(ibov_ret, file = "data/ibov_ret.rda")






# 
# # Baixando as cotações dos ativos
# ibov_symbols <- as.character(c(df$yahoo))
# 
# ibov_precos <- stockDataDownload(stock_symbols = ibov_symbols, 
#                           from = "2015-12-28", 
#                           to   = "2022-07-02",
#                           rm_stocks_with_na = TRUE)
# 
# # Calculando os retornos diários
# ibov_ret <- Return.calculate(ibov_precos$adjusted, method = c("discrete", "log"))
# ibov_ret <- na.fill(ibov_ret, fill=0.0)


faang_data <- stockDataDownload(c("GOOG", "NFLX", "AAPL", "AMZN", "FB"),from = "2015-12-28", 
  to   = "2022-07-02")

index_data <- faang_data <- stockDataDownload(c("BOVA11.SA", "IVVB11.SA"),from = "2015-12-28", 
                                              to   = "2022-07-02")

# # Salvando os dados
save(ibov_ret, file = "data/ibov_ret.rda")
save(ibov_precos, file = "data/ibov_precos.rda")
save(faang_data, file = "data/faang_data.rda")
save(index_data, file = "data/index_data.rda")



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
