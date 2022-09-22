rm(list = ls())

library(tidyverse)
library(lubridate)
library(tidyquant)

# IBOV em 2017
df <- read_csv("data-raw/ibov2017.csv",
                col_names = TRUE  # Usa a 1a linha como colnames
               )

df$yahoo <- paste0(df$Ticker, ".SA")

# Ações que mudaram o ticker 
blacklist <- c("BVMF3.SA", "ESTC3.SA", "FIBR3.SA", "KROT3.SA", "LAME4.SA", 
               "NATU3.SA", "PCAR4.SA", "SMLE3.SA", "SUZB5.SA", "VIVT4.SA", 
               "TIMP3.SA"
               )

df <- df[!(df$yahoo %in% blacklist),]
df$Peso <- df$Peso/sum(df$Peso)

tickers <- c(df$yahoo, '^BVSP')

df <- df[,-3]
# # Salvando os dados
save(df, file = "data/ibov2017.rda")

# data.env <- new.env()
getSymbols(tickers,
           src = "yahoo",
           from = "2015-12-28", 
           to   = "2022-07-02",
           #    env=data.env,
           reload.Symbols = FALSE,
           verbose = TRUE)

# Trocando ^BVSP por IBOV e ajustando os tickers
IBOV <- BVSP
tickers <- gsub('.{3}$', '', tickers)
tickers[tickers=='^B'] <- 'IBOV'

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
save(prices, file = "data/prices.rda")
save(returns, file = "data/returns.rda")

# Salvando as planilhas
write.csv(prices, file = "data-raw/prices.csv", row.names=FALSE)
write.csv(returns, file = "data-raw/returns.csv", row.names=FALSE)


