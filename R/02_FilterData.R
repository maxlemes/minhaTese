rm(list = ls())

library(tidyverse) # necessario
library(lubridate) # necessario
library(tidyquant) # necessario
library(PortfolioAnalytics) 

# Escolhendo os ativos -------------------------------------------------------

# carregando os ativos e seus retornos
load("data/ibov2017.rda")
load("data/returns.rda")

tickers <- colnames(returns)

# Selecionandos segundo a relevancia no indice
d10 <- tibble(
  # As 10 mais relevantes no ibov
  df %>% 
    arrange(-Peso) %>%
    .[1:10,1] %>%
    rename(High10 = Ticker),
  # As 10 menos relevantes no ibov
  df %>% 
    arrange(Peso) %>%
    .[1:10,1] %>%
    rename(Low10 = Ticker),
  # As 5 mais relevantes e as 5 menos relevantes no ibov
  df %>% 
    arrange(-Peso) %>%
    .[c(1:5, (nrow(df)-4):nrow(df)),1] %>%
    rename(HighLow = Ticker)
)


# Selecionando segundo a volatilidade em 2016

# separando os dados do 1o ano
ret16 <- returns
ret16 <- ret16[index(ret16) >= "2016-01-01"]
ret16 <- ret16[index(ret16) <  "2017-01-01"]


# Mais voláteis
df <- reshape2::melt(StdDev(ret16)) %>%
   arrange(-value) %>%
   rename(Ticker = "Var2", 
          vol = "value") %>%
   .[,2:3]

df$Ticker <- as.character(df$Ticker)
df <- as_tibble(df)

# Selecionandos segundo a relevancia no indice
v10 <- tibble(
  # As 10 mais relevantes no ibov
  df %>% 
    arrange(-vol) %>%
    .[1:10,1] %>%
    rename(volHigh10 = Ticker),
  # As 10 menos relevantes no ibov
  df %>% 
    arrange(vol) %>%
    .[1:10,1] %>%
    rename(volLow10 = Ticker),
  # As 5 mais relevantes e as 5 menos relevantes no ibov
  df %>% 
    arrange(-vol) %>%
    .[c(1:5, (nrow(df)-4):nrow(df)),1] %>%
    rename(volHighLow = Ticker)
)
 
d10 <- cbind(d10, v10) 

ativos10 <- c(d10[[1]], d10[[2]], d10[[4]], d10[[5]]) %>%
  unique() %>% 
  sort()


# Salvando os dados
save(d10, file = "data/Best10.rda")

