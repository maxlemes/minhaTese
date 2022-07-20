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
source("R/myFunctions.R")

ibra <- b3_ibra() %>%
  dplyr::arrange(Ticker)

#------ Atualizando os dados fundamentalistas -----
urlstatus <- readr::read_file("data-raw/status.txt")
status <- readr::read_csv2(urlstatus)

status <- status %>%
  dplyr::rename(Ticker = "TICKER", Preço = "PRECO")

# Baixando os dados dos ativos
Ra.SA <- ibra$yahoo %>%
  tidyquant::tq_get(get  = "stock.prices",
                    from = Sys.Date()-183,
                    to   = Sys.Date()) %>%
  dplyr::group_by(symbol)

# baixando os dados do Benchmark (Ibovespa)
RaIBOV <- "^BVSP" %>%
  tidyquant::tq_get(get  = "stock.prices",
                    from = Sys.Date()-183,
                    to   = Sys.Date())

RaIBOV$symbol <- "IBOV"

Ra.SA <- rbind(Ra.SA, RaIBOV)

# Salvando os dados
save(Ra.SA, file = "data/RaSA.rda")
save(Ra.SA, file = paste0("data/RaSA-",format(Sys.Date(), "%B-%y"),".rda"))

#------ Atualizando a taxa Selic --------------------
selic <- Quandl::Quandl(code = 'BCB/4389',
                        type='raw',
                        start_date = Sys.Date()-7,
                        end_date = Sys.Date())
rf <- selic[1,2]/100


#Retornos Diários dos Ativos
RaD <- Ra.SA %>% #[Ra.SA$symbol != "IBOV", ] %>%
  tidyquant::tq_transmute(select = adjusted, 
                          mutate_fun = periodReturn, 
                          period     = "daily", 
                          col_rename = "Ra") %>%
  dplyr::rename(yahoo = "symbol")

# Rb  = Retornos Diários do Benchmark (Ibovespa)
RaD_IBOV <- Ra.SA[Ra.SA$symbol == "IBOV", ] %>%
  tidyquant::tq_transmute(select     = adjusted, 
                          mutate_fun = periodReturn, 
                          period     = "daily", 
                          col_rename = "Rb")

# Juntanto tudo 
RaD <- left_join(RaD, RaD_IBOV[, 2:3], by = "date")

#Tabela dos retornos anuais dos ativos
RaRb_annualized_returns <- RaD  %>%
  tq_performance(Ra = Ra, Rb = NULL, 
                 Rf = rf/252,  #juros simples
                 performance_fun = table.AnnualizedReturns)


# Arrumando os retornos e o sharpe ----------------------------------
SR <- left_join(ibra, RaRb_annualized_returns, by = ("yahoo" = "yahoo"))
colnames(SR) <- gsub("Annualized","", colnames(SR))

#------------------- Definindo o Momentum  ---------------------------
# Considerando os retornos diário dos últimos 6 meses ----------

Ra6M <- RaD 

ret <- pivot_wider(Ra6M,names_from = yahoo, values_from = Ra)
pos_returns <- pos_neg(ret[,2:ncol(ret)])

Ra6M <- Ra6M %>%
  group_by(yahoo)%>%
  summarise(mom = prod(1+Ra)-1) 

mom <- left_join(Ra6M, pos_returns, by = ("yahoo" = "yahoo"))
SR <- left_join(SR, mom, by = ("yahoo" = "yahoo"))

#------------------- Juntanto com status -------------
status <- dplyr::left_join(SR,status, by = ("Ticker" = "Ticker"))

status[is.na(status)] <- 0

status <- status[,c(1:6,8:ncol(status),7)] 

# Salvando os dados
save(status, file = "data/status.rda")
save(status, file = paste0("data/status-",format(Sys.Date(), "%B-%y"),".rda"))

#-------------------- AUTENTICAR no GOOGLE DRIVE 
#logar na conta do GDrive
drive_auth(email = "maxvlemes@gmail.com",
           scopes = c(
             'https://www.googleapis.com/auth/drive',
             'https://www.googleapis.com/auth/spreadsheets'))

gs4_auth(token = drive_token())

# ----------------------- 1a vez criar a planilha, depois baixá-la
# #criar uma planilha
ss4 <- googlesheets4::gs4_create(
  paste0("Stocks-", format(Sys.Date(), "%Y")),
  sheets = format(Sys.Date(), "%b%y")
)

#baixando uma planilha
ss4 <- drive_get(paste0("Stocks-", format(Sys.Date(), "%Y")))

#escrever em uma sheet (SS)
status <- status[,-6] #eliminar a coluna yahoo
sheet_write(status, ss=ss4, sheet = format(Sys.Date(), "%b%y"))

#criar um folder
momentum <- drive_mkdir("Max-Durval/momentum")

#ler o caminho um folder
oldMomentum <- drive_get("oldMomentum")

# copiar a planilha para o folder
drive_cp(
  file = paste0("Stocks-", format(Sys.Date(), "%Y")),
  name = paste0("OLD_Stocks-", format(Sys.Date(), "%B-%Y")),
  path = oldMomentum
)

# copiar a planilha para o folder
drive_mv(
  file = paste0("Stocks-", format(Sys.Date(), "%Y")),
  path = momentum
)


