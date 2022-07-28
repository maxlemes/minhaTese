rm(list = ls())

library(tidyverse) # necessario
library(lubridate) # necessario
library(tidyquant) # necessario
library(PortfolioAnalytics) 

# Escolhendo os ativos --------------------------------------------------------

# Escolhendo os ativos pelos pesos do IBOV
load("data/Pesos.rda")
load("data/df.IBOV.rda")

symbols <- df$yahoo
# Escolhendo as com maior percenutal no IBOV
# df <- df[(df$Peso<=0.001425)|(df$Peso>=0.033728),]


# # Escolhendo os ativos
 symbols <- c("ABEV3.SA", "B3SA3.SA", "BBDC4.SA", "BBAS3.SA", "LREN3.SA", "WEGE3.SA", "ITSA4.SA", "PETR4.SA", "ITUB4.SA", "VALE3.SA")

#listadas <- df$yahoo

#symbols <-  c("IRBR3.SA", "GOLL4.SA", "AZUL4.SA", "HBOR3.SA", "AMAR3.SA",  "POSI3.SA", "BPAN4.SA", "PMAM3.SA", "OIBR3.SA", "CVCB3.SA")

# # Escolhendo as com menor percenutal no IBOV
# df <- df %>%
#   arrange(-Peso) %>%
#   .[1:15,] %>%
#   arrange(yahoo)

# symbols <- c(df$yahoo, symbols)

df <- df %>% 
  filter(yahoo %in% symbols) %>%
  arrange(yahoo)


# Lendo os precos dos ativos
#load("data/ibov_precos.rda")
load("data/ibov_ret.rda")

df <- df[df$yahoo %in% names(ibov_ret),c(1:3)]

# ordenando as colunas
ibov_ret <- ibov_ret[,names(ibov_ret) %in% df$yahoo]
ibov_ret <- ibov_ret[,order(names(ibov_ret))]

# Mais voláteis
# dg <- reshape2::melt(StdDev(ibov_ret),  id.vars = 'Stocks', variable.name = 'Vol')
# dg <- dg[,2:3]
# 
# # Vitorioso mais voláteis
# dg <- dg %>% 
#   arrange(-value) %>%
#   .[1:10,]
# volateis <- df$yahoo
# 
# df <- df %>% 
#   filter(yahoo %in% dg$Var2) %>%
#   arrange(yahoo) %>%
#   .[,2:3]

# # ordenando as colunas
# ibov_ret <- ibov_ret[,names(ibov_ret) %in% df$yahoo]
# ibov_ret <- ibov_ret[,order(names(ibov_ret))]

# Testando se inclui todas as açoes
length(df$yahoo %in% names(ibov_ret)) - length(df$yahoo)

# Selecionando o periodo
datas <- format(seq(as.Date("2016-01-01"), as.Date("2022-07-01"), by="months"), format="%Y-%m-%d")

# removendo os dados antigos do diretorio antes de rodar o algoritmo
unlink("../../Numericaltests/max/dRet/*")
unlink("../../Numericaltests/max/dCov/*")
unlink("../../Numericaltests/max/dStdev/*")

for (i in 1:(length(datas)-12)){
    
    # definindo o periodo  ----------------------------------------
    data_ini <- date(datas[i])
    data_fim <- date(datas[i+12])
    
    # Filtrando os retornos no periodo
    R <- ibov_ret
    R <- R[index(R) >= data_ini]
    R <- R[index(R) <  data_fim]
    
    # Calculando os retornos
    ret <- colMeans(R)
    
    # Salvando os novos dados
    if (i < 10){
      write.csv(ret, file = paste0("../../Numericaltests/max/dRet/dRet0",i,".csv"), row.names=FALSE)
    }
    else {
      write.csv(ret, file = paste0("../../Numericaltests/max/dRet/dRet",i,".csv"), row.names=FALSE)
    }
    
    # Calculando a matriz de covariancia
    R_cov <- cov(R)
    
    # Salvando os novos dados
    if (i < 10){
      write.csv(R_cov, file = paste0("../../Numericaltests/max/dCov/iCov0",i,".csv"), row.names=FALSE)
    }
    else {
      write.csv(R_cov, file = paste0("../../Numericaltests/max/dCov/iCov",i,".csv"), row.names=FALSE)
    }
    
    # Calculando a Volatilidade
    vol <- StdDev(R)
    
    # Salvando os novos dados
    if (i < 10){
      write.csv(vol, file = paste0("../../Numericaltests/max/dStdev/Stdev0",i,".csv"), row.names=FALSE)
    }
    else {
      write.csv(vol, file = paste0("../../Numericaltests/max/dStdev/Stdev",i,".csv"), row.names=FALSE)
    }
}

# Ir para o MatLab

# montando os rebalanceamentos do RPP
file <- list.files(path="data-raw/pesos/rpp/", pattern=".csv")
gile <- list.files(path="data-raw/pesos/mvp/", pattern=".csv")
df <- df[,1:2]
dg <- df

# Capturando os pesos do Matlab
for (i in 1:(length(datas)-12)){
  p <- read.csv(paste0("data-raw/pesos/rpp/",file[i]), header = FALSE)
  q <- read.csv(paste0("data-raw/pesos/mvp/",gile[i]), header = FALSE)
  
  # organizando em colunas
  p <- pivot_longer(p,1:ncol(p),values_to = "peso")
  q <- pivot_longer(q,1:ncol(q),values_to = "peso")
  
  # arrendondando a soma para 1
  p[,2] <- p[,2]/colSums(p[,2])
  q[,2] <- q[,2]/colSums(q[,2])
  
  df[,ncol(df)+1] <- p[,2]
  colnames(df)[ncol(df)] <- gsub(".csv", "", file[i])
  
  dg[,ncol(dg)+1] <- q[,2]
  colnames(dg)[ncol(dg)] <- gsub(".csv", "", gile[i])
}

#dg[,(ncol(dg)-2):ncol(dg)] <- dg[,(ncol(dg)-5):(ncol(dg)-3)]


# Criando um xts com a primeira entrada
 # dt <- datas
 # datas <- dt
 #datas <- dt[1:50] # até 2020-01-01
 #datas <- dt[37:length(dt)] # de 2020-01-01 até o fim
 #datas <- dt[37:61]

data_ini <- date(datas[13])
RPP <- xts(cbind(MVP=0,RPP=0), data_ini)

# Calculando o Retorno do portfolio
for (i in 1:(length(datas)-13)){

  # definindo o periodo  ----------------------------------------
  data_fim <- date(datas[i+13])
  
  R <- ibov_ret
  R <- R[index(R) > index(RPP)[nrow(RPP)]]
  R <- R[index(R) < data_fim]
  
  # Equal Weigth Porfolio - EWP ou MVP
  we <- dg[,2+i]
  ew <- Return.portfolio(R, 
                         weights = we,
                         rebalance_on = NA, 
                         geometric = TRUE, 
                         wealth.index = FALSE,
                         verbose = FALSE
                         )
  colnames(ew) <- "MVP"
  ERP <- ew
  
  # Risk Parity Portfolio - RPP
  wp <- df[,2+i]
  rp <- Return.portfolio(R, 
                         weights = wp,
                         rebalance_on = NA, 
                         geometric = TRUE, 
                         wealth.index = FALSE,
                         verbose = FALSE
  )
  ERP$RPP <- rp
  
  RPP <- rbind(RPP, ERP)
  
  print(i)
}

RPP_ret<-RPP

RPP$MVP <- cumprod(1+RPP$MVP)
RPP$RPP <- cumprod(1+RPP$RPP)

RPP1 <- RPP[index(RPP)<="2019-12-30"]

RPP2 <- RPP[index(RPP)>="2019-12-30"]
RPP2$MVP <- RPP2$MVP/RPP2$MVP[[1]]
RPP2$RPP <- RPP2$RPP/RPP2$RPP[[1]]

# Plot 
ggplot(RPP1, aes(x=Index)) +
  geom_line(aes(y=RPP, color = "RPP"), size=1)+
  geom_line(aes(y=MVP, color = "MVP"), size=1)

head(RPP)
tail(RPP)
table.AnnualizedReturns(Return.calculate(RPP))
table.AnnualizedReturns(Return.calculate(RPP1))
table.AnnualizedReturns(Return.calculate(RPP2))
# table.CalendarReturns((RPP$MVP))
# table.CalendarReturns((RPP$RPP))
# Salvando os dados
save(RPP, file = "data/RPP.rda")
save(RPP, file = "data/RPP1.rda")
save(RPP, file = "data/RPP2.rda")


# ------ Definindo as contribuiçoes de Risco ---------
for (i in 1:(length(datas)-13)){ #-13

  # definindo o periodo  ----------------------------------------
  data_ini <- date(datas[i+1]) # +1
  data_fim <- date(datas[i+13])      # +13
  
  R <- ibov_ret
  R <- R[index(R) >= data_ini]
  R <- R[index(R) < data_fim]
  
  # Calculando a matriz de covariancia
  R_cov <- cov(R)
  
  # definindo os pesos do período 
  pesos <- df[,2+i] # RPP
  
  # Volatilidade do portfólio
  sigma <- pesos %*% R_cov %*% pesos
  sigma <- sqrt(sigma)
  
  # Risk contribution
  v <- R_cov %*% pesos
  dp <- sqrt(252)*(pesos * v )/sigma[[1]]
  dp <- dp  %>%
    as.data.frame() %>%
    rownames_to_column(var = 'yahoo') %>%
    pivot_longer(cols = -yahoo, names_to = 'group2') %>%
    .[,c(1,3)]
  
  df[,ncol(df)+1] <- dp[,2]/sum(dp[,2])
  
  if (i < 10){
    colnames(df)[ncol(df)] <- paste0("RPP0",i)
  }
  else{
    colnames(df)[ncol(df)] <- paste0("RPP",i)
  }
  
  # ----------------------- MVP ----------------------------------
  
  # definindo os pesos do período 
  # pesos <- rep(1/nrow(df), nrow(df)) # Pesos iguais
  pesos <- dg[,2+i] # MVP
  
  # Volatilidade do portfólio
  sigma <- pesos %*% R_cov %*% pesos
  sigma <- sqrt(sigma)
  
  # Risk contribution
  v <- R_cov %*% pesos
  dp <- sqrt(252)*(pesos * v )/sigma[[1]]
  dp <- dp  %>%
    as.data.frame() %>%
    rownames_to_column(var = 'yahoo') %>%
    pivot_longer(cols = -yahoo, names_to = 'group2') %>%
    .[,c(1,3)]
  
  dg[,ncol(dg)+1] <- dp[,2]/sum(dp[,2])
  
  if (i < 10){
    colnames(dg)[ncol(dg)] <- paste0("MVP0",i)
  }
  else{
    colnames(dg)[ncol(dg)] <- paste0("MVP",i)
  }
  print(i)
}

# Salvando os dados
save(df, file = "data/RiskContributionsRPP.rda")
save(dg, file = "data/RiskContributionsMVP.rda")

