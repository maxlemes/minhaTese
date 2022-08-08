rm(list = ls())

library(tidyverse) # necessario
library(lubridate) # necessario
library(tidyquant) # necessario
library(PortfolioAnalytics) 

# Escolhendo os ativos --------------------------------------------------------

# carregando os retornos dos ativos
load("data/returns.rda")

tickers <- colnames(returns)

# # Escolhendo os 10 maiores ativos
# tickers <- c("ABEV3.SA", "B3SA3.SA", "BBDC4.SA", "BBAS3.SA", "LREN3.SA", "WEGE3.SA", "ITSA4.SA", "PETR4.SA", "ITUB4.SA", "VALE3.SA")

# separando os dados do 1o ano
ret16 <- returns
ret16 <- ret16[index(ret16) >= "2016-01-01"]
ret16 <- ret16[index(ret16) <  "2017-01-01"]


# Mais voláteis
 dg <- reshape2::melt(StdDev(ret16)) %>%
   arrange(-value) %>%
   rename(yahoo = "Var2", 
          vol = "value") %>%
   .[,2:3] 


# Selecionando as 5 mais voláteis e as 5 menos voláteis
n <- 5
m <- nrow(dg)
df <- dg[c(1:n, (m-n):m),]
df <- df[-11,] # tirando a SUZANO

# ordenando as colunas
ibov_ret <- returns
ibov_ret <- ibov_ret[,names(ibov_ret) %in% df$yahoo]
ibov_ret <- ibov_ret[,order(names(ibov_ret))]


# Selecionando o periodo
datas <- format(seq(as.Date("2016-01-01"), as.Date("2022-07-01"), by="months"), format="%Y-%m-%d")

# removendo os dados antigos do diretorio antes de rodar o algoritmo
unlink("../../matlab/dRet/*")
unlink("../../matlab/dCov/*")
unlink("../../matlab/dStdev/*")

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
      write.csv(ret, file = paste0("../../matlab/dRet/dRet0",i,".csv"), row.names=FALSE)
    }
    else {
      write.csv(ret, file = paste0("../../matlab/dRet/dRet",i,".csv"), row.names=FALSE)
    }
    
    # Calculando a matriz de covariancia
    R_cov <- cov(R)
    
    # Salvando os novos dados
    if (i < 10){
      write.csv(R_cov, file = paste0("../../matlab/dCov/iCov0",i,".csv"), row.names=FALSE)
    }
    else {
      write.csv(R_cov, file = paste0("../../matlab/dCov/iCov",i,".csv"), row.names=FALSE)
    }
    
    # Calculando a Volatilidade
    vol <- StdDev(R)
    
    # Salvando os novos dados
    if (i < 10){
      write.csv(vol, file = paste0("../../matlab/dStdev/Stdev0",i,".csv"), row.names=FALSE)
    }
    else {
      write.csv(vol, file = paste0("../../matlab/dStdev/Stdev",i,".csv"), row.names=FALSE)
    }
}

# Ir para o MatLab

# montando os rebalanceamentos do RPP
rpp <- list.files(path="data-raw/pesos/rpp/", pattern=".csv")
mvp <- list.files(path="data-raw/pesos/mvp/", pattern=".csv")
dt <- df

# Capturando os pesos do Matlab
for (i in 1:(length(datas)-12)){
  p <- read.csv(paste0("data-raw/pesos/rpp/",rpp[i]), header = FALSE)
  q <- read.csv(paste0("data-raw/pesos/mvp/",mvp[i]), header = FALSE)
  
  # organizando em colunas
  p <- pivot_longer(p,1:ncol(p),values_to = "peso")
  q <- pivot_longer(q,1:ncol(q),values_to = "peso")
  
  # arrendondando a soma para 1
  p[,2] <- p[,2]/colSums(p[,2])
  q[,2] <- q[,2]/colSums(q[,2])
  
  df[,ncol(df)+1] <- p[,2]
  colnames(df)[ncol(df)] <- gsub(".csv", "", rpp[i])
  
  dt[,ncol(dt)+1] <- q[,2]
  colnames(dt)[ncol(dt)] <- gsub(".csv", "", mvp[i])
}

# Checando se a soma dos pesos é igual a 1
colSums(df[,-c(1:2)])
colSums(dt[,-c(1:2)])

#dg[,(ncol(dg)-2):ncol(dg)] <- dg[,(ncol(dg)-5):(ncol(dg)-3)]


# Criando um xts com a primeira entrada
 # dt <- datas
 # datas <- dt
 #datas <- dt[1:50] # até 2020-01-01
 #datas <- dt[37:length(dt)] # de 2020-01-01 até o fim
 #datas <- dt[37:61]

data_ini <- date(datas[13])
ports_ret <- xts(cbind(MVP=0,RPP=0), data_ini)

# Calculando o Retorno do portfolio
for (i in 1:(length(datas)-13)){

  # definindo o periodo  ----------------------------------------
  data_fim <- date(datas[i+13])
  
  R <- ibov_ret
  R <- R[index(R) > index(ports_ret)[nrow(ports_ret)]]
  R <- R[index(R) < data_fim]
  
  # Minimum Variance Porfolio - MVP
  w <- dt[,2+i]
  mv <- Return.portfolio(R, 
                         weights = w,
                         rebalance_on = NA, 
                         geometric = TRUE, 
                         wealth.index = FALSE,
                         verbose = FALSE
                         )
  colnames(mv) <- "MVP"
  
  # Risk Parity Portfolio - RPP
  w <- df[,2+i]
  rp <- Return.portfolio(R, 
                         weights = w,
                         rebalance_on = NA, 
                         geometric = TRUE, 
                         wealth.index = FALSE,
                         verbose = FALSE
  )
  mv$RPP <- rp
  
  ports_ret <- rbind(ports_ret, mv)
  
  print(i)
}

ports <- ports_ret

ports$MVP <- cumprod(1+ports_ret$MVP)
ports$RPP <- cumprod(1+ports_ret$RPP)

ports1 <- ports[index(ports)<="2019-12-30"]

ports2 <- ports[index(ports)>="2019-12-30"]
ports2$MVP <- ports2$MVP/ports2$MVP[[1]]
ports2$RPP <- ports2$RPP/ports2$RPP[[1]]

# Plot 
ggplot(ports1, aes(x=Index)) +
  geom_line(aes(y=RPP, color = "RPP"), size=1)+
  geom_line(aes(y=MVP, color = "MVP"), size=1)

head(ports)
tail(ports)
table.AnnualizedReturns(Return.calculate(ports))
table.AnnualizedReturns(Return.calculate(ports1))
table.AnnualizedReturns(Return.calculate(ports2))
# table.CalendarReturns((RPP$MVP))
# table.CalendarReturns((RPP$RPP))
# Salvando os dados
save(ports, file = "data/ports.rda")
save(ports1, file = "data/ports1.rda")
save(ports2, file = "data/ports2.rda")



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
  pesos <- dt[,2+i] # MVP
  
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
  
  dt[,ncol(dt)+1] <- dp[,2]/sum(dp[,2])
  
  if (i < 10){
    colnames(dt)[ncol(dt)] <- paste0("MVP0",i)
  }
  else{
    colnames(dt)[ncol(dt)] <- paste0("MVP",i)
  }
  print(i)
}

# Salvando os dados
save(df, file = "data/RiskContributionsRPP.rda")
save(dt, file = "data/RiskContributionsMVP.rda")

