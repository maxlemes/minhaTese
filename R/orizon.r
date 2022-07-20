rm(list = ls())

library(tidyverse)
library(lubridate)
library(googledrive)
library(googlesheets4)


#-------------------- AUTENTICAR no GOOGLE DRIVE 
#logar na conta do GDrive
drive_auth(email = "maxvlemes@gmail.com",
  scopes = c(
  'https://www.googleapis.com/auth/drive',
  'https://www.googleapis.com/auth/spreadsheets'))

gs4_auth(token = drive_token())

#-----------------------------------------------
# buscando a planilha
drive_find("ibra",type = "spreadsheet")

#baixando uma planilha
ss4 <- drive_get("ibra")

#criar uma sheet
ss4 <- googlesheets4::gs4_create(
  "Orizon",
  sheets = "Cotações"
)

#ler de uma sheet
#read_sheet(id_ibra[[2]])

#----------------- SELECIONAR AS ACOES DO IBRA 

file <- list.files(path="data-raw/", pattern=".csv")

file <- file[2]

df <- read.table(paste0("data-raw/",file), sep = ";", dec = ",", header = FALSE, fill = TRUE)

file.copy(paste0("data-raw/",file), paste0("data-raw/Olds/",file))

df <- df[3:(nrow(df)-2),c(1,2,4,5)]


colnames(df) <- c("Ticker", "Nome", "Qtde", "Fatia")

df$Fatia <- as.numeric(sub(",", ".", df$Fatia, fixed = TRUE))

df$yahoo <- paste0(df$Ticker, ".SA")

df$Fatia <- df$Fatia/100

df[nrow(df),"Fatia"] <- 1-sum(df[1:(nrow(df)-1),"Fatia"])

df <- df %>%
  arrange(Ticker)

#-------------------- ORGANIZAR A PLANILHA COTACOES

dff <- LETTERS[seq(1,26,1)]

for (i in 1:26){
  for (j in 1:26){
    k <- 26*i+j
    dff[k] <- paste0(LETTERS[i],LETTERS[j])
  } 
}


orizon <- matrix(nrow = 2, ncol = 2*length(df))%>%
  as_tibble()

df <- c("BBAS3","ABEV3","EGIE3", "GGBR4", 'ITUB4', 'BBDC4', "KLBN4", "EZTC3", "FLRY3", "BBSE3", "TAEE11", "IVVB11", "BOVA11")


for (i in seq(1,length(df),2)){
  orizon[1,i+1] <- df[i]
  orizon[2,i] <- paste0('=GOOGLEFINANCE(',df[i+1],"2",';"price";DATEVALUE(TODAY()-183);TODAY();1)')
}

sink("gs4Formula.R")
for (i in seq(1,length(df),2)){
cat(
  paste0('orizon$V',i,' <- gs4_formula(orizon$V',i,')')
)
  cat("\n")
}
sink()

source("gs4Formula.R")


#escrever em uma sheet (SS)
sheet_write(orizon, ss=ss4$id, sheet = "Cotacoes")


# ------------------- ORGANIZAR A PLANILHA PRECOS ----

ibra2 <- matrix(nrow = 300 , ncol = 1+length(df$Ticker))%>%
  as_tibble()

for(i in 2:nrow(ibra2)){
  ibra2[i,1] <- paste0("='Cotacoes'!A",i)
  for (j in seq(1,length(df$Ticker),1)){
    ibra2[i,j+1] <- paste0("='Cotacoes'!",dff[2*j],i)
  }
}

sink("data/gs4Formula2.R")
for (i in seq(1,1+length(df$Ticker),1)){
  cat(
    paste0('ibra2$V',i,' <- gs4_formula(ibra2$V',i,')')
  )
  cat("\n")
}
sink()

source("data/gs4Formula2.R")

#escrever em uma sheet (SS)
sheet_write(orizon, ss=ss4, sheet = "Cotações")
