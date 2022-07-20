b3_ibra <- function(){
  
  file <- list.files(path="data-raw/", pattern=".csv")
  
  df <- read.table(paste0("data-raw/",file), sep = ";", dec = ",", header = FALSE, fill = TRUE)
  
  file.copy(paste0("data-raw/",file), paste0("data-raw/Olds/",file))
  
  df <- df[3:(nrow(df)-2),c(1,2,4,5)]
  
  colnames(df) <- c("Ticker", "Nome", "Qtde", "Fatia")
  
  df$Fatia <- as.numeric(sub(",", ".", df$Fatia, fixed = TRUE))
  
  df$yahoo <- paste0(df$Ticker, ".SA")
  
  df$Fatia <- df$Fatia/100
  
  df[nrow(df),"Fatia"] <- 1-sum(df[1:(nrow(df)-1),"Fatia"])
  
  #-------- Atualizando os Segmentos ----------------
  segmentos <- seg()
  
  #-------- Mesclando os dados ----------------------
  df <- dplyr::left_join(df, segmentos[,2:5], by = ("Nome" = "Nome"))
  
  df <- df[, c(1,2,6:8,5)]
  
  ibra <- df
  save(ibra, file = paste0("data/ibra", format(as.Date(Sys.Date()), "-%m-%Y"), ".rda"))
  
  return(df)
}

#--------
seg <- function(){
  urlsegmentos <- "http://www.b3.com.br/lumis/portal/file/fileDownload.jsp?fileId=8AA8D0975A2D7918015A3C81693D4CA4"
  
  download.file(urlsegmentos, "data-raw/ClassifSetorial.zip")
  unzip("data-raw/ClassifSetorial.zip",exdir="data-raw/")
  file <- list.files(path="data-raw/", pattern=".xlsx")
  file.rename(paste0("data-raw/",file), "data-raw/ClassifSetorial.xlsx")
  
  df <- readxl::read_excel("data-raw/ClassifSetorial.xlsx")
  file.remove("data-raw/ClassifSetorial.xlsx", "data-raw/ClassifSetorial.zip")
  
  colnames(df) <- c("Setor", "Subsetor", "Nome", "Ticker", "Segmento")
  
  df$Segmento <- as.character(NA)
  
  for (i in which(is.na(df$Ticker))){
    df[i,5] <-  df[i,3]
    df[i,3] <- "Nome"
    df[i,4] <- "Ticker"
  }
  
  df <- df[,c(4,3,1,2,5)]
  
  df <- zoo::na.locf(df)
  
  df <- df %>%
    dplyr::filter(Ticker != "LISTAGEM",
                  Ticker !=  "CÓDIGO",
                  Ticker !=  "Ticker")
  
  return(df)
}

#Criando o tibble com o percentual dos retornos
pos_neg <- function(df){
  t_1 <- nrow(df) - 126
  t_2 <- nrow(df)
  # Separando os retornos positivos (1) e negativos (0)
  df <- (1 + sign(df))/2 
  df <- replace(df, list = is.na(df), values = 0)
  # calculando o percentual de dias positivos
  df[nrow(df),] <- raster::colSums(df)/nrow(df) 
  # separando a linha com o o percentual de dias positivos
  df <- df[nrow(df),]  
  # transfomando em um tibble
  df <- tidyr::gather(dplyr::as_tibble(df), yahoo, pos) 
  return(df)
}

mome <- function(df) {
  df <- df %>%
    dplyr::arrange(-mom)%>%
    .[1:60,] %>%
    dplyr::arrange(-pos) %>%
    .[1:30,c(1,2,5,7:12, 15, 28)] %>%
    dplyr::arrange(.[,6]) %>%
    na.omit()
}
