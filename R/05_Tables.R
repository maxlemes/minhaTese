rm(list = ls())

# Libraries
library(xtable)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tikzDevice)
library(knitr)
library(kableExtra)

load("data/ports.rda")
load("data/ports1.rda")
load("data/ports2.rda")

name <- "Low10"

# Tabela 1
df <- table.AnnualizedReturns(Return.calculate(ports))

df <- tibble(
  " " = c("Annualized Return", 
          "Annualized Std Dev", 
          "Annualized Sharpe (Rf=0\\%)"),
  "MVP" = df$MVP,
  "RPP" = df$RPP
)

tdf <- df %>% kable(format = "latex", 
                     booktabs = TRUE,
                     escape = F, 
                     linesep = "",
                #     format.args = list(big.mark = ".", decimal.mark =","),
                     align = c("l","r","r"))%>%
  column_spec(c(1), bold = T) #%>%
#  column_spec(3, border_right = T)

sink(paste0("../tables/", name, ".tex")) # LEMBRE-SE DE MUDAR O NOME do ARQUIVO
cat(c("\\begin{table}
      \\centering
      \\begingroup
      \\fontsize{9}{9}
      \\selectfont",
      tdf,
      "\\caption{Annualized Returns: Jan17 - Jun22}
      \\label{tab:",name,"}  
      \\endgroup{}
      \\end{table}"))
sink()

# Tabela 1
df <- table.AnnualizedReturns(Return.calculate(ports1))
name <- paste0(name, 1)

df <- tibble(
  " " = c("Annualized Return", 
          "Annualized Std Dev", 
          "Annualized Sharpe (Rf=0\\%)"),
  "MVP" = df$MVP,
  "RPP" = df$RPP
)

tdf <- df %>% kable(format = "latex", 
                    booktabs = TRUE,
                    escape = F, 
                    linesep = "",
                    #     format.args = list(big.mark = ".", decimal.mark =","),
                    align = c("l","r","r"))%>%
  column_spec(c(1), bold = T) #%>%
#  column_spec(3, border_right = T)

sink(paste0("../tables/", name, ".tex")) # LEMBRE-SE DE MUDAR O NOME do ARQUIVO
cat(c("\\begin{table}
      \\centering
      \\begingroup
      \\fontsize{9}{9}
      \\selectfont",
      tdf,
      "\\caption{Annualized Returns: Jan17 - Dez19}
      \\label{tab:",name,"}  
      \\endgroup{}
      \\end{table}"))
sink()

# Tabela 1
df <- table.AnnualizedReturns(Return.calculate(ports2))
name <- paste0(gsub(".{1}$", "", name), 2)

df <- tibble(
  " " = c("Annualized Return", 
          "Annualized Std Dev", 
          "Annualized Sharpe (Rf=0\\%)"),
  "MVP" = df$MVP,
  "RPP" = df$RPP
)

tdf <- df %>% kable(format = "latex", 
                    booktabs = TRUE,
                    escape = F, 
                    linesep = "",
                    #     format.args = list(big.mark = ".", decimal.mark =","),
                    align = c("l","r","r"))%>%
  column_spec(c(1), bold = T) #%>%
#  column_spec(3, border_right = T)

sink(paste0("../tables/", name, ".tex")) # LEMBRE-SE DE MUDAR O NOME do ARQUIVO
cat(c("\\begin{table}
      \\centering
      \\begingroup
      \\fontsize{9}{9}
      \\selectfont",
      tdf,
      "\\caption{Annualized Returns: Jan20 - Jun22}
      \\label{tab:",name,"}  
      \\endgroup{}
      \\end{table}"))
sink()
