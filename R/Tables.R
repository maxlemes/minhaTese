rm(list = ls())

# Libraries
library(xtable)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(tikzDevice)
library(knitr)
library(kableExtra)

load("data/RPP.rda")
load("data/RPP1.rda")
load("data/RPP2.rda")

df <- table.AnnualizedReturns(Return.calculate(RPP))
df <- table.AnnualizedReturns(Return.calculate(RPP1))
df <- table.AnnualizedReturns(Return.calculate(RPP2))

df <- tibble(
  " " = c("Annualized Return", "Annualized Std Dev", "Annualized Sharpe (Rf=0\\%)"),
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


sink("../tables/RPP2.tex") # LEMBRE-SE DE MUDAR O NOME do ARQUIVO
cat(c("\\begin{table}
   % \\centering
      \\begingroup
      \\fontsize{9}{9}
      \\selectfont",
      tdf,
      "\\caption{Annualized Returns: Jan20 - Jun22}
      \\label{tab:RPP2}  % LEMBRE-SE DE MUDAR O LABEL
      \\endgroup{}
      \\end{table}"))
sink()
