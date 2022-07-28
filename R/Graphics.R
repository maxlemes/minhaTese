rm(list = ls())

library(ggplot2)
library(tikzDevice)
library(hrbrthemes)
library(paletteer)
library(PortfolioAnalytics)

myColors <- RColorBrewer::brewer.pal(10,"Spectral") 

load("data/RPP2.rda")

gdf <- RPP2
#plot(gdf, bt, line)

# ----------  gráfico das cotações --------------------------------------
pgdf <- ggplot(gdf, aes(x=Index)) +
  geom_line(aes(y=RPP, color = "RPP"), size=1.5)+
  geom_line(aes(y=MVP, color = "MVP"), size=1.5)+
  xlab("")+
  ylab("")+
 # ylim(-0.1,0.2)+
  paletteer::scale_colour_paletteer_d("nbapalettes::kings_city", direction=-1) +
  #scale_colour_brewer(palette="Dark2", direction = 1)+
  theme_minimal()+
  labs(title = "",
       colour = "")+
  #scale_y_continuous(breaks=c(0,1,2), labels = c("\u2113", "\u2113", "\u2113"))+
  #scale_y_continuous(labels = scales::percent_format())+
 # scale_x_date(date_breaks = "1 day", date_labels = "%b")+
  theme(legend.position="bottom")

pgdf

# cria o tikz do gráfico das cotações
tikz("../tikz/retorno/retornoRPPMVP2.tex",
     standAlone = TRUE, 
     bg = "transparent",
     width = 6,
     height = 3)

pgdf
dev.off()

#--------------------------------------------------------------------------
# ----------  gráfico do Risco Total  -------------------------------------

load("data/RiskContributionsRPP.rda")
load("data/RiskContributionsMVP.rda")
df <- dg

dp <- df[,c(2:68)]
dr <- df[,c(2,70:ncol(df))]

datas <- format(seq(as.Date("2017-01-01"), as.Date("2022-06-01"), by="months"), format="%b%y")

colnames(dp) <- c("Stocks", datas)
colnames(dr) <- c("Stocks", datas)

dt <- dr

gdf <- reshape2::melt(dt,  id.vars = 'Stocks', variable.name = 'Data')

gdf <- gdf %>%
  arrange(Stocks)

#gdf$Data <- as.Date(gdf$Data)

pgdf <- ggplot(gdf[order(gdf$Stocks, decreasing = F),], aes(fill=Stocks, y=value, x=Data)) + 
  geom_bar(position="stack", stat="identity") +
  #scale_fill_viridis(discrete = T) +
  theme_minimal() +
  scale_fill_brewer(palette="RdBu", name=NULL, direction = -1)+
  ylab("")+
  xlab("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 2, vjust = 0.5, size = 5))+
  theme(legend.position="bottom", )

pgdf

# cria o tikz do gráfico das cotações
tikz("../tikz/total/totalRiskRPP.tex",
     standAlone = TRUE, 
     bg = "transparent",
     width = 6,
     height = 3)

pgdf
dev.off()

#--------------------------------------------------------------------------
# ----------  gráfico da Contribuiçao de Risco ----------------------------
load("data/RiskContributions.rda")

df <- df[,c(1,seq(from = 70,to = 201, by = 2))]

datas <- format(seq(as.Date("2017-01-01"), as.Date("2022-06-30"), by="months"), format="%Y-%m-%d")
colnames(df) <- c("Stocks", datas)

df[,1] <- lista <- c("ABEV3", "BBDC4", "ITUB4", "PETR4","VALE3")

i <- 7
  gdf <- df[,c(1,i)]
  colnames(gdf)[2] <- c("value")
  
  pgdf <- ggplot(gdf, aes(x=Stocks, y=value)) + 
    geom_bar(position="stack", stat="identity", fill=myColors[i])+ #fill = "#3288BD") +
    #scale_fill_viridis(discrete = T) +
    theme_ipsum() +
    #scale_fill_brewer(palette="Blues")+
    ylab("")+
    xlab("")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  pgdf


  
  # cria o tikz do gráfico das cotações
  tikz(paste0("../tikz/RiskContrib10",i-1,".tex"),
       standAlone = TRUE, 
       bg = "transparent",
       width = 5,
       height = 3)
  
  pgdf
  dev.off()


  #--------------------------------------------------------------------------
  # ----------  gráfico da Contribuiçao de Risco ----------------------------
  
  colnames(df)[1] <- c("Stocks")
  
  df[,1] <- c("Stock01", "Stock02", "Stock03", "Stock04","Stock05", "Stock06", "Stock07", "Stock08", "Stock09", "Stock10")
  
  #gdf <- reshape2::melt(df[,c(1,9)],  id.vars = 'Stocks', variable.name = 'mes')
  
  rdf <- df[,c(1,seq(from = 10,to = 20, by = 2))]
  
  i =  7 # do 2 ao 7
  gdf <- rdf[,c(1,i)]
  colnames(gdf)[2] <- c("value")
  
  pgdf <- ggplot(gdf, aes(x=Stocks, y=value)) + 
    geom_bar(position="stack", stat="identity", fill = "#3288BD") +
    #scale_fill_viridis(discrete = T) +
    theme_ipsum() +
    scale_fill_brewer(palette="Blues")+
    ylab("")+
    xlab("")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
  
  pgdf
  
  # cria o tikz do gráfico das cotações
  tikz(paste0("../tikz/RiskContribEqual10",i-1,".tex"),
       standAlone = TRUE, 
       bg = "transparent",
       width = 5,
       height = 3)
  
  pgdf
  dev.off()
  

