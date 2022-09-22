rm(list = ls())

library(ggplot2)
library(tikzDevice)
library(hrbrthemes)
library(paletteer)
library(PortfolioAnalytics)

myColors <- RColorBrewer::brewer.pal(10,"Spectral") 

load("data/ports.rda")
load("data/ports1.rda")
load("data/ports2.rda")

name <- "Low10"

# ----------  gráfico das cotações --------------------------------------
gdf <- ports
pgdf <- ggplot(gdf, aes(x=Index)) +
  geom_line(aes(y=RPP,  color = "RPP"), size=1.5)+
  geom_line(aes(y=MVP,  color = "MVP"), size=1.5)+
#  geom_line(aes(y=IBOV, color = "IBOV"), size=1.5)+
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
tikz(paste0("../tikz/Rgraphics/retorno", name, ".tex"),
     standAlone = TRUE, 
     bg = "transparent",
     width = 6,
     height = 3)

pgdf
dev.off()

# ----------  gráfico das cotações 1 --------------------------------------
name <- paste0(name, 1)
gdf <- ports1

pgdf <- ggplot(gdf, aes(x=Index)) +
  geom_line(aes(y=RPP, color = "RPP"), size=1.5)+
  geom_line(aes(y=MVP, color = "MVP"), size=1.5)+
  #  geom_line(aes(y=IBOV, color = "IBOV"), size=1.5)+
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
tikz(paste0("../tikz/Rgraphics/retorno", name, ".tex"),
     standAlone = TRUE, 
     bg = "transparent",
     width = 6,
     height = 3)

pgdf
dev.off()

# ----------  gráfico das cotações 2 --------------------------------------
name <- paste0(gsub(".{1}$", "", name), 2)
gdf <- ports2

pgdf <- ggplot(gdf, aes(x=Index)) +
  geom_line(aes(y=RPP, color = "RPP"), size=1.5)+
  geom_line(aes(y=MVP, color = "MVP"), size=1.5)+
  #  geom_line(aes(y=IBOV, color = "IBOV"), size=1.5)+
  
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
tikz(paste0("../tikz/Rgraphics/retorno", name, ".tex"),
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
name <- gsub(".{1}$", "", name)

#---------------------------------------------------------------------------
name.port <- "MVP"
dq <- dt

dp <- dq[,c(1,3:68)]
dr <- dq[,c(1,70:(ncol(dq)-1))]

datas <- format(seq(as.Date("2017-01-01"), as.Date("2022-06-01"), by="months"), 
                format="%b%y")

colnames(dp) <- c("Stocks", datas)
colnames(dr) <- c("Stocks", datas)

#---------------------------------------------------------------------------
gdf <- reshape2::melt(dp,  id.vars = 'Stocks', variable.name = 'Data')

gdf <- gdf %>%
  arrange(Stocks)

#gdf$Data <- as.Date(gdf$Data)

pgdf <- ggplot(gdf[order(gdf$Stocks, decreasing = F),], 
               aes(fill=Stocks, y=value, x=Data)) + 
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
tikz(paste0("../tikz/Rgraphics/Weigth", name.port, name, ".tex"),
     standAlone = TRUE, 
     bg = "transparent",
     width = 6,
     height = 3)

pgdf
dev.off()

#---------------------------------------------------------------------------
gdf <- reshape2::melt(dr,  id.vars = 'Stocks', variable.name = 'Data')

gdf <- gdf %>%
  arrange(Stocks)

#gdf$Data <- as.Date(gdf$Data)

pgdf <- ggplot(gdf[order(gdf$Stocks, decreasing = F),], 
               aes(fill=Stocks, y=value, x=Data)) + 
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
tikz(paste0("../tikz/Rgraphics/Risk", name.port, name, ".tex"),
     standAlone = TRUE, 
     bg = "transparent",
     width = 6,
     height = 3)

pgdf
dev.off()

#---------------------------------------------------------------------------
name.port <- "RPP"
dq <- df

dp <- dq[,c(1,3:68)]
dr <- dq[,c(1,70:(ncol(dq)-1))]

datas <- format(seq(as.Date("2017-01-01"), as.Date("2022-06-01"), by="months"), 
                format="%b%y")

colnames(dp) <- c("Stocks", datas)
colnames(dr) <- c("Stocks", datas)

#---------------------------------------------------------------------------
gdf <- reshape2::melt(dp,  id.vars = 'Stocks', variable.name = 'Data')

gdf <- gdf %>%
  arrange(Stocks)

#gdf$Data <- as.Date(gdf$Data)

pgdf <- ggplot(gdf[order(gdf$Stocks, decreasing = F),], 
               aes(fill=Stocks, y=value, x=Data)) + 
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
tikz(paste0("../tikz/Rgraphics/Weigth", name.port, name, ".tex"),
     standAlone = TRUE, 
     bg = "transparent",
     width = 6,
     height = 3)

pgdf
dev.off()

#---------------------------------------------------------------------------
gdf <- reshape2::melt(dr,  id.vars = 'Stocks', variable.name = 'Data')

gdf <- gdf %>%
  arrange(Stocks)

#gdf$Data <- as.Date(gdf$Data)

pgdf <- ggplot(gdf[order(gdf$Stocks, decreasing = F),], 
               aes(fill=Stocks, y=value, x=Data)) + 
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
tikz(paste0("../tikz/Rgraphics/Risk", name.port, name, ".tex"),
     standAlone = TRUE, 
     bg = "transparent",
     width = 6,
     height = 3)

pgdf
dev.off()
