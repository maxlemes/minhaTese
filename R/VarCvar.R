library(dplyr)
library(ggplot2)
library(tikzDevice)
minhasCores <- RColorBrewer::brewer.pal(8, "Blues") 

blank_theme <- theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    axis.text.x=element_blank(),
    panel.grid=element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(size=14, face="bold")
  )

p <- rchisq(10000, df =  4, ncp=2)
q <- rlnorm(10000, meanlog = 0, sdlog = 0.6)
r <- p/q+10*q
hist(r,breaks=200)

hist(r,
     freq = TRUE, #FALSE = Density
  #   labels = TRUE,
     breaks = 200, #número de células do histograma
     xlim = c(0,80),
     ylim = c(0,400), #pode ser ignorado
     main = '',
     xlab = "Portfolio loss",
     ylab = "Frequency",
     col = minhasCores[4],
     cex.lab = 1,
     cex.axis = 1,
     cex.main = 2,
     cex.sub = 1)

# Barplot
df %>% ggplot(., aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")

tikz("../tikz/pieplot.tex",standAlone = TRUE, width = 5,height = 3)
# pieplot
df %>% ggplot(., aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0) +
  scale_fill_brewer(palette="Blues")+
  blank_theme +
  #geom_text(aes(y = value/3 + c(0, cumsum(value)[-length(value)]), 
  #              label = scales::percent(value/100)), size=5) +
  labs(fill = "Grau de Instrução")

dev.off()
# Compile the tex file
tools::texi2dvi('../tikz/pieplot.tex',pdf=T)
# move o arquivo para a figs
#file.rename("pieplot.pdf", "../figs/pieplot.pdf")
# optionally view it:
# system(paste(getOption('pdfviewer'),'symbol-regression.pdf'))



tikz("../tikz/scatter.tex",standAlone = TRUE, width = 5,height = 3)
plot(milsa[,c(6,5)],
     xlim = c(15,50),
     ylim = c(4,10), #pode ser ignorado
     xlab = "Idade",
     ylab = "Salário",
     cex = 3,
     pch = 20,
     col = minhasCores,
     cex.lab = 1,
     cex.axis = 1,
     cex.main = 2,
     cex.sub = 1)
dev.off()

# Compile the tex file
tools::texi2dvi('../tikz/scatter.tex',pdf=T)
# move o arquivo para a figs
#file.rename("scatter.pdf", "../figs/scatter.pdf")
# optionally view it:
# system(paste(getOption('pdfviewer'),'symbol-regression.pdf'))


tikz("../tikz/histograma.tex",standAlone = TRUE, width = 5,height = 3)
hist(milsa$Salário,
     freq = TRUE, #FALSE = Density
     labels = TRUE,
     breaks = 10, #número de células do histograma
     xlim = c(4,25),
     ylim = c(0,10), #pode ser ignorado
     main = '',
     xlab = "Salários",
     ylab = "Frequência",
     col = minhasCores[4],
     cex.lab = 1,
     cex.axis = 1,
     cex.main = 2,
     cex.sub = 1)
dev.off()

# Compile the tex file
tools::texi2dvi('../tikz/histograma.tex',pdf=T)

tikz("../tikz/boxplot.tex",standAlone = TRUE, width = 5,height = 3)
boxplot(milsa$Salário ~ milsa$Inst,
 #       main = 'BoxPlot do Salários',
        xlab = "Grau de Instrução",
        ylab = "Salários",
        col = minhasCores,
        cex.lab = 1,
        cex.axis = 1,
        cex.main = 2,
        cex.sub = 1)
dev.off()

# Compile the tex file
tools::texi2dvi('../tikz/boxplot.tex',pdf=T)

ex <- c(8, 11, 8,6, 10, 14, 10, 16, 10, 7, 10, 14, 14, 8, 14, 14, 15, 8, 8, 7, 12, 11, 9, 7, 15, 12, 14, 13, 19, 6, 12, 12, 12, 8, 11, 14, 7, 5, 11, 6, 9, 12, 8, 12, 10, 12, 22, 5, 12, 7)
df <- tibble::as_tibble(table(ex))
colnames(df) <- c("Nro de Erros", "Frequência")

tikz("../tikz/scatter2.tex",standAlone = TRUE, width = 5,height = 3)
plot(x=df$`Nro de Erros`, y= df$Frequência,
     #main = "Erros de impressão",
     xlab = "Número de Erros",
     ylab = "Freqência",
     col = "darkblue",
     cex = 2, 
     pch = 20,
     cex.lab = 1,
     cex.axis = 1,
     cex.main = 2,
     cex.sub = 1)
dev.off()

# Compile the tex file
tools::texi2dvi('../tikz/scatter2.tex',pdf=T)

tikz("../tikz/histograma2.tex",standAlone = TRUE, width = 5,height = 3)
hist(ex,
     freq = FALSE, #FALSE = Density
     # labels = TRUE, # números em cima das barras
     labels = paste0(round(hist(ex, plot = FALSE)$counts / length(ex) * 100, 1), "%"),
     breaks = 10, #número de células do histograma
     xlim = c(4,25),
     ylim = c(0,0.15), #pode ser ignorado
     main = '',
     xlab = "Número de Erros",
     ylab = "Proporção",
     col = minhasCores[1],
     cex.lab = 1,
     cex.axis = 1,
     cex.main = 2,
     cex.sub = 1)
dev.off()

# Compile the tex file
tools::texi2dvi('../tikz/histograma2.tex',pdf=T)
system("rm *.aux; rm *.log")

