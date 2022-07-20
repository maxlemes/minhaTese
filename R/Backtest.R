df <- df%>%arrange(yahoo)
ibov_symbols <- as.character(c(df$yahoo))


ibov <- stockDataDownload(stock_symbols = ibov_symbols, 
                           from = "2021-12-31", to = "2022-06-30")
# resample 10 times from SP500, each with 50 stocks and 2-year consecutive data 
my_dataset_list <- financialDataResample(ibov, 
                                         N_sample = 50, T_sample = 252*2, 
                                         num_datasets = 10)

dataset <- my_dataset_list
# define GMVP (with heuristic not to allow shorting)
my_portfolio_fun <- function(dataset, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  Sigma <- cov(X)  # compute SCM
  # design GMVP
  w <- solve(Sigma, rep(1, nrow(Sigma)))
  w <- abs(w)/sum(abs(w))
  return(w)
}

df <- df%>%arrange(yahoo)

ibov_weights <- function(dataset, ...) {
  X <- diff(log(dataset$adjusted))[-1]  # compute log returns
  N <- ncol(X)
  w <- df$Fatia[1:N]
  return(w)
}

bt <- portfolioBacktest(list("Test"= my_portfolio_fun,
                             "IBOV"= ibov_weights),
                        my_dataset_list)

res_sum <- backtestSummary(bt)
names(res_sum)

summaryTable(res_sum, type = "DT", order_col = "Sharpe ratio", order_dir = "desc")

backtestSelector(bt, portfolio_name = c("Test", "IBOV"), 
                 measures = c("Sharpe ratio", "max drawdown"))

backtestTable(bt, measures = c("Sharpe ratio", "max drawdown"))


summaryBarPlot(res_sum, measures = c("Sharpe ratio", "max drawdown"))

backtestChartCumReturn(bt, value=names(bt))

backtestSummary(bt)$performance

backtestChartCumReturn(bt)

backtestChartDrawdown(bt)


library("PerformanceAnalytics")


w <- df$PesoJan
R <- ibov_ret
#R[is.na(R)] <- 0
R <- R[index(R) > "2021-12-31"]
R <- R[index(R) < "2022-05-01"]

#benchmark
bt <- Return.portfolio(R, weights = w, wealth.index = T)
bt <- bt/bt[[1]]
colnames(bt) <- "IBOV"


# Risk Parity Portfolio
w <- df$Pesos21
rt <- Return.portfolio(R, weights = w, rebalance_on="months",  wealth.index = T)
rt <- rt/rt[[1]]

bt$RPP <- rt
plot(bt, line)

chart.CumReturns(rt)
chart.StackedBar(rt$BOP.Weight)
chart.StackedBar(rt$BOP.Value)

plot(bt, line)

myPricesClose <- R
myPosition <- as.numeric(w/zoo::coredata(R[1,]))
myPosition[myPosition == "Inf"] <- 0

myPortfolio <- zoo::coredata(myPricesClose)*myPosition
myPortfolio$total <- rowSums(myPortfolio,na.rm=TRUE)
myPortfolio <- myPortfolio[myPortfolio$total != 0,]

rt <- rt/rt[[1]]

w <- df$PesoMai

bt<- Return.portfolio(R, weights = w, wealth.index = T,
                      contribution = F, geometric = F)


bt <- bt/bt[[1]]



ib <- RaIBOV[,c(2,8)]
ib$date <- as.POSIXct.Date(ib$date)

ib <- as.xts(ib, index(date))

rtt <- xts(order.by = ib$date)
rtt$ibov <- ib$adjusted


ib <- merge(rt, bt, join = 'left')
ib$ibov <- ib$ibov/ib$ibov[[1]]

bt <- R[,1]
bt[,1] <- myPortfolio$total

plot(rt, line)
