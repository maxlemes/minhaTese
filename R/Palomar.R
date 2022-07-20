rm(list = ls())

library(xts)
library(portfolioBacktest)
library(riskParityPortfolio)
library(quadprog)

# download price data
faang_data <- stockDataDownload(c("ABEV3.SA", "B3SA3.SA", "BBAS3.SA", "BBDC4.SA", "CIEL3.SA"),
                                from = "2016-01-01", to = "2020-06-25")

faang_data$open <- na.omit(faang_data$open)
faang_data$adjusted <- na.omit(faang_data$adjusted)

# define portfolios to be backtested
# risk parity portfolio
risk_parity <- function(dataset) {
  prices <- dataset$adjusted
  log_returns <- diff(log(prices))[-1]
  return(riskParityPortfolio(cov(log_returns))$w)
}

# tangency portfolio (maximum sharpe ratio)
max_sharpe_ratio <- function(dataset) {
  prices <- dataset$adjusted
  log_returns <- diff(log(prices))[-1]
  N <- ncol(prices)
  Sigma <- cov(log_returns)
  mu <- colMeans(log_returns)
  if (all(mu <= 1e-8))
    return(rep(0, N))
  Dmat <- 2 * Sigma
  Amat <- diag(N)
  Amat <- cbind(mu, Amat)
  bvec <- c(1, rep(0, N))
  dvec <- rep(0, N)
  res <- solve.QP(Dmat = Dmat, dvec = dvec, Amat = Amat, bvec = bvec, meq = 1)
  w <- res$solution
  return(w/sum(w))
}

# call portfolioBacktest and benchmark against the uniform (1/N) portfolio
bt <- portfolioBacktest(list("risk parity portfolio" = risk_parity,
                             "tangency portfolio"    = max_sharpe_ratio),
                        list(faang_data),
                        lookback = 12*20, 
                        optimize_every = 3*20, 
                        rebalance_every = 3*20)

# dates of the designed portfolios
index(bt$tangency$data1$w_designed)
#>  [1] "2014-12-12" "2015-03-12" "2015-06-08" "2015-09-01" "2015-11-25" "2016-02-24" "2016-05-19" "2016-08-15"
#>  [9] "2016-11-08" "2017-02-06" "2017-05-03" "2017-07-28" "2017-10-23" "2018-01-19" "2018-04-17" "2018-07-12"
#> [17] "2018-10-05" "2019-01-03" "2019-04-01"

# check performance summary
backtestSummary(bt)$performance
#>                   risk parity portfolio tangency portfolio
#> Sharpe ratio                  1.3800144          0.8787596
#> max drawdown                  0.3062046          0.3516856
#> annual return                 0.3117200          0.2324203
#> annual volatility             0.2258817          0.2644868
#> Sterling ratio                1.0180122          0.6608751
#> Omega ratio                   1.2710283          1.1793760
#> ROT (bps)                  8310.1199557        793.0188434

# plot cumulative returns chart
backtestChartCumReturns(bt)
