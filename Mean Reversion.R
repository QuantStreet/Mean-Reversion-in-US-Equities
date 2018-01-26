# Author: Danny Ferdman
# Date: 01/25/2018
# Project Name: Short Term Mean Reversion in US Equities

library(data.table)

# R version and package version checker:
if (sessionInfo()$R.version$version.string != "R version 3.3.2 (2016-10-31)") {
  warning('Built in R Version 3.3.2')
}
if (packageVersion('data.table') != "1.10.4.3"){
  warning("Built Using data.table Package Version 1.10.4.3")
}
if (packageVersion('zoo') != "1.7.14"){
  warning("Built Using zoo Package Version 1.7.14")
}

startTime <- Sys.time()

stocks <- readRDS('data.RDS')

stocks <- stocks[SHRCD %in% c(10,11),] # Keeps only common shares
stocks <- stocks[EXCHCD %in% c(1,2,3),] # Keeps only NYSE, NASDAQ and NYSE
setorder(stocks, PERMNO, date)

stocks[, c('RET', 'DLRET', 'vwretd') := .(as.numeric(RET),
                              as.numeric(DLRET),
                              as.numeric(vwretd))]

errorCodes <- c(-44, -55, -66,-77,-88,-99) # Error codes from stocks
stocks <- stocks[!(RET %in% errorCodes | DLRET %in% errorCodes)]

stocks[, date := as.Date(as.character(date), format = '%Y%m%d')]

# Notice that there is a need to identify entry and exit points 
#   for each week and they serve both for performance and sorting portfolios.

# Extract date index and market return data for creating non overlapping rank groupings----
stockMkt <- merge(data.table(date = seq(min(stocks$date), 
                                       max(stocks$date), 
                                       'day')), 
                 stocks[, .SD[1, .(vwretd)], .(date)],
                 by = 'date', 
                 all = T)

stockMkt[, YrWk := paste0(ifelse(strftime(date, "%V") == '01' & month(date) == 12, 
                                 year(date) + 1, 
                                 year(date)), 
                          '-', strftime(date, "%V"))] #Year-Week combo 

stockMkt[, wkday := strftime(date, '%A')] # Weekday
stockMkt[, num_wkday := as.numeric(strftime(date, '%u'))] # Numeric equivalent of weekday

entryDay <- 1 # Monday

stockMkt[num_wkday == entryDay, groupByWK := YrWk]
stockMkt[, groupByWK := zoo::na.locf(groupByWK, na.rm = F)]

stocks <- merge(x = stockMkt[, -'vwretd', with = F], 
              y = stocks, 
              by = 'date', 
              all.y = T, 
              all.x = F)

setorder(stocks, 'PERMNO', 'date')

# Market vol over the ranking week:
stockMkt[, groupByVol := sd(vwretd, na.rm = T), .(groupByWK)]

# Rolling vol observations for the market over 10 and 20 trading days----
stockMkt[!is.na(vwretd), c('rollVol_10D',  'rollVol_20D') := .(
  zoo::rollapply(data = vwretd, 
                 width = 10, 
                 FUN = sd,
                 fill = NA, 
                 align = 'right'), 
  zoo::rollapply(data = vwretd, 
                 width = 20, 
                 FUN = sd,
                 fill = NA, 
                 align = 'right')
  )]


# Additional stocks data cleaning----
stocks[!(is.na(RET) & is.na(DLRET)), totRet := { # geometric addition of returns
  subSD <- .SD[,.(RET, DLRET)]
  subSD[is.na(RET), RET := 0]
  subSD[is.na(DLRET), DLRET := 0]
  subSD[, (1 + RET) * (1 + DLRET) - 1]
}]

# Assume that returns either below 100% or above 300% are errors in the data
stocks <- stocks[!(totRet < -1 | totRet > 3)] 

stocks[, logTotRet := log(1 + totRet)] # log returns for faster calculations

# Aggregaing statistics for the database----
statStocks <- stocks[, .('ret_wk' = exp(sum(logTotRet, na.rm = T)) - 1), .(PERMNO, groupByWK)]

# Equities that lost over 85% in the ranking week are assumed to be too risky and are not ranked
statStocks[ret_wk > -0.85, sortDecile := cut(ret_wk, 
                             breaks = quantile(x = ret_wk, 
                                               probs = (0:20)/20, 
                                               include.lowest = T), 
                             include.lowest = T,
                             labels = 1:20), 
         .(groupByWK)]

statStocks[, sortDecile := as.numeric(sortDecile)] # I use 'decile' and 'quantile' interchangibly 
statStocks <- statStocks[, .SD[.N > 1] ,.(PERMNO)] # Drop PERMNOs with only week's worth of observations
# Creating effective decile column for easier debugging in portfolion creation stage
statStocks[, effDecile := shift(sortDecile),.(PERMNO)] 

# Holding period of 30 days post ranking:
setkey(stocks, date)
holdPeriodMat <- stockMkt[, .('Beg' = date[1], 
                              'End' = date[1] + 29),
                          .(groupByWK)]

statStocks_30D <- rbindlist(
  apply(holdPeriodMat, 
        MARGIN = 1, 
        FUN = function(x){
          # browser()
          stocks[date %inrange% as.Date(x[2:3]), .('log_ret_30D' = sum(logTotRet, na.rm = T), 
                                                   'Date' = as.character(x[1])),
                 .(PERMNO)]
        })
) #statStocks_30D has a few more observations than statStocks
#    because the single observations weren't dropped. Gets dropped
#    a few lines below in the merge.

statStocks_30D[, ret_30D := exp(log_ret_30D) -1]
setnames(statStocks_30D, 'Date', 'groupByWK')


statStocks <- merge(x = statStocks_30D[, -'log_ret_30D', with = F], 
                    y = statStocks, 
                    by = c('PERMNO', 'groupByWK'), 
                    all.x = F, 
                    all.y = T)


# Long Short portfolio over the subsequent week post ranking----
LS_Portfolios <- statStocks[!is.na(effDecile), .('LS_Ret' = {
  mean(.SD[effDecile == 1, ret_wk]) - mean(.SD[effDecile == 20, ret_wk])}, 
  'LS_30D_Ret' = {
    mean(.SD[effDecile == 1, ret_30D]) - mean(.SD[effDecile == 20, ret_30D])
    }), 
  .(groupByWK)][order(groupByWK)]



# Add a lagged volatility measure for analysis:
LS_Portfolios[, c('lag_mkt_Vol', 
                  'lag_mkt_roll10D_Vol', 
                  'lag_mkt_roll30D_Vol') := 
                shift(stockMkt[!is.na(vwretd) & groupByWK != stockMkt$groupByWK[1], 
                              .SD[.N, .(groupByVol, 
                                        rollVol_10D, 
                                        rollVol_20D)], 
                              by = .(groupByWK)][, -'groupByWK', with = F])]

# Add grouped market returns:
LS_Portfolios <- merge(x = LS_Portfolios, 
                       y = stockMkt[, .(mktRet_wk = prod(1 + vwretd, na.rm = T) - 1), .(groupByWK)], 
                       by = 'groupByWK', 
                       all.x = T, 
                       all.y = F)

# Testing the effects of lagged vol on portfolio returns:
LS_Portfolios[, {subSD <- copy(.SD)
  subSD[, 1] <- log(subSD[, 1])
  plot(subSD)
  abline(lm(subSD[, 2:1]), col = 'green')
  summary(lm(subSD[, 2:1]))}, .SDcols = c('lag_mkt_roll30D_Vol', 'LS_Ret')]

# Establish a position only when lagged vol is greater to or equal than the rolling median:
LS_Portfolios[lag_mkt_roll30D_Vol >= 
                sapply(seq_along(LS_Portfolios$lag_mkt_roll30D_Vol), 
                       function(x){LS_Portfolios[1:x, median(lag_mkt_roll30D_Vol, na.rm = T)]}), 
              volOverMedian := T]

    
# Results Visualization:
#   Market portfolio:
LS_Portfolios[, {
  portRet <- copy(.SD[, mktRet_wk])
  hist(portRet, 50, col = rgb(0,.2,1,0.5), main = 'Histogram of Market Returns')
  .('Mean Market Ret' = mean(portRet),
    'Strat SR(1WK)' = mean(portRet)/sd(portRet),
    'Strat SR(1YR)' = mean(portRet)/sd(portRet) * sqrt(52)
  )}]

#   Long-Short portfolio:
LS_Portfolios[, {
  portRet <- copy(.SD[, LS_Ret])
  hist(portRet, 50, col = rgb(.1,.8,.1,0.7), main = 'Histogram of Long-Short Returns')
  .('Mean Long-Short Ret' = mean(portRet),
    'Strat SR(1WK)' = mean(portRet)/sd(portRet),
    'Strat SR(1Yr)' = mean(portRet)/sd(portRet) * sqrt(52)
  )}]

#    Long-Short portfolio with vol overlay:
LS_Portfolios[, {
  portRet <- copy(.SD[volOverMedian == T, LS_Ret])
  hist(portRet, 50, col = rgb(.2,.6,.1,0.7), main = 'Histogram of Long-Short Returns\n w. Vol Overlay')
  .('Mean L/S w. vol overlay Ret' = mean(portRet),
    'Strat SR(1WK)' = mean(portRet)/sd(portRet), 
    'Portion of qualifying weeks' = length(portRet)/LS_Portfolios[, .N]
  )}]

#    Long-Short 30 Days portfolio:
LS_Portfolios[, {
  portRet <- copy(.SD[, LS_30D_Ret])
  hist(portRet, 50, col = rgb(.2,.6,.1,0.7), main = 'Histogram of Long-Short 30 Day Returns')
  .('Mean L/S 30D Ret' = mean(portRet),
    'Strat SR(30D)' = mean(portRet)/sd(portRet), 
    'Strat SR(1Yr)' = mean(portRet)/sd(portRet) * sqrt(12)
  )}]

#    Long-Short 30 Days portfolio w. vol overlay:
LS_Portfolios[, {
  portRet <- copy(.SD[volOverMedian == T, LS_30D_Ret])
  hist(portRet, 50, col = rgb(.2,.6,.1,0.7), main = 'Histogram of Long-Short 30 Day Returns\n w. Vol Overlay')
  .('Mean L/S 30D Ret' = mean(portRet),
    'Strat SR(30D)' = mean(portRet)/sd(portRet), 
    'Strat SR(1Yr)' = mean(portRet)/sd(portRet) * sqrt(12)
  )}]

strat_Running_Time <- Sys.time() - startTime
strat_Running_Time
