#Change the symbol in 4th line and run the whole program to see 5 day forcast of stocks based on AR and MA TimeSerise Model
library(quantmod)
back_sixmonth <- Sys.Date() - 180
Stock <- getSymbols("AMZN", from = back_sixmonth, to = Sys.Date(), auto.assign = FALSE)
Stock_Prices <- Stock[, 6]
Stock_TS <- as.ts(Stock_Prices, start = back_sixmonth)
#Modeling
AR_Stock <- arima(Stock_TS, order = c(1,0,0))
MA_Stock <- arima(Stock_TS, order = c(0,0,1))

if (AIC(AR_Stock) < AIC(MA_Stock)) {
  print("Analysis Based On AutoRegressive Model")
  ts.plot(Stock_TS)
  Adjusted_points <- Stock_TS - resid(AR_Stock)
  points(Adjusted_points, type = "l")
  predict(AR_Stock, n.ahead = 5)
} else {
  print("Analysis Based On MovingAverage Model")
  ts.plot(Stock_TS)
  Adjusted_points <- Stock_TS - resid(MA_Stock)
  points(Adjusted_points, type = "l")
  predict(MA_Stock, n.ahead = 5)
}

   