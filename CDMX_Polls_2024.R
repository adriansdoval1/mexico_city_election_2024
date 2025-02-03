library(readxl)
library(data.table)
library(fpp2)
CDMX_Polls_2024 <- read_excel("Adrián Stuff/Adrian Projects/CDMX_Polls_2024.xlsx")

CDMX_Polls_2024_dt <- as.data.table(CDMX_Polls_2024)

cb_ts <- ts(CDMX_Polls_2024_dt$CB, start = c(2021,7), frequency = 12)
acf(cb_ts)
pacf(cb_ts)
# AR(1)

forecast(arima(cb_ts, order = c(1,0,0)), 1)

res1 <- 0

forecast(arima(cb_ts[-11], order = c(1,0,0)), 1)
res1[1] <- forecast(arima(cb_ts[-11], order = c(1,0,0)), 1)$mean - cb_ts[11]

res1[2] <- forecast(arima(cb_ts[-c(10, 11)], order = c(1,0,0)), 1)$mean - cb_ts[10]

res1[3] <- forecast(arima(cb_ts[-c(9, 10, 11)], order = c(1,0,0)), 1)$mean - cb_ts[9]

res1[4] <- forecast(arima(cb_ts[-c(8, 9, 10, 11)], order = c(1,0,0)), 1)$mean - cb_ts[8]

res1[5] <- forecast(arima(cb_ts[-c(7, 8, 9, 10, 11)], order = c(1,0,0)), 1)$mean - cb_ts[7]

res1[6] <- forecast(arima(cb_ts[-c(6, 7, 8, 9, 10, 11)], order = c(1,0,0)), 1)$mean - cb_ts[6]

res1[7] <- forecast(arima(cb_ts[-c(5, 6, 7, 8, 9, 10, 11)], order = c(1,0,0)), 1)$mean - cb_ts[4]

mean(res1)


# MA(1)

forecast(arima(cb_ts, order = c(0,0,1)), 1)

res2 <- 0

forecast(arima(cb_ts[-11], order = c(0,0,1)), 1)
res2[1] <- forecast(arima(cb_ts[-11], order = c(0,0,1)), 1)$mean - cb_ts[11]

res2[2] <- forecast(arima(cb_ts[-c(10, 11)], order = c(0,0,1)), 1)$mean - cb_ts[10]

res2[3] <- forecast(arima(cb_ts[-c(9, 10, 11)], order = c(0,0,1)), 1)$mean - cb_ts[9]

res2[4] <- forecast(arima(cb_ts[-c(8, 9, 10, 11)], order = c(0,0,1)), 1)$mean - cb_ts[8]

res2[5] <- forecast(arima(cb_ts[-c(7, 8, 9, 10, 11)], order = c(0,0,1)), 1)$mean - cb_ts[7]

res2[6] <- forecast(arima(cb_ts[-c(6, 7, 8, 9, 10, 11)], order = c(0,0,1)), 1)$mean - cb_ts[6]

res2[7] <- forecast(arima(cb_ts[-c(5, 6, 7, 8, 9, 10, 11)], order = c(0,0,1)), 1)$mean - cb_ts[4]

mean(res2)


# ARMA(1, 1)

forecast(arima(cb_ts, order = c(1,0,1)), 1)

res3 <- 0

forecast(arima(cb_ts[-11], order = c(1,0,1)), 1)
res3[1] <- forecast(arima(cb_ts[-11], order = c(1,0,1)), 1)$mean - cb_ts[11]

res3[2] <- forecast(arima(cb_ts[-c(10, 11)], order = c(1,0,1)), 1)$mean - cb_ts[10]

res3[3] <- forecast(arima(cb_ts[-c(9, 10, 11)], order = c(1,0,1)), 1)$mean - cb_ts[9]

res3[4] <- forecast(arima(cb_ts[-c(8, 9, 10, 11)], order = c(1,0,1)), 1)$mean - cb_ts[8]

res3[5] <- forecast(arima(cb_ts[-c(7, 8, 9, 10, 11)], order = c(1,0,1)), 1)$mean - cb_ts[7]

res3[6] <- forecast(arima(cb_ts[-c(6, 7, 8, 9, 10, 11)], order = c(1,0,1)), 1)$mean - cb_ts[6]

res3[7] <- forecast(arima(cb_ts[-c(5, 6, 7, 8, 9, 10, 11)], order = c(1,0,1)), 1)$mean - cb_ts[4]

mean(res3)



# ARIMA(1, 1, 1)

forecast(arima(cb_ts, order = c(1,1,1)), 1)

res4 <- 0

forecast(arima(cb_ts[-11], order = c(1,1,1)), 1)
res4[1] <- forecast(arima(cb_ts[-11], order = c(1,1,1)), 1)$mean - cb_ts[11]

res4[2] <- forecast(arima(cb_ts[-c(10, 11)], order = c(1,1,1)), 1)$mean - cb_ts[10]

res4[3] <- forecast(arima(cb_ts[-c(9, 10, 11)], order = c(1,1,1)), 1)$mean - cb_ts[9]

res4[4] <- forecast(arima(cb_ts[-c(8, 9, 10, 11)], order = c(1,1,1)), 1)$mean - cb_ts[8]




mean(abs(res1))
mean(abs(res2))
mean(abs(res3))
mean(abs(res4))

# Best model is ARIMA(1,1,1)
fit <- arima(cb_ts, order = c(1,1,1))
cdmx_ma <- forecast(arima(cb_ts, order = c(1,1,1)), 1)
cdmx_ma


acf(resid(cdmx_ma))
# Simulation
arima.sim(list(order = c(1,1,1), ar=fit$coef["ar1"],ma=fit$coef["ma1"]),sd=fit$sigma,10)











