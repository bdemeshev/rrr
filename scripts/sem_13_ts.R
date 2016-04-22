library("forecast")

library("ggplot2")
library("fUnitRoots")
library("xts")

# y_t = 0.5 y_{t-1} + e_t + 0.7e_{t-1}
# теоретическая ACF
ARMAacf(ar = 0.5, ma = 0.7, lag.max = 10)

# теоретическая PACF
ARMAacf(ar = 0.5, ma = 0.7,
        lag.max = 10, pacf = TRUE)

# искусственно сгенерируем
# y_t = 0.5 y_{t-1} + e_t + 0.7e_{t-1}
y <- arima.sim(n = 200,
      model = list(ar = 0.5, ma = 0.7))
y

# искусственно сгенерируем
# z_t = 0.5 z_{t-1} + e_t + 0.7e_{t-1} - 0.2e_{t-2}
z <- arima.sim(n = 200,
  model = list(ar = 0.5, ma = c(0.7, -0.2)))

# график, выборочные ACF и PACF
tsdisplay(y)

# x_1 = y_1
# x_2 = y_1 + y_2
# x_3 = y_1 + y_2 + y_3
x <- cumsum(y)
tsdisplay(x)


z <- arima.sim(n = 200,
               model = list(ar = 0.5, ma = c(0.7)))
tsdisplay(z)

adfTest(y)

ma2 <- Arima(y, order = c(0, 0, 2))
ma2

arma11 <- Arima(y, order = c(1, 0, 1))
AIC(ma2)
AIC(arma11)

best_model <- auto.arima(y)
summary(best_model)

forecast(arma11, h = 10)

future <- forecast(arma11, h = 10)
plot(future)

length(x)
length(y)
length(z)

str(x)
str(y)
str(z)

library("lubridate")

# ymd("2010-10-10") + 1:10 * days(30)
# ymd("2010-10-10") + 1:10 * months(1)

all <- data_frame(x = x, y = y, z = z)
n_series <- ncol(all)

all_models <- list()
short_summary <- data_frame(
  id = 1:n_series,
  AIC = NA,
  RMSE = NA)

for (col_no in 1:n_series) {
  ts <- all[, col_no]
  model <- auto.arima(ts)

  all_models[[col_no]] <- model
  short_summary$AIC[col_no] <- AIC(model)
  measures <- accuracy(model)
  short_summary$RMSE[col_no] <- measures[2]
}
short_summary

# если забыли посчитать что-то
for (col_no in 1:n_series) {
  short_summary$loglik[col_no] <-
    all_models[[col_no]]$loglik
}
short_summary


saveRDS(all_models, "all_models.Rds")
saveRDS(short_summary, "short_summary.Rds")

