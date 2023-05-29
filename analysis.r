library(ggplot2)    
library(forecast)    

data <- read.csv("timeseries.csv")

ts_data <- ts(data$y, start = c(data$year[1], data$month[1]), frequency = 12)

autoplot(ts_data) + xlab("Date") + ylab("Value") + ggtitle("Time Series Data")

decomposed <- decompose(ts_data)

autoplot(decomposed)

forecast_model <- forecast(ts_data)

autoplot(forecast_model) + xlab("Date") + ylab("Value") + ggtitle("Time Series Forecast")

stl_decomposed <- stl(ts_data, s.window = "periodic")

plot(stl_decomposed$time.series[, "seasonal"], main = "Seasonal Component")

trend_model <- lm(ts_data ~ time(ts_data))

summary(trend_model)
