install.packages("fpp2")  # Time series analysis 
install.packages("httr")  # Downloading data 
install.packages("readr") # Reading datasets 
library(fpp2) 
library(httr) 
library(readr)
url <- "http://robjhyndman.com/tsdldata/data/fancy.dat" 
sales_data <- read_table(url, col_names = FALSE)
sales_ts <- ts(sales_data$X1, start = c(1987, 1), frequency = 12)  # Monthly data
autoplot(sales_ts) + 
  ggtitle("Monthly Sales Data") + 
  xlab("Year") + 
  ylab("Sales") 
summary(sales_ts) 
ggseasonplot(sales_ts, year.labels = TRUE, year.labels.left = TRUE) + 
  ggtitle("Seasonal Plot of Monthly Sales") + 
  ylab("Sales") 
ggsubseriesplot(sales_ts) + 
  ggtitle("Subseries Plot of Monthly Sales") + 
  ylab("Sales")
sales_decomposed <- decompose(sales_ts, type = "multiplicative") 
autoplot(sales_decomposed) + 
  ggtitle("Decomposition of Monthly Sales Data")
sales_arima <- auto.arima(sales_ts) 
summary(sales_arima) 
sales_forecast <- forecast(sales_arima, h = 12) 
autoplot(sales_forecast) + 
  ggtitle("Forecast of Monthly Sales") + 
  xlab("Year") + 
  ylab("Sales")