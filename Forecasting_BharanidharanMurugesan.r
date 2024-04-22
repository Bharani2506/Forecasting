setwd("C:/Users/bmurugesan1/OneDrive - IESEG/Documents/SEM 2/FTS/Forecasting Exam Assignment 2024") # Specify your own working directory here.
#===============================================================================
# Libraries & Data
#===============================================================================
# Data manipulation
library(portes)
library(fpp2)
library(readxl)
library(tidyverse)
library(forecast)
library(fabletools)
library(dplyr)
library(tsibble)
library(feasts)
# Aggregate by month
# Load the data
data <- read.csv("EU_trip_expenditures.csv")
agg_data <- data %>%
  group_by(year, month) %>%
  summarise(avg_exp_ngt = mean(avg_exp_ngt))
agg_data
# Convert to time series
eu_data <- ts(agg_data$avg_exp_ngt, frequency = 12, start=c(2012,1))
plot(eu_data, main="Eu Trip Expenditure",
     ylab="Average Expenditure", xlab="Year")
#===============================================================================
# Spliting Data
#===============================================================================
train <- window(eu_data, start = 2012, end = c(2017,3))
test <- window(eu_data, start = c(2017,4), end = c(2020, 3))
dum1 <- window(eu_data, start = c(2020,4), end = c(2022, 12))
covid <- window(eu_data, start = c(2012,1), end = c(2020,2))
#===============================================================================
# Q1
#===============================================================================
# Time series plot
autoplot(eu_data) +
  labs(title = "Time Series Plot",
       y = "Avg",
       x = "Year")
# Seasonal plot
ggseasonplot(eu_data, year.labels = TRUE, year.labels.left = TRUE) +
  labs(title = "Seasonal Plot",
       y = "Avg",
       x = "Month")
# Seasonal subseries plot
ggsubseriesplot(eu_data) +
  labs(title = "Seasonal Subseries Plot",
       y = "Avg",
       x = "Month")
#ACF
ggAcf(eu_data) 
#PACF
ggPacf(eu_data)  
#LAG Plot
lag.plot(eu_data)  
#===============================================================================
# Q2
#===============================================================================
par(mfrow=c(1,1))

# Using Log Transformation
cvd <- log(covid)
plot(cvd)
# Plotting the Seasonal and Monthly Trend Graphs
seasonplot(cvd, year.labels=TRUE, year.labels.left=TRUE,
           main="Seasonal Plot",
           ylab="Trips Expenditure",
           col=rainbow(25), 
           pch=20)
monthplot(cvd, main="Seasonal Subseries Plot", ylab = "Average Expenditure",
          xlab="Month", type="l")
# Plotting the ACF & PACF (Autocorrelation & Partial Autocorrelation)
lag.plot(cvd)
tsdisplay(cvd)
# Find optimal lambda
lambda = BoxCox.lambda(covid)
lambda
exp_train = BoxCox(train, lambda)
exp_test = BoxCox(test, lambda)
#===============================================================================
# Q3
#===============================================================================
# Applying seasonal naive forecasting and plotting
# Save Length of Test 
h = length(test)

covid_m1 <- snaive(train, h=h)
autoplot(covid_m1)
lines(test, col="red")

#Evaluating forecast accuracy
accuracy(covid_m1, test)

# Getting residuals
checkresiduals(covid_m1)
res <- residuals(covid_m1)

tsdisplay(res)
Box.test(res, lag = 12, fitdf = 0, type = "Lj")

# Evaluating Forecast Accuracy
accuracy(covid_m1, test)[,c(2,3,4,5,6,7)]
#===============================================================================
# Q4
#===============================================================================
# Applying STL decomposition
trip_stl <- stl(train, s.window = "periodic")
plot(train, col = "black", main = "Trip Expenditure", ylab = "AVG", xlab = "")
lines(trip_stl$time.series[,2], col = "red")

autoplot(trip_stl)
# Initialize empty data frames to store accuracy results
accuracy_train <- data.frame(Method = c("Naive", "RW Drift", "ETS", "ARIMA"),
                             RMSE = numeric(4),
                             MAE = numeric(4),
                             MAPE = numeric(4),
                             MASE = numeric(4))

accuracy_test <- data.frame(Method = c("Naive", "RW Drift", "ETS", "ARIMA"),
                            RMSE = numeric(4),
                            MAE = numeric(4),
                            MAPE = numeric(4),
                            MASE = numeric(4))

# Function to update accuracy data frame
update_accuracy <- function(df, method_name, accuracy_result) {
  df[df$Method == method_name, "RMSE"] <- accuracy_result[1, "RMSE"]
  df[df$Method == method_name, "MAE"] <- accuracy_result[1, "MAE"]
  df[df$Method == method_name, "MAPE"] <- accuracy_result[1, "MAPE"]
  df[df$Method == method_name, "MASE"] <- accuracy_result[1, "MASE"]
  return(df)
}

# Naive Forecasting
stl_nai <- forecast(trip_stl, method = "naive")
accuracy_nai <- accuracy(stl_nai, test)
accuracy_train <- update_accuracy(accuracy_train, "Naive", accuracy_nai)
accuracy_test <- update_accuracy(accuracy_test, "Naive", accuracy_nai)

# RW Drift Forecasting
stl_rwdrift <- forecast(trip_stl, method = "rwdrift")
accuracy_rwdrift <- accuracy(stl_rwdrift, test)
accuracy_train <- update_accuracy(accuracy_train, "RW Drift", accuracy_rwdrift)
accuracy_test <- update_accuracy(accuracy_test, "RW Drift", accuracy_rwdrift)

# ETS Forecasting
stl_ets <- forecast(trip_stl, method = "ets")
accuracy_ets <- accuracy(stl_ets, test)
accuracy_train <- update_accuracy(accuracy_train, "ETS", accuracy_ets)
accuracy_test <- update_accuracy(accuracy_test, "ETS", accuracy_ets)

# ARIMA Forecasting
stl_arima <- forecast(trip_stl, method = "arima")
accuracy_arima <- accuracy(stl_arima, test)
accuracy_train <- update_accuracy(accuracy_train, "ARIMA", accuracy_arima)
accuracy_test <- update_accuracy(accuracy_test, "ARIMA", accuracy_arima)

# Print accuracy results for training set
print("Accuracy Results for Training Set:")
print(accuracy_train)

# Print accuracy results for test set
print("Accuracy Results for Test Set:")
print(accuracy_test)
checkresiduals(stl_nai)
checkresiduals(stl_rwdrift)
checkresiduals(stl_ets)
checkresiduals(stl_arima)

#===============================================================================
# Q5
#===============================================================================
# Automated ETS model selection
auto_ets <- ets(train)
summary(auto_ets)
# Manual selection of ETS models
model_ets_AAA <- ets(train, model = "AAA")
model_ets_ANA <- ets(train, model = "ANA")

# Summary of manually selected ETS models
summary(model_ets_AAA)
summary(model_ets_ANA)
# Residual diagnostics for manually selected ETS models
checkresiduals(model_ets_AAA)
checkresiduals(model_ets_ANA)

# Forecast accuracy for manually selected ETS models
accuracy_AAA <- accuracy(forecast(model_ets_AAA, h = length(test)), test)
accuracy_ANA <- accuracy(forecast(model_ets_ANA, h = length(test)), test)

print(accuracy_AAA)
print(accuracy_ANA)
# Fit the ETS(A,N,A) model to the training data
final_ets_model <- ets(train, model = "ANA")

# Display the summary of the final ETS model
summary(final_ets_model)
# Forecast visualization for the final ETS model
final_ets_forecast <- forecast(final_ets_model, h = length(test))
autoplot(final_ets_forecast) + 
  ggtitle("Forecast using Final ETS Model") +
  xlab("Time") + 
  ylab("Value")
#===============================================================================
# Q6
#===============================================================================
# Fit ARIMA model using auto.arima
auto_arima_model <- auto.arima(train)

# Print the estimated ARIMA model
print(auto_arima_model)
# Residual diagnostics for auto.arima model
checkresiduals(auto_arima_model)

# Forecast accuracy for auto.arima model
accuracy_auto_arima <- accuracy(forecast(auto_arima_model, h = length(test)), test)
print(accuracy_auto_arima)

# Fit SARIMA model
sarima_model1 <- Arima(train, order=c(1,1,1), seasonal=c(1,1,1), 
                       lambda=0, include.constant=TRUE)

# Forecast
forecast_sarima1 <- forecast(sarima_model1, h=length(test))
# Fit SARIMA model
sarima_model2 <- Arima(train, order=c(0,1,2), seasonal=c(0,1,2), 
                       lambda=0, include.constant=TRUE)

# Forecast
forecast_sarima2 <- forecast(sarima_model2, h=length(test))
# Fit SARIMA model
sarima_model3 <- Arima(train, order=c(2,1,1), seasonal=c(1,1,2), 
                       lambda=0, include.constant=TRUE)

# Forecast
forecast_sarima3 <- forecast(sarima_model3, h=length(test))
# Evaluate SARIMA model performance
accuracy_sarima1 <- accuracy(forecast_sarima1, test)
print("Accuracy measures for SARIMA(1,1,1)(1,1,1)[12]:")
print(accuracy_sarima1)

accuracy_sarima2 <- accuracy(forecast_sarima2, test)
print("Accuracy measures for SARIMA(0,1,2)(0,1,2)[12]:")
print(accuracy_sarima2)

accuracy_sarima3 <- accuracy(forecast_sarima3, test)
print("Accuracy measures for SARIMA(2,1,1)(1,1,2)[12]:")
print(accuracy_sarima3)

# Estimate ARIMA(1,0,0)(1,1,0)[12] model
arima_model4 <- arima(train, order=c(1,0,0), seasonal=list(order=c(1,1,0), period=12))

# Forecast using ARIMA model
forecast_arima4 <- forecast(arima_model4, h=length(test))

# Calculate forecast accuracy measures
accuracy_arima4 <- accuracy(forecast_arima4, test)

# Print forecast accuracy measures
print(accuracy_arima4)
#===============================================================================
# Q7
#===============================================================================

#===============================================================================
# Q8
#===============================================================================

f1 <- window(eu_data, start = c(2012,1), end = c(2020,3))
new_fit <- ets(f1, model="AAA", damped = TRUE, biasadj=FALSE)
abe_new_fit <- forecast(new_fit, h=57) # Adjusted for additional months
plot(abe_new_fit)
#===============================================================================
# Q9
#===============================================================================
par(mfrow=c(1,1))
# Plot the original time series
plot(eu_data, main="EU Travel Expenditures", ylab="Average Expenditure", xlab="Date", col="blue", ylim=c(min(eu_data), max(eu_data)))

# Add forecasted values to the plot
lines(abe_new_fit$mean, col="red")

# Highlight the pandemic period
abline(v=as.Date("2020-04-01"), col="gray", lty=2)
abline(v=as.Date("2020-12-31"), col="gray", lty=2)

# Extract actual values during the pandemic period
pandemic_data <- window(eu_data, start = c(2020,4), end = c(2022,12))

# Extract forecasted values during the pandemic period
forecast_pandemic <- window(abe_new_fit$mean, start = c(2020,4), end = c(2022,12))

# Calculate the difference between actual and forecasted values
difference <- pandemic_data - forecast_pandemic
#===============================================================================
# END
#===============================================================================
print("Difference between Actual and Forecasted values during the pandemic:")
print(summary(difference))

