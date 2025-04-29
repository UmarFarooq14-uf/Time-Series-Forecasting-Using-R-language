
setwd('C:/Teja/MSBA/Semester 2/Data Mining for Competitive Advantage/Project/Dataset')

data = read.csv("HTRUCKSSAAR.csv")


data$date = paste(data$Year, data$Month, sep = "/")

bu_data = data[c("date", "Sales")]

#Predicting Truck Sales
Trucks.ts = ts(bu_data$Sales, start = c(1967, 1), end = c(2022, 12), freq = 12)
summary(Trucks.ts)

length(Trucks.ts)
# check for missing values in bu_data
sum(is.na(bu_data))


ylow = round(summary(Trucks.ts)[1],-4)
yhigh = summary(Trucks.ts)[6] + 29
paste(ylow, yhigh)

options(scipen = 999) # prevent scientific notation from showing on the plot

plot(Trucks.ts, ylim = c(ylow, yhigh),  ylab = "Sales", 
     xlab = "Year", bty = "l", xaxt = "n", xlim = c(1967, (2022)), main = "Time Series Plot - Truck Sales")
axis(1, at = seq(1967, 2022, 1), labels = format(seq(1967, 2022, 1)))


#partitioning

nValid = 12
nTrain = length(Trucks.ts) - nValid

train_Trucks.ts = window(Trucks.ts, start = c(1967, 1), end = c(2021, 12))
valid_Trucks.ts = window(Trucks.ts, start = c(2022, 1), end = c(2022, 12))
summary(train_Trucks.ts)
summary(valid_Trucks.ts)


#ETS
set.seed(111)
library(forecast)
Truck_ETS = ets(train_Trucks.ts, model = "ZZZ") 
summary(Truck_ETS)

Truck_ETS.pred = forecast(Truck_ETS, h = 48, level = 0,model = "ZZZ")
print(Truck_ETS.pred)
accuracy(Truck_ETS.pred, valid_Trucks.ts) # 
Truck_ETS.pred$method
Truck_ETS.pred$model$aic 

plot(Truck_ETS.pred, ylim = c(ylow, yhigh),  ylab = "Truck Sales", 
     xlab = "Year", bty = "l", xaxt = "n", xlim = c(1967, (2022)), main = "ETS: Forecasted Vs Actual Truck Sales")
axis(1, at = seq(1967, 2022, 1), labels = format(seq(1967, 2022, 1)))
lines(valid_Trucks.ts, lwd = 2, col = "Red") 
lines(Truck_ETS.pred$fitted, lwd = 2, col = "Green") 





#ARIMA

library(forecast)

# Create the AR model
model_AR <- Arima(train_Trucks.ts, order=c(2,0,0))

# Print model summary
summary(model_AR)

# Make predictions for validation set and 2023
forecast_AR <- forecast(model_AR, h=12)
print(forecast_AR)

forecast_AR <- forecast(model_AR, h=24)
print(forecast_AR)

# Plot the forecasted values
plot(forecast_AR, main="Truck Sales Forecast - AR", xlab="Year", ylab="Sales")


#Plot Actual Vs Forecasted
plot(forecast_AR, main="ARIMA: Forecasted Vs Actual Truck Sales", xlab="Year", ylab="Sales")
lines(valid_Trucks.ts, lwd = 2, col = "Red") 

# Compute and print the MASE
accuracy_AR <- accuracy(forecast_AR, valid_Trucks.ts)
print(accuracy_AR)




setwd('C:/Teja/MSBA/Semester 2/Data Mining for Competitive Advantage/Project/Dataset')
#Load necessary packages
library(forecast)

# Read the data
data <- read.csv("HTRUCKSSAAR.csv")
data$date <- as.Date(paste0(data$Year,"-",data$Month,"-01"))

# Create time series object
ts_data <- ts(data$Sales, start = c(1967,1), frequency = 12)



#Load necessary packages
library(forecast)
library(ggplot2)

# Read the data
data <- read.csv("HTRUCKSSAAR.csv")
data$date <- as.Date(paste0(data$Year,"-",data$Month,"-01"))

# Create time series object
ts_data <- ts(data$Sales, start = c(1967,1), frequency = 12)

# Decompose the time series
decomp <- decompose(ts_data)



# Stationarity check
library(tseries)
adf.test(ts_data)
kpss.test(ts_data)

# Differencing to achieve stationarity
ts_data_diff <- diff(ts_data, differences = 1)
adf.test(ts_data_diff)
kpss.test(ts_data_diff)

# Autocorrelation and partial autocorrelation plots
acf(ts_data_diff)
pacf(ts_data_diff)

print(Acf)

# Create training and validation datasets
train_data <- window(ts_data, end = c(2021,12))
valid_data <- window(ts_data, start = c(2022,1))
