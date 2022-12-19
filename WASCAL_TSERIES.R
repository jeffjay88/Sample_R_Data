#Load data
library(readxl)
WascalTS <- read_excel("C:/Users/user/Desktop/DATA ANALYSIS_CLIMATE/WascalTS.xlsx")
View(WascalTS)

x<-WascalTS[,-c(1,3,4,5,6,7)]


library(caTools)
#make this example reproducible
set.seed(1)
#use 70% of dataset as training set and 30% as test set
sample <- sample(c(TRUE, FALSE), nrow(x), replace=TRUE, prob=c(0.7,0.3))
train  <- x[sample, ]
test   <- x[!sample, ]
dim(train)
dim(test)

library(tseries)

# library required for decimal_date() function
library(lubridate)

wasc <- ts(x, start = decimal_date(ymd("1975-01-01")),
          frequency = 12)
start(wasc)
end(wasc)



plot(wasc, xlab ="Monthly Data",
     ylab ="Rainfall",
     main ="Time",
     col.main ="darkgreen")


wasc2 <- ts(train, start = decimal_date(ymd("1975-01-01")),
           frequency = 12)
start(wasc2)
end(wasc2)


#STATIONARITY
#library(tseries)
adf.test(wasc2)

#kpss.test(wasc2)
#to figure out the order of MA
acf(wasc2) 

#to figure out the order of AR
pacf(wasc2)

#forecasting
# forecasting model using arima model
library(forecast)
fit <- auto.arima(wasc2)
fit

#check if model is fit for prediction
checkresiduals(fit)

# Next 5 forecasted values
z<-forecast(fit, 21)
z


# plotting the graph with next
# 12 monthly forecasted values
plot(forecast(fit, 12), xlab ="Monthly Data",
     ylab ="Rainfall",
     main ="Rainfall", col.main ="darkgreen")

