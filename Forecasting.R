#Section 1
#Reading in the data
dY <- read.csv("LakeHuron.csv",header = TRUE)
Y1 <- dY$LakeHuron

par(mfrow=c(1,1))
ts.plot(Y1) #We can see a trend - does not look stationary 
par(mfrow=c(2,2))
acf(Y1) #does not look stationary 
acf(Y1, lag=50)
pacf(Y1)
pacf(Y1, lag=50)
summary(Y1)

#Testing for zero mean 
t.test(Y1) #small pvalue so we reject the null hypothesis that the mean = 0


# Dickey Fuller Test 
library(tseries)
adf.test(Y1) #Null hypothesis: not stationary. High pvalue, do not reject the null 

# We have non-stationary data, so we take differences 
diff_Y1 <- diff(Y1)
par(mfrow=c(1,1))
ts.plot(diff_Y1) #We no longer have a trend
par(mfrow=c(2,1))
acf(diff_Y1, lag=50) #Better - we have a sine wave
pacf(diff_Y1, lag=50)

#Dickey Fuller Test
adf.test(diff_Y1) #Pvlaue of 0.01, hence we reject the null hypothesis of non stationarity.

#Testing for zero mean 
t.test(diff_Y1) #We have a high pvalue, hence we do not reject the null hypothesis of zero mean.

# Candidate Models
# Since our ACF plot has a damped sine wave we could try AR or ARMA models on the differenced data
# Looking at the ACF, we could perhaps also try an MA(4) model 
# Our PACF plot shows exponential decay which points towards an MA or ARMA model


############################################################

# First candidate model: ARIMA(1, 1 ,1)
library(forecast) 
fitf0 = arima(Y1, order=c(1,1,1))
fitf0 

#Plotting the inverse roots
par(mfrow=c(1,1))
plot(fitf0) #We see that the inverse root of the MA characteristic polynomial are inside the unit circle
# Hence we have invertibility.
# The inverse root of the AR characteristic polynomial is inside the unit circle.
# Hence we have stationarity.

#Residuals
res = fitf0$residuals
ts.plot(res)
par(mfrow=c(2,1));
# ACF and PACF of residuals both show that 95% of spikes inside CI.
# Residual terms are approximately white noise
acf.res <- acf(res,lag=30) 
pacf.res <- pacf(res,lag=30)

#Ljung-Box Pierce Test for correlation of the residuals
# We want no correlation between the residuals
tsdiag(fitf0)  #All pvalues > 0.05, but some are still small.


############################################################

# ARIMA(0,1,3) 
fitf3 = arima(Y1, order=c(0,1,3))
fitf3

#Plotting the inverse roots
par(mfrow=c(1,1))
plot(fitf3) # All the inverse roots inside the unit circle hence we have invertibility 

#Residuals
res3 = fitf3$residuals
ts.plot(res3)
par(mfrow=c(2,1));
# ACF and PACF of residuals 
acf(res3,lag=30) 
pacf(res3,lag=30)

#Ljung-Box Pierce 
tsdiag(fitf3) # We have high pvalue so we do not reject the null hypothesis of no correlation 


############################################################

# Second candidate model
# ARIMA(0,1,4) - CHOSEN MODEL
fitf1 = arima(Y1, order=c(0,1,4))
fitf1
fitf1$coef #parameters

#Plotting the inverse roots
par(mfrow=c(1,1))
plot(fitf1) # All the inverse roots inside the unit circle hence we have invertibility 

#Residuals
res1 = fitf1$residuals
ts.plot(res1)
par(mfrow=c(2,1));
# ACF and PACF of residuals 
acf(res1,lag=30) 
pacf(res1,lag=30)

#Ljung-Box Pierce 
tsdiag(fitf1) # We have high pvalue so we do not reject the null hypothesis of no correlation 


#######################################################

# Third candidate model
# ARIMA(3,1,0)
fitf2 = arima(Y1, order=c(3,1,0))
fitf2

#Plotting the inverse roots
par(mfrow=c(1,1))
plot(fitf2) #All the inverse roots inside the unit circle

#Residuals
res2 = fitf2$residuals
ts.plot(res2)
par(mfrow=c(2,1));
# ACF and PACF of residuals both show that 95% of spikes inside CI - white noise
acf(res2,lag=30)
pacf(res2,lag=30)

#Ljung-Box Pierce 
tsdiag(fitf2) # We have high pvalue so we do not reject the null hypothesis of no correlation 



# Our chosen model is ARIMA(0,1,4). We had to take first differences since the original data 
# wasn't stationary. Taking first differences solved this as shown by the dickey fuller test.
# We also did a t-test to check for zero mean. Looking at the ACF suggested ARIMA(0,1,4) could be 
# a candidate model. The PACF had exponential decay which also pointed towards an ARIMA(0,1,4) model.
# Then, when doing the diagnostic checks of the model we found that it was invertibile as all the 
# inverse roots were inside the unit circle. We know that it is stationary. Plots of the ACF and PACF
# of the residuals revealed that 95% of spikes were inside the confidence interval - synonymous with 
# white noise. We also did a Ljung-Box Pierce test, this tests the null hypothesis that the entire 
# residual autocorrelation function is zero at all lags from 1 to 10 
# We have high pvalues hence we do not reject this null hypothesis.

#######################################################

# FORECASTING 
# Using model ARIMA(0,1,4)

par(mfrow = c(1,1))

#5 step ahead forecast
fit1.for <- predict(fitf1, n.ahead = 5)
fit1.for$pred

par(mfrow=c(1,1))
plot(Y1, type="l", xlim = c(0,106),ylab="Lake Huron Water Level")
lines( fit1.for$pred, lty=3, pch=20, lwd=1, type="l",col = 'red')

#For 90% CI
lines( fit1.for$pred + 1.64*fit1.for$se, lty=2, pch=20, lwd=1, type="l")
lines( fit1.for$pred - 1.64*fit1.for$se, lty=2, pch=20, lwd=1, type="l")


# Our forecasts have a similar pattern to the original data. 
# Although our confidence intervals are quite wide, they do fit the pattern of the original time series

#######################################################




#Section 2

#Loading in the data
dX <- read.csv("EUStockMarket.csv",header = TRUE)
X1 <- dX$DAX #DAX data 

ts.plot(X1) #We can see a trend - does not look stationary 

par(mfrow=c(2,2))
acf(X1); #does not look stationary 
acf(X1, lag=100); 
pacf(X1); 
pacf(X1, lag=100);
summary(X1)

#Testing for zero mean 
t.test(X1) #small pvalue so we reject the null hypothesis that the mean = 0

#Dickey Fuller Test 
adf.test(X1) #high pvalue

#Since we have non-stationary data, we take first differences

diff_X1 <- diff(X1)
ts.plot(diff_X1) #We no longer have a trend
par(mfrow=c(2,1))
acf(diff_X1, lag=100) #Better - we could have a damped sine wave, or we could have spikes at 2,2s etc (SMA)
pacf(diff_X1, lag=100) #Looks like the PACF of a seasonal model- seasonal spikes

#Dickey Fuller test
adf.test(diff_X1)#snall p-value hence we reject the null-hypothesis of non-stationary data

# We take a seasonal difference to remove the trend 
# Since we have daily data (excluding weekends), we can try a period of 5


#######################################################

# We have taken a seasonal difference, and then a non-seasonal difference
diff1 <- diff(X1, lag=5) #seasonal difference - weekly data
par(mfrow=c(1,1))
plot(diff1)

par(mfrow= c(2,1))
acf(diff1, lag = 20)
pacf(diff1, lag = 20)

Z <- diff(diff1, lag=1) #non-seasonal difference
ts.plot(Z)
par(mfrow= c(2,2))
acf(Z, lag = 20) 
acf(Z, lag = 100) #spikes at lag 5,10,15 etc - points towards SARMA
pacf(Z, lag = 20)
pacf(Z, lag = 100) #spikes at lag 5,10,15 etc - points towards SARMA

#Testing for zero mean 
t.test(Z)  #high pvalue do not reject the null hypothesis of zero mean 

# Dickey Fuller Test 
library(tseries)
adf.test(Z) #small pvalue reject the null of non-stationarity.


# Candidate Models 
# SARIMA(0,1,0) x (1,1,1) period 5
# SARIMA(0,1,0) x (0,1,1) period 5


# SARIMA(0,1,0) x (1,1,1) period 5
fit1 <- arima(X1, order = c(0,1,0), seasonal = list(order = c(1,1,1), period=5))
fit1
fit1$coef

#Plotting the inverse roots
library(forecast) 
par(mfrow=c(1,1));
plot(fit1) 

#Residuals
res_1 = fit1$residuals
par(mfrow=c(1,1));
ts.plot(res_1)
par(mfrow=c(2,1));
# ACF and PACF of residuals 
acf(res_1,lag=30) 
pacf(res_1,lag=30) 

#Ljung-Box Pierce 
tsdiag(fit1) 


# SARIMA(0,1,0) x (0,1,1) period 5

fit2 <- arima(X1, order = c(0,1,0), seasonal = list(order = c(0,1,1), period=5))
fit2
fit2$coef

#Plotting the inverse roots
library(forecast) 
par(mfrow=c(1,1));
plot(fit2) 

#Residuals
res_2 = fit2$residuals
par(mfrow=c(1,1));
ts.plot(res_2)

par(mfrow=c(2,1));
acf(res_2,lag=50) 
pacf(res_2,lag=50) 

#Ljung-Box Pierce 
tsdiag(fit2) 

# Whenever the variability seems to increase with the mean, 
# it is natural to take logs to try and stabilise things
# Taking logarithms of the data 

X1_log <- log(X1) #Not stationary - we have a trend
ts.plot(X1_log) 
par(mfrow=c(2,2))
acf(X1_log, lag=20)
acf(X1_log, lag=200) 
pacf(X1_log, lag=20)
pacf(X1_log, lag=200)

# Dickey Fuller Test 
library(tseries)
adf.test(X1_log) 


# Take a difference

X3 <- diff(X1_log)

par(mfrow=c(1,1))
ts.plot(X3) 
par(mfrow=c(2,1))
acf(X3, lag=100) #no spikes, damped sinusoid
pacf(X3, lag=100) #seasonal spikes, sine shape


# Dickey Fuller Test 
library(tseries)
adf.test(X3) #small pvlaue hence we reject the null hypothesis of non-stationary data


# Plot of PACF looks like they could be some seasonality, hence we take a seasonal 
# difference and a non-seasonal difference

#Seasonal difference
X1log_diff <- diff(X1_log, lag=5) #seasonal difference - weekly data
par(mfrow= c(1,1))
ts.plot(X1log_diff)

par(mfrow= c(2,1))
acf(X1log_diff, lag = 20)
pacf(X1log_diff, lag = 20)

# Non-seasonal difference
X1log_diff2 <- diff(X1log_diff, lag=1)
plot(X1log_diff2)
par(mfrow= c(2,2))
acf(X1log_diff2, lag = 20) 
acf(X1log_diff2, lag = 100) #spikes at lag 5 points towards SMA(1)
pacf(X1log_diff2, lag = 20)
pacf(X1log_diff2, lag = 100) #exponential decay with lags at 5,10 etc SMA(1)

#Testing for zero mean 
t.test(X1log_diff2)  #high pvalue do not reject the null hypothesis of zero mean 

# Dickey Fuller Test 
library(tseries)
adf.test(X1log_diff2) #small pvalue reject the null of non-stationary.


# Fitting models 

# First model we tried, was SARIMA (0,1,0) x (0,1,1) period 5 as our ACF and PACF plots suggested

# SARIMA (0,1,0) x (0,1,1) period 5

model_1 <- arima(X1_log, order = c(0,1,0), seasonal = list(order = c(0,1,1), period=5))
model_1

#Plotting the inverse roots
library(forecast) 
plot(model_1) #invertibility fails

#Residuals
model1_res = model_1$residuals
ts.plot(model1_res)
par(mfrow=c(2,1));

acf(model1_res,lag=30) 
pacf(model1_res,lag=30) 

#Ljung-Box Pierce 
tsdiag(model_1) # good, high p-values


# Second candidate model: # SARIMA (0,1,0) x (1,1,1) period 5
# We tried a SARMA(1,1) model as our ACF does have a sort of sine shape, and 
# our PACF does have spikes at s,2s etc

model_2 <- arima(X1_log, order = c(0,1,0), seasonal = list(order = c(1,1,1), period=5))
model_2

#Plotting the inverse roots
library(forecast) 
plot(model_2) 

#Residuals
model2_res = model_2$residuals
ts.plot(model2_res)
par(mfrow=c(2,1));
acf(model2_res,lag=30) 
pacf(model2_res,lag=30) 

#Ljung-Box Pierce 
tsdiag(model_2) 



# FORECASTING 
# Chosen model:

fit1 <- arima(X1, order = c(0,1,0), seasonal = list(order = c(1,1,1), period=5))

#10 step ahead forecast
fit1.for <- predict(fit1, n.ahead = 10)
fit1.for$pred

par(mfrow=c(2,1));
plot(X1, type="l", xlim = c(0,1890),ylab="DAX Prices")
lines(fit1.for$pred, lty=1, lwd=1, type="l", col = 2)
#For 95% 
lines( fit1.for$pred + 1.96*fit1.for$se, lty=1, lwd=1, type="l")
lines( fit1.for$pred - 1.96*fit1.for$se, lty=1, lwd=1, type="l")

plot(X1, type="l", xlim = c(1600,1890),ylab="DAX Prices")
lines(fit1.for$pred, lty=1, pch = 20, lwd=1, type="l", col = 2)
lines( fit1.for$pred + 1.96*fit1.for$se, lty=1, lwd=1, type="l")
lines( fit1.for$pred - 1.96*fit1.for$se, lty=1, lwd=1, type="l")


# Forecasting results have a similar pattern to the original data
# Narrow confidence interval


# Split the log data into 80$ train and 20% test

X1_log #1860 observations 

0.8*1860 # train my model on first 1488 values

1860 - 1488

#Getting the 1488 values:

first80 <- head(X1_log,1488)
last20 <- tail(X1_log,372)

ts.plot(first80) #We see the same trend as before so we take a seasonal difference and non-seasonal

#Seasonal difference
first80_diff <- diff(first80, lag=5) #seasonal difference - weekly data

# Non-seasonal difference
first80_diff2 <- diff(first80_diff, lag=1)
par(mfrow= c(2,2))
acf(first80_diff2, lag = 20) 
acf(first80_diff2, lag = 100) #spikes at lag 5 points towards SMA(1)
pacf(first80_diff2, lag = 20)
pacf(first80_diff2, lag = 100) #exponential decay with lags at 5,10 etc SMA(1)

#Testing for zero mean 
t.test(first80_diff2)  #high pvalue do not reject the null hypothesis of zero mean 

# Dickey Fuller Test 
library(tseries)
adf.test(first80_diff2) #small pvalue so we reject the null hypothesis of non-stationary time series 


# Run the chosen model on the 80% data

model_3 <- arima(first80, order = c(0,1,0), seasonal = list(order = c(1,1,1), period=5))
model_3

library(forecast) 
par(mfrow=c(1,1));
plot(model_3) 

#Residuals
model3_resid = model_3$residuals
par(mfrow=c(1,1));
ts.plot(model3_resid)
par(mfrow=c(2,1));
# ACF and PACF of residuals 
acf(model3_resid,lag=30) 
pacf(model3_resid,lag=30) 

#Ljung-Box Pierce 
tsdiag(model_3) 

# All the diagnostic checks are satisfied, hence we proceed with the forecasting.

# Forecast for 372 values

fit3.for <- predict(model_3, n.ahead = 372)
fit3.for$pred

par(mfrow=c(2,1));
plot(X1_log, type="l", xlim = c(0,1900),ylim = c(7,9.5),ylab="DAX Prices")
lines( fit3.for$pred, lty=4, pch=20, lwd=1, type="l", col = 2)
lines( fit3.for$pred + 1.96*fit3.for$se, lty=2, pch=20, lwd=1, type="l")
lines( fit3.for$pred - 1.96*fit3.for$se, lty=2, pch=20, lwd=1, type="l")

plot(X1_log, type="l", xlim = c(1400,1900),ylim = c(7,9.5),ylab="DAX Prices")
lines( fit3.for$pred, lty=4, pch=20, lwd=1, type="l", col = 2)
lines( fit3.for$pred + 1.96*fit3.for$se, lty=2, pch=20, lwd=1, type="l")
lines( fit3.for$pred - 1.96*fit3.for$se, lty=2, pch=20, lwd=1, type="l")


# My forecasted values continues to follow the upward trend wearas the true 20%
# of the data has a lot more variability, it also has a couple of dips
# which are not accounted for in my forecasted values

# In comparison to parts d,e (forecasted based on the whole sample)
# where my model forecast matched quite well the pattern shown

