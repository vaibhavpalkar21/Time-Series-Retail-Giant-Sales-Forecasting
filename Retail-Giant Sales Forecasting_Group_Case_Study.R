
######## Retail-Giant Sales Forecasting_Group_Case_Study #########

# Group Members:
# Dr. Rajendra Warke
# Pradnya Paithankar
# Vaibhav Palkar
# Sameer Sinha


# load libraries
library(forecast)
library(tseries)
library(dplyr)

############################ DATA COLLECTION ##########################################
# Read data
superstore <- read.csv("Global Superstore.csv",stringsAsFactors = F)

################## DATA UNDERSTANDING & EXPLORATION ###################################
# dimensions
str(superstore)
# dataframe global superstore contains 24 variables and 51290 observations

# check for NA's
sum(is.na(superstore))
# total NA values = 41296
sapply(superstore, FUN=function(x){sum(is.na(x))})
# all 41296 NA values are in Postal.Code column, hence, no action to be taken at this step

# Check blank values
sum(is.null(superstore))
# no blank/Null values found

# Convert market and Segment into factors
superstore$Market <- as.factor(superstore$Market)
levels(superstore$Market)
# 7 Geographic Market Segments :: "Africa" "APAC"   "Canada" "EMEA"   "EU"     "LATAM"  "US" 

superstore$Segment <- as.factor(superstore$Segment)
levels(superstore$Segment)
# 3 customer Market Segments :: "Consumer"    "Corporate"   "Home Office"

superstore <- mutate(superstore,market.segment=paste(Market,Segment,sep="_"))
superstore$market.segment <- as.factor(superstore$market.segment)

levels(superstore$market.segment)
# 21 market segments identified as ::
# [1] "Africa_Consumer"    "Africa_Corporate"   "Africa_Home Office" "APAC_Consumer"     
# [5] "APAC_Corporate"     "APAC_Home Office"   "Canada_Consumer"    "Canada_Corporate"  
# [9] "Canada_Home Office" "EMEA_Consumer"      "EMEA_Corporate"     "EMEA_Home Office"  
# [13] "EU_Consumer"        "EU_Corporate"       "EU_Home Office"     "LATAM_Consumer"    
# [17] "LATAM_Corporate"    "LATAM_Home Office"  "US_Consumer"        "US_Corporate"      
# [21] "US_Home Office"    

### 21 subsets of dataset can be identified with the column market.segment

head(superstore)
# Convert Order.Date & Ship.Date columns to proper dates
superstore$Order.Date <- as.Date(superstore$Order.Date, format="%d-%m-%Y")

# Create month sequence column by Order.Date
superstore$Order.Month.Seq <- sapply(superstore$Order.Date, function(X) length(seq(from= min(superstore$Order.Date), to = X, by = 'month')))

head(superstore)

summary(superstore$Order.Date)
# data is for 48 months from Jan'11 to Dec'14

# New data frame to store aggregated values of Sales, Quantity and Profit over Order.date to arrive at 
# monthly values

superstore.summary <- superstore %>% group_by(market.segment,Order.Month.Seq) %>%
                                      summarise(Total.Sales=sum(Sales),
                                                Total.Profit=sum(Profit),
                                                Total.Quantity=sum(Quantity))
head(superstore.summary)


# Identifying two most profitable and consistent market segments using metric COV (Coefficient of Variation)
# of Profit

Profit.Summary <- superstore.summary %>% 
                  group_by(market.segment) %>%
                  summarise(Covar=sd(Total.Profit)*100/mean(Total.Profit)) %>%
                  arrange(Covar)
Profit.Summary

# Most consistently profitable market segments identified as EU_Consumer and APAC_Consumer

  # market.segment   Covar
  # <fct>            <dbl>
  #   1 EU_Consumer       62.4
  # 2 APAC_Consumer     63.2

# Create two dataframes for top two profitable segments

EU_Consumer <- subset(superstore.summary,market.segment=="EU_Consumer") %>% 
                arrange(Order.Month.Seq)
APAC_Consumer<- subset(superstore.summary,market.segment=="APAC_Consumer") %>%
                  arrange(Order.Month.Seq)

# Creating time series for 48 months for Sales and Quantity for each segment for analysis

ts.EU_Consumer_Sales <- ts(EU_Consumer$Total.Sales)
plot(ts.EU_Consumer_Sales)
axis(side=1,at=seq(0,48,by=2))
# Upward trend and yearly seasonality found

ts.EU_Consumer_Quantity <- ts(EU_Consumer$Total.Quantity)
plot(ts.EU_Consumer_Quantity)
axis(side=1,at=seq(0,48,by=2))
# Upward trend and yearly seasonality found

ts.APAC_Consumer_Sales <- ts(APAC_Consumer$Total.Sales)
plot(ts.APAC_Consumer_Sales)
axis(side=1,at=seq(0,48,by=2))
# Upward trend and yearly seasonality found

ts.APAC_Consumer_Quantity <- ts(APAC_Consumer$Total.Quantity)
plot(ts.APAC_Consumer_Quantity)
axis(side=1,at=seq(0,48,by=2))
# Upward trend and yearly seasonality found

par(mfrow=c(2,2))
plot(ts.EU_Consumer_Sales,main="EU_Consumer_Sales")
plot(ts.EU_Consumer_Quantity,main="EU_Consumer_Quantity")
plot(ts.APAC_Consumer_Sales,main="APAC_Consumer_Sales")
plot(ts.APAC_Consumer_Quantity,main="APAC_Consumer_Quantity")
par(mfrow=c(1,1))

########## CREATE TIME SERIES FOR 42 MONTHS FOR MODEL BUILDING & SMOOTHENING #########

# creating list of all time series
ts_list <- list()
ts_list[[1]] <- ts.EU_Consumer_Sales
ts_list[[2]] <- ts.EU_Consumer_Quantity
ts_list[[3]] <- ts.APAC_Consumer_Sales
ts_list[[4]] <- ts.APAC_Consumer_Quantity

# creating in data time series for first 42 months
ts_list_indata <- lapply(ts_list,FUN=function(x){
  y <- ts((x)[1:42])
})

# Using moving average smoothening of width 3 and creating final data frame for modeling
par(mfrow=c(2,2))
ts_list_indata_smooth <- lapply(ts_list_indata,FUN=function(x){
  smooth <- stats::filter(x,filter = rep(1/3,3),method = 'convolution',sides = 2)
  smooth[1] <- smooth[2] - (smooth[3]-smooth[2])
  smooth[42] <- smooth[41] + (smooth[41]- smooth[40])
  plot(x)
  lines(smooth,col='red')
  smooth_df <- as.data.frame(cbind(1:42,as.vector(smooth)))
  })
par(mfrow=c(1,1))
# smoothened TS for segments show linear upward trend with seasonality

# getting individual data frames for modeling
EU_Consumer_Sales_ïn_sm <- ts_list_indata_smooth[[1]]
EU_Consumer_Quantity_in_sm <- ts_list_indata_smooth[[2]]
APAC_Consumer_Sales_in_sm<- ts_list_indata_smooth[[3]]
APAC_Consumer_Quantity_in_sm <- ts_list_indata_smooth[[4]]

colnames(EU_Consumer_Sales_ïn_sm) <- c("Month","Sales")
colnames(EU_Consumer_Quantity_in_sm)<- c("Month","Quantity")
colnames(APAC_Consumer_Sales_in_sm)<- c("Month","Sales")
colnames(APAC_Consumer_Quantity_in_sm)<- c("Month","Quantity")

############ MODEL BUILDING FOR EU_Consumer Sales #############
  # GLOBAL COMPONENT MODELING
lm_EU_Consumer_Sales <- lm(Sales~sin(Month*0.5)+cos(Month*0.2) +  Month, data=EU_Consumer_Sales_ïn_sm)
summary(lm_EU_Consumer_Sales)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      18430.66    1553.74  11.862 2.42e-14 ***
#   sin(Month * 0.5) -6049.22    1060.99  -5.701 1.46e-06 ***
#   cos(Month * 0.2) -4230.09    1135.03  -3.727 0.000629 ***
#   Month              531.34      63.39   8.382 3.62e-10 ***
# Residual standard error: 4915 on 38 degrees of freedom
# Multiple R-squared:  0.7465,	Adjusted R-squared:  0.7265 
# F-statistic: 37.31 on 3 and 38 DF,  p-value: 2.064e-11
  
  # GLOBAL COMPONENT PREDICTION
global_In_EU_Consumer_Sales <- predict(lm_EU_Consumer_Sales,data.frame(Month =c(1:42)))
plot(ts(EU_Consumer_Sales_ïn_sm$Sales))
lines(global_In_EU_Consumer_Sales,col='red')

  # LOCAL COMPONENT IDENTIFICATION
local_EU_Consumer_Sales <- ts_list_indata[[1]] - global_In_EU_Consumer_Sales
plot(local_EU_Consumer_Sales)
acf(local_EU_Consumer_Sales)
pacf(local_EU_Consumer_Sales)
armafit_EU_Consumer_Sales <-auto.arima(local_EU_Consumer_Sales)
tsdiag(armafit_EU_Consumer_Sales )
armafit_EU_Consumer_Sales
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 105750721:  log likelihood=-447.6
# AIC=897.21   AICc=897.31   BIC=898.95
# This indicates that local component is stationary

  # CONFIRMATION OF STATIONARITY OF LOCAL/RESIDUAL
adf.test(local_EU_Consumer_Sales,alternative = "stationary")
# p-value = 0.01, hence, series is stationary
kpss.test(local_EU_Consumer_Sales)
# p-value = 0.1, hence, series is stationary

  # FUTURE FORECAST for next 6 months

global_pred_out_EU_Consumer_Sales <- predict(lm_EU_Consumer_Sales,data.frame(Month =c(43:48)))

#Now, let's compare our prediction with the actual values, using MAPE

Accuracy_Check_EU_Consumer_Sales <- accuracy(global_pred_out_EU_Consumer_Sales,EU_Consumer$Total.Sales[43:48])[5]
Accuracy_Check_EU_Consumer_Sales
# MAPE:23.87619

  # PLOTTING ENTIRE DATASET FOR PREDICTIONS

total_EU_Consumer_Sales_Predict <- c(ts(global_In_EU_Consumer_Sales),ts(global_pred_out_EU_Consumer_Sales))
plot(ts(EU_Consumer$Total.Sales), col = "black")
lines(total_EU_Consumer_Sales_Predict, col = "red")

# Build AUTOARIMA Model
arima_EU_Consumer_Sales <- auto.arima(ts_list_indata[[1]])
arima_EU_Consumer_Sales
tsdiag(arima_EU_Consumer_Sales)
plot(arima_EU_Consumer_Sales$x, col="black")
lines(fitted(arima_EU_Consumer_Sales), col="red")

#Again, l et's check if the residual series is white noise

resi_auto_arima_EU_Consumer_Sales <- ts_list_indata[[1]] - fitted(arima_EU_Consumer_Sales)

adf.test(resi_auto_arima_EU_Consumer_Sales,alternative = "stationary")
# P value : 0.01
kpss.test(resi_auto_arima_EU_Consumer_Sales)
# P value : 0.1
# hence, residual series is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima_EU_Consumer_Sales <- predict(arima_EU_Consumer_Sales, n.ahead = 6)

MAPE_auto_arima_EU_Consumer_Sales <- accuracy(fcast_auto_arima_EU_Consumer_Sales$pred,EU_Consumer$Total.Sales[43:48])[5]
MAPE_auto_arima_EU_Consumer_Sales
# MAPE : 28.9226

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_EU_Consumer_Sales <- c(fitted(arima_EU_Consumer_Sales),ts(fcast_auto_arima_EU_Consumer_Sales$pred))
plot(ts(EU_Consumer$Total.Sales), col = "black")
lines(auto_arima_pred_EU_Consumer_Sales, col = "red")


############## MODEL BUILDING FOR EU_Consumer Quantity #############

  # GLOBAL COMPONENT MODELING
lm_EU_Consumer_Quantity <- lm(Quantity~sin(Month*0.5)*poly(Month,3)+cos(Month*0.5)*poly(Month,3)+Month, data=EU_Consumer_Quantity_in_sm)
summary(lm_EU_Consumer_Quantity)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      228.5043    13.5949  16.808  < 2e-16 ***
#   sin(Month * 0.5) -69.4387     9.2909  -7.474 5.67e-09 ***
#   cos(Month * 0.5) -41.8617     9.6173  -4.353 9.78e-05 ***
#   Month              6.6373     0.5505  12.057 1.48e-14 ***
# Residual standard error: 43.05 on 38 degrees of freedom
# Multiple R-squared:  0.851,	Adjusted R-squared:  0.8393 
# F-statistic: 72.37 on 3 and 38 DF,  p-value: 9.027e-16

global_In_EU_Consumer_Quantity <- predict(lm_EU_Consumer_Quantity,data.frame(Month =c(1:42)))
plot(ts(EU_Consumer_Quantity_in_sm$Quantity))
lines(global_In_EU_Consumer_Quantity,col='red')

# LOCAL COMPONENT IDENTIFICATION
local_EU_Consumer_Quantity <- ts_list_indata[[2]] - global_In_EU_Consumer_Quantity
plot(local_EU_Consumer_Quantity)
#auto.arima(local_EU_Consumer_Quantity)

# check for autoregressive ness in the local component
acf(local_EU_Consumer_Quantity) # found to have q = 3
pacf(local_EU_Consumer_Quantity) # found to have p = 2
armafit_EU_Consumer_Quantity <-arima(local_EU_Consumer_Quantity,order = c(2,0,3),method = "ML")
tsdiag(armafit_EU_Consumer_Quantity )
armafit_EU_Consumer_Quantity
# Coefficients:
#   ar1      ar2      ma1      ma2     ma3  intercept
# -0.4088  -0.2232  -1.1842  -0.6043  0.8060     0.1474
# s.e.   0.1880   0.1695   0.1726   0.2837  0.1624     0.1081
# sigma^2 estimated as 2553:  log likelihood = -229.95,  aic = 473.9

arma_pred_EU_Consumer_Quantity <- fitted(armafit_EU_Consumer_Quantity)
## Calculate residuals
resi_EU_Consumer_Quantity <- local_EU_Consumer_Quantity - arma_pred_EU_Consumer_Quantity

# CONFIRMATION OF STATIONARITY OF LOCAL/RESIDUAL
acf(resi_EU_Consumer_Quantity)
pacf(resi_EU_Consumer_Quantity)
# both acf and pacf plots show all lags nearly zero

adf.test(resi_EU_Consumer_Quantity,alternative = "stationary")
# p-value = 0.0128, hence, stationary
kpss.test(resi_EU_Consumer_Quantity)
# p-value = 0.1, hence, series is stationary

local_pred_EU_Consumer_Quantity <- c(arma_pred_EU_Consumer_Quantity,as.vector(predict(armafit_EU_Consumer_Quantity,n.ahead = 6)[[1]]))

# FUTURE FORECAST for next 6 months
global_pred_out_EU_Consumer_Quantity <- predict(lm_EU_Consumer_Quantity,data.frame(Month =c(43:48)))
global_pred_EU_Consumer_Quantity <- c(global_In_EU_Consumer_Quantity,global_pred_out_EU_Consumer_Quantity)

pred_EU_Consumer_Quantity <- global_pred_EU_Consumer_Quantity+ local_pred_EU_Consumer_Quantity
#Now, let's compare our prediction with the actual values, using MAPE, RMSE & others

Accuracy_Check_EU_Consumer_Quantity <- accuracy(global_pred_out_EU_Consumer_Quantity,EU_Consumer$Total.Quantity[43:48])[5]
Accuracy_Check_EU_Consumer_Quantity
# MAPE = 30.39741

# PLOTTING ENTIRE DATASET FOR PREDICTIONS

plot(ts(EU_Consumer$Total.Quantity), col = "black")
lines(pred_EU_Consumer_Quantity, col = "red")


# Build AUTOARIMA Model
arima_EU_Consumer_Quantity <- auto.arima(ts_list_indata[[2]])
arima_EU_Consumer_Quantity  
tsdiag(arima_EU_Consumer_Quantity)
plot(arima_EU_Consumer_Quantity$x, col="black")
lines(fitted(arima_EU_Consumer_Quantity), col="red")

#Again, l et's check if the residual series is white noise

resi_auto_arima_EU_Consumer_Quantity <- ts_list_indata[[2]] - fitted(arima_EU_Consumer_Quantity)

adf.test(resi_auto_arima_EU_Consumer_Quantity,alternative = "stationary")
# P value : 0.04521
kpss.test(resi_auto_arima_EU_Consumer_Quantity)
# P value : 0.1
# hence, residual series is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima_EU_Consumer_Quantity <- predict(arima_EU_Consumer_Quantity, n.ahead = 6)

MAPE_auto_arima_EU_Consumer_Quantity <- accuracy(fcast_auto_arima_EU_Consumer_Quantity$pred,EU_Consumer$Total.Quantity[43:48])[5]
MAPE_auto_arima_EU_Consumer_Quantity
# MAPE : 30.13319

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_EU_Consumer_Quantity <- c(fitted(arima_EU_Consumer_Quantity),ts(fcast_auto_arima_EU_Consumer_Quantity$pred))
plot(ts(EU_Consumer$Total.Quantity), col = "black")
lines(auto_arima_pred_EU_Consumer_Quantity, col = "red")


############## MODEL BUILDING FOR APAC Consumer Sales #############

lm_APAC_Consumer_Sales <- lm(Sales~sin(Month*0.5)+cos(Month*0.6) + Month, data=APAC_Consumer_Sales_in_sm)
summary(lm_APAC_Consumer_Sales)
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      20606.88    1678.39  12.278 8.50e-15 ***
#   sin(Month * 0.5) -5011.81    1204.00  -4.163 0.000174 ***
#   cos(Month * 0.6)  4617.99    1218.74   3.789 0.000525 ***
#   Month              664.93      67.91   9.791 6.12e-12 ***
# Residual standard error: 5324 on 38 degrees of freedom
# Multiple R-squared:  0.7963,	Adjusted R-squared:  0.7802 
# F-statistic:  49.5 on 3 and 38 DF,  p-value: 3.358e-13
global_In_APAC_Consumer_Sales <- predict(lm_APAC_Consumer_Sales,data.frame(Month =c(1:42)))
plot(ts(APAC_Consumer_Sales_in_sm$Sales))
lines(global_In_APAC_Consumer_Sales,col='red')

# LOCAL COMPONENT IDENTIFICATION
local_APAC_Consumer_Sales <- ts_list_indata[[3]] - global_In_APAC_Consumer_Sales
plot(local_APAC_Consumer_Sales)
acf(local_APAC_Consumer_Sales) # q =0
pacf(local_APAC_Consumer_Sales) # p = 0
armafit_APAC_Consumer_Sales <-auto.arima(local_APAC_Consumer_Sales)
tsdiag(armafit_APAC_Consumer_Sales )
armafit_APAC_Consumer_Sales
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 98873820:  log likelihood=-446.19
# AIC=894.38   AICc=894.48   BIC=896.12
# This indicates that local component is stationary

# CONFIRMATION OF STATIONARITY OF LOCAL/RESIDUAL
adf.test(local_APAC_Consumer_Sales,alternative = "stationary")
# p-value = 0.01, hence, series is stationary
kpss.test(local_APAC_Consumer_Sales)
# p-value = 0.1, hence, series is stationary

# FUTURE FORECAST for next 6 months

global_pred_out_APAC_Consumer_Sales <- predict(lm_APAC_Consumer_Sales,data.frame(Month =c(43:48)))

#Now, let's compare our prediction with the actual values, using MAPE

Accuracy_Check_APAC_Consumer_Sales <- accuracy(global_pred_out_APAC_Consumer_Sales,APAC_Consumer$Total.Sales[43:48])[5]
Accuracy_Check_APAC_Consumer_Sales
  # MAPE = 25.83035

# PLOTTING ENTIRE DATASET FOR PREDICTIONS

total_APAC_Consumer_Sales_Predict <- c(ts(global_In_APAC_Consumer_Sales),ts(global_pred_out_APAC_Consumer_Sales))
plot(ts(APAC_Consumer$Total.Sales), col = "black")
lines(total_APAC_Consumer_Sales_Predict, col = "red")

# Build AUTOARIMA Model
arima_APAC_Consumer_Sales <- auto.arima(ts_list_indata[[3]])
arima_APAC_Consumer_Sales
tsdiag(arima_APAC_Consumer_Sales)
plot(arima_APAC_Consumer_Sales$x, col="black")
lines(fitted(arima_APAC_Consumer_Sales), col="red")

#Again, l et's check if the residual series is white noise

resi_auto_arima_APAC_Consumer_Sales <- ts_list_indata[[3]] - fitted(arima_APAC_Consumer_Sales)

adf.test(resi_auto_arima_APAC_Consumer_Sales,alternative = "stationary")
# P value : 0.01
kpss.test(resi_auto_arima_APAC_Consumer_Sales)
# P value : 0.1
# hence, residual series is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima_APAC_Consumer_Sales <- predict(arima_APAC_Consumer_Sales, n.ahead = 6)

MAPE_auto_arima_EU_Consumer_Sales <- accuracy(fcast_auto_arima_APAC_Consumer_Sales$pred,APAC_Consumer$Total.Sales[43:48])[5]
MAPE_auto_arima_EU_Consumer_Sales
# MAPE : 27.68952

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_APAC_Consumer_Sales <- c(fitted(arima_APAC_Consumer_Sales),ts(fcast_auto_arima_APAC_Consumer_Sales$pred))
plot(ts(APAC_Consumer$Total.Sales), col = "black")
lines(auto_arima_pred_APAC_Consumer_Sales, col = "red")


############## MODEL BUILDING FOR APAC Consumer Quantity #############

# build classical decomposition model

lm_APAC_Consumer_Quantity <- lm(Quantity~(poly(Month,2)*cos(Month)+sin(Month)*poly(Month,2)), data=APAC_Consumer_Quantity_in_sm)

summary(lm_APAC_Consumer_Quantity)
global_In_APAC_Consumer_Quantity <- predict(lm_APAC_Consumer_Quantity,data.frame(Month =c(1:42)))
plot(ts(APAC_Consumer_Quantity_in_sm$Quantity))
lines(global_In_APAC_Consumer_Quantity,col='red')
# LOCAL COMPONENT IDENTIFICATION
local_APAC_Consumer_Quantity <- ts_list_indata[[4]] - global_In_APAC_Consumer_Quantity
plot(local_APAC_Consumer_Quantity)

#auto.arima(local_APAC_Consumer_Quantity)

# check for autoregressive ness in the local component
acf(local_APAC_Consumer_Quantity) # found to have q = 0
pacf(local_APAC_Consumer_Quantity) # found to have p = 0
armafit_APAC_Consumer_Quantity <-auto.arima(local_APAC_Consumer_Quantity)
tsdiag(armafit_APAC_Consumer_Quantity )
armafit_APAC_Consumer_Quantity
# ARIMA(0,0,0) with zero mean 
# sigma^2 estimated as 12445:  log likelihood=-257.61
# AIC=517.21   AICc=517.31   BIC=518.95
# Local Component is stationary/residual

adf.test(local_APAC_Consumer_Quantity,alternative = "stationary")
# p-value = 0.04478, hence, stationary
kpss.test(local_APAC_Consumer_Quantity)
# p-value = 0.1, hence, series is stationary

global_pred_out_APAC_Consumer_Quantity <- predict(lm_APAC_Consumer_Quantity,data.frame(Month =c(43:48)))

#Now, let's compare our prediction with the actual values, using MAPE, RMSE & others

Accuracy_Check_APAC_Consumer_Quantity <- accuracy(global_pred_out_APAC_Consumer_Quantity,APAC_Consumer$Total.Quantity[43:48])[5]
Accuracy_Check_APAC_Consumer_Quantity
#MAPE = 35.47013

# PLOTTING ENTIRE DATASET FOR PREDICTIONS

total_APAC_Consumer_Quantity_Predict <- c(ts(global_In_APAC_Consumer_Quantity),ts(global_pred_out_APAC_Consumer_Quantity))
plot(ts(APAC_Consumer$Total.Quantity), col = "black")
lines(total_APAC_Consumer_Quantity_Predict, col = "red")

# Build AUTOARIMA Model
arima_APAC_Consumer_Quantity <- auto.arima(ts_list_indata[[4]])
arima_APAC_Consumer_Quantity  
tsdiag(arima_APAC_Consumer_Quantity)
plot(arima_APAC_Consumer_Quantity$x, col="black")
lines(fitted(arima_APAC_Consumer_Quantity), col="red")

#Again, l et's check if the residual series is white noise

resi_auto_arima_APAC_Consumer_Quantity <- ts_list_indata[[4]] - fitted(arima_APAC_Consumer_Quantity)

adf.test(resi_auto_arima_APAC_Consumer_Quantity,alternative = "stationary")
# P value : 0.01
kpss.test(resi_auto_arima_APAC_Consumer_Quantity)
# P value : 0.1
# hence, residual series is stationary

#Also, let's evaluate the model using MAPE
fcast_auto_arima_APAC_Consumer_Quantity <- predict(arima_APAC_Consumer_Quantity, n.ahead = 6)

MAPE_auto_arima_APAC_Consumer_Quantity <- accuracy(fcast_auto_arima_APAC_Consumer_Quantity$pred,APAC_Consumer$Total.Quantity[43:48])[5]
MAPE_auto_arima_APAC_Consumer_Quantity
# MAPE : 26.24458

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred_APAC_Consumer_Quantity <- c(fitted(arima_APAC_Consumer_Quantity),ts(fcast_auto_arima_APAC_Consumer_Quantity$pred))
plot(ts(APAC_Consumer$Total.Quantity), col = "black")
lines(auto_arima_pred_APAC_Consumer_Quantity, col = "red")


### END ###