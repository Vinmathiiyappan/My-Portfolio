## USE FORECAST AND ZOO LIBRARIES.

library(forecast)
library(zoo)
#-------------------------------------------------------------------------------
# Set working directory for locating files.
setwd("/Users/vinmathi/Documents/Ban 673")

# Create data frame.
Sales.data <- read.csv("673_case1.csv")
# See the first 6 records of the file.
tail(Sales.data)
Sales.data$Sales
#-------------------------------------------------------------------------------
## CREATE TIME SERIES DATA SET.

# Function ts() takes three arguments: start, end, and freq.
# With monthly data, frequency (freq) of periods in a season (year) is 12. 
# With quarterly data, frequency in a season (year) is equal to 4.
# Arguments start and end are pairs: (season number, period number).
monsales.ts <- ts(Sales.data$Sales, 
                   start = c(2015, 1), end = c(2021, 12), freq = 12)
head(monsales.ts)
#-------------------------------------------------------------------------------
# Use plot() to plot time series data  
plot(monsales.ts, 
     xlab = "Time", ylab = "Sales (in millions)", 
     ylim = c(100, 500), xaxt = 'n',
     main = "Monthly sales at Grocery store")

# Establish x-axis scale interval for time in months.
axis(1, at = seq(2015, 2021, 1), labels = format(seq(2015, 2021, 1)))
#-------------------------------------------------------------------------------
# Use tslm() function to create linear trend  and 
# quadratic trend  for time series data. 
sales.lin <- tslm(monsales.ts ~ trend)
sales.quad <- tslm(monsales.ts ~ trend + I(trend^2))

# Use plot() function to create plot with linear trend. 
plot(monsales.ts, 
     xlab = "Time", ylab = "Sales  in Millions",
#     ylim = c (1300, 3000),
     xaxt = 'n',
     main = "Sales with Linear Trend")
lines(sales.lin$fitted, lwd = 2, col = "blue")
axis(1, at = seq(2015, 2021, 1), labels = format(seq(2015, 2021, 1)))

# Use plot() function to create plot with quadratic trend. 
plot(monsales.ts, 
     xlab = "Time", ylab = "Sales  in Millions",
#     ylim = c (1300, 3000), 
      xaxt = 'n',
     main = "Sales with Quadratic Trend")
lines(sales.quad$fitted, lwd = 2, col = "blue")
axis(1, at = seq(2015, 2021, 1), labels = format(seq(2015, 2021, 1)))

#-------------------------------------------------------------------------------
# Use stl() function to plot times series components of the original data. 
# The plot includes original data, trend, seasonal, and reminder 
# (level and noise component).
monsales.stl <- stl(monsales.ts, s.window = "periodic")
autoplot(monsales.stl, main = "Sales Time Series Components")

# Use Acf() function to identify autocorrelation and plot autocorrelation
# for different lags.
autocor <- Acf(monsales.ts, lag.max = 12, 
               main = "Autocorrelation for Sales")

# Display autocorrelation coefficients for various lags.
Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

#-------------------------------------------------------------------------------
## CREATE DATA PARTITION.

# Define the numbers of months in the training and validation sets,
# nTrain and nValid, respectively.
nValid <- 24
nTrain <- length(monsales.ts) - nValid
train.ts <- window(monsales.ts, start = c(2015, 1), end = c(2015, nTrain))
valid.ts <- window(monsales.ts, start = c(2015, nTrain + 1), 
                   end = c(2015, nTrain + nValid))
train.ts
valid.ts
#-------------------------------------------------------------------------------
# In rollmean(), use argument align = "right" to calculate a trailing MA.
ma.trailing_2 <- rollmean(train.ts, k = 2, align = "right")
ma.trailing_6 <- rollmean(train.ts, k = 6, align = "right")
ma.trailing_12 <- rollmean(train.ts, k = 12, align = "right")

# Use head() function to show training MA (windows width k=12 
# for the first 6 MA results and tail() function to show the 
# last 6 MA results for MA. 
head(ma.trailing_12)
tail(ma.trailing_12)
#-------------------------------------------------------------------------------
# Create forecast for the validation data for the window widths 
# of k = 4, 5, and 12. 
ma.trail_2.pred <- forecast(ma.trailing_2, h = nValid, level = 0)
ma.trail_2.pred
ma.trail_6.pred <- forecast(ma.trailing_6, h = nValid, level = 0)
ma.trail_6.pred
ma.trail_12.pred <- forecast(ma.trailing_12, h = nValid, level = 0)
ma.trail_12.pred
#-------------------------------------------------------------------------------
# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(ma.trail_2.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_6.pred$mean, valid.ts), 3)
round(accuracy(ma.trail_12.pred$mean, valid.ts), 3)
#-------------------------------------------------------------------------------

# Fit a regression model with linear trend and seasonality for
# training partition. 
trend.seas <- tslm(train.ts ~ trend + season)
summary(trend.seas)
#-------------------------------------------------------------------------------
# Identify and display regression residuals for training
# partition (differences between actual and regression values 
# in the same periods).
trend.seas.res <- trend.seas$residuals
trend.seas.res
# Apply trailing MA for residuals with window width k = 2
# for training partition.
ma.trail.res <- rollmean(trend.seas.res, k = 2, align = "right")
ma.trail.res
## FORECAST USING REGRESSION AND TRAILING MA FOR VALIDATION PERIOD.
# Create regression forecast with trend and seasonality for 
# validation period.
trend.seas.pred <- forecast(trend.seas, h = nValid, level = 0)
trend.seas.pred
# Regression residuals in validation period.
trend.seas.res.valid <- valid.ts - trend.seas.pred$mean
trend.seas.res.valid

# Create residuals forecast for validation period.
ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

#-------------------------------------------------------------------------------
# Develop two-level forecast for validation period by combining  
# regression forecast and trailing MA forecast for residuals.
fst.2level <- trend.seas.pred$mean + ma.trail.res.pred$mean
fst.2level

# Create a table for validation period: validation data, regression 
# forecast, trailing MA for residuals and total forecast.
valid.df <- round(data.frame(valid.ts, trend.seas.pred$mean, 
                             ma.trail.res.pred$mean, 
                             fst.2level), 3)
names(valid.df) <- c("Sales", "Regression.Fst", 
                     "MA.Residuals.Fst", "Combined.Fst")
valid.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy(trend.seas.pred$mean, valid.ts), 3)
round(accuracy(fst.2level, valid.ts), 3)

#-------------------------------------------------------------------------------
## USE REGRESSION AND TRAILING MA FORECASTS FOR ENTIRE DATA SET. 
## USE 2-LEVEL (COMBINED) FORECAST TO FORECAST 12 FUTURE PERIODS.
## MEASURE ACCURACY OF REGRESSION AND 2-LEVEL FORECASTS FOR
## ENTIRE DATA SET.

# Fit a regression model with linear trend and seasonality for
# entire data set.
tot.trend.seas <- tslm(monsales.ts ~ trend  + season)
summary(tot.trend.seas)

# Create regression forecast for future 12 periods.
tot.trend.seas.pred <- forecast(tot.trend.seas, h = 12, level = 0)
tot.trend.seas.pred

# Identify and display regression residuals for entire data set.
tot.trend.seas.res <- tot.trend.seas$residuals
tot.trend.seas.res

# Use trailing MA to forecast residuals for entire data set.
tot.ma.trail.res <- rollmean(tot.trend.seas.res, k = 2, align = "right")
tot.ma.trail.res

# Create forecast for trailing MA residuals for future 12 periods.
tot.ma.trail.res.pred <- forecast(tot.ma.trail.res, h = 12, level = 0)
tot.ma.trail.res.pred

# Develop 2-level forecast for future 12 periods by combining 
# regression forecast and trailing MA for residuals for future
# 12 periods.
tot.fst.2level <- tot.trend.seas.pred$mean + tot.ma.trail.res.pred$mean
tot.fst.2level

# Create a table with regression forecast, trailing MA for residuals,
# and total forecast for future 12 periods.
future12.df <- round(data.frame(tot.trend.seas.pred$mean, tot.ma.trail.res.pred$mean, 
                                tot.fst.2level), 3)
names(future12.df) <- c("Regression.Fst", "MA.Residuals.Fst", "Combined.Fst")
future12.df

# Use accuracy() function to identify common accuracy measures.
# Use round() function to round accuracy measures to three decimal digits.
round(accuracy((snaive(monsales.ts))$fitted, monsales.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted, monsales.ts), 3)
round(accuracy(tot.trend.seas.pred$fitted+tot.ma.trail.res, monsales.ts), 3)


#-------------------------------------------------------------------------------
## HOLT-WINTER'S (HW) EXPONENTIAL SMOOTHING WITH PARTITIONED DATA, AUTOMATIC
## ERROR, TREND and SEASONALITY (ZZZ) OPTIONS, AND OPTIMAL PARAMETERS
## ALPHA, BETA, AND GAMMA.
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ # (A,N,A)
# Use forecast() function to make predictions using this HW model with 
# validation period (nValid). 
# Show predictions in tabular format.
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

#-------------------------------------------------------------------------------
## FORECAST WITH HOLT-WINTER'S MODEL USING ENTIRE DATA SET INTO
## THE FUTURE FOR 12 PERIODS.

# Create Holt-Winter's (HW) exponential smoothing for full Sales data set. 
# Use ets() function with model = "ZZZ", to identify the best HW option
# and optimal alpha, beta, & gamma to fit HW for the entire data period.
HW.ZZZ <- ets(monsales.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred
#-------------------------------------------------------------------------------
# Identify performance measures for HW forecast.
round(accuracy((snaive(monsales.ts))$fitted, monsales.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, monsales.ts), 3)

