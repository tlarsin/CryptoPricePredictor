library(astsa)
library(stats)
library(ggplot2)
library(car)
library(dynlm)
library(lubridate)
library(tsa)
#library(quantmod);library(xts);
library(timeSeries);library(forecast);library(tseries);

#save.image("predict2.RData")
#load("predict2.RData")


bitcoin=read.csv("/Users/jinshuning/Documents/Semester6/CS5751/Project/data/bitcoin-historical-data/bitstampUSD_1-min_data_2012-01-01_to_2018-01-08.csv")
dim(bitcoin) #3161057, 8
str(bitcoin)
head(bitcoin)
timestamp = as.POSIXct(bitcoin$Timestamp,origin="1970-01-01")

############################
# 1 Preprocesssing
start = which(timestamp == "2017-01-01 00:00:00 CST") #2625737
end  = which(timestamp == "2018-01-01 00:00:00 CST") #3151337

subset = bitcoin[start:end,]
subset$Timestamp=as.POSIXct(subset$Timestamp,origin="1970-01-01")
dim(subset) #2017-01-01 0:00 to 2018-01-01 0:00 # 525601  8

'''
plot(subset$Timestamp[1:5000],subset$Close[1:5000])
hour = seq(1, 50000 , by=60)
plot(subset$Timestamp[hour],subset$Close[hour])
day = seq(1,525601,by=60*24)
plot(subset$Timestamp[day],subset$Close[day])
'''

# daily data
#index17 
#index12
index = NULL
for (i in 1:dim(subset)[1])
{
  
  len = length(grep("^\\d{4}-\\d{2}-\\d{2}$", as.character(subset$Timestamp[i])))
  if (len ==1)
    index = c(index,i)
}
index
length(index)
subset.day = subset[index,]
dim(subset.day) #366   8
subset.day$Timestamp = as.Date(as.POSIXct(subset.day$Timestamp, origin="1970-01-01"))

# plot
time  = subset.day$Timestamp
close = subset.day$Close
plot(time,close,type='l', xlab = 'Time', ylab = 'Price (USD)',
     main = '2017-2018 Bitcoin Daily Close Price') #  sub = '2017-01-01 to 2018-01-01'

##############################
# 2 Differencing

differencing = function(x)
{
  # first differencing, lose 1 data point
  x.t= x[-length(x)]
  x.tplus= x[-1]
  y = x = x.tplus - x.t
  return (y)
}

# original data
x = subset.day$Close
plot(x,type='l')

x.d1 = differencing (x); plot(x.d1,type='l')
#x.d2 = differencing (x.d1); plot(x.d2,type='l')

# log transform data
x.log = log(x);

plot(time, x.log,  type='l',
     xlab = 'Time', ylab = 'Price (log)',
     main = '2017-2018 Bitcoin Daily Close Price (log)')

x.log.d1 = differencing (x.log);
plot(x.log.d1,type='l', main = 'First Differenced Data',ylab = 'Return')
abline(h =mean(x.log.d1) )
#x.log.d2 = differencing (x.log.d1);plot(x.log.d2,type='l')
#acf(x.log.d2,length(x.log.d2))

acf(x.log,length(x.log))
acf(x.log.d1,length(x.log.d1),ylim =c(-0.12,0.12))
pacf(x.log.d1,length(x.log.d1))

print(adf.test(x.log.d1))




# acf and pacf tails off, thus should use ARMA(p,q)
# conclude d=1, how to choose p, q?

############################################
# 3 ARIMA estimation
#sarima(x.log, p=1, d=1, q=1 )
sarima(x.log.d1, p=1, d=0, q=1)
sarima.for(x.log.d1,n.ahead=10,p=1, d=0, q=1)

sarima(x.log.d1, p=1, d=0, q=1)

####################################
# 4 Prediction
# 4.1 one-step-ahead
# 4.2 long-range: m-step-ahead

#split into test and train set 90:10
total.size = length(x.log.d1) #365 total-> train size = 328 floor, test size = 37
train.size = floor(total.size *0.9)
test.size = total.size - train.size
train = x.log.d1[1:train.size]
test = x.log.d1[(train.size+1):total.size]

fit  = arima(train, order = c(1,0,1))
accuracy(fit)
fore  = predict(fit, n.ahead = test.size)
pred = fore$pred

error_list = NULL
for (i in 1:test.size)
{
  error  = abs(test[i] - pred[i])/test[i]
  error_list = c(error_list,error) 
}
mape  = 100/test.size * sum(error_list)
mape #23.40647

mse = mean((test - pred)^2)
mse


'''
accuracy(fit)

accuracy(arima(ts(x.log.d1), order = c(1,0,0)))

ts.plot(ts(x.log.d1), fore$pred)
U = fore$pred+fore$se; L = fore$pred-fore$se
xx = c(time(U), rev(time(U))); yy = c(L, rev(U))
polygon(xx, yy, border = 8, col = gray(.6, alpha = .2))
lines(fore$pred, type="p", col=2)
'''
