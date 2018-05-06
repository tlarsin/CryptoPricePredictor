## Load Library
library(stats)
library(ggplot2)
library(astsa)

## Handle Workspace Image 
# save.image("ar.RData")  #save
# load("ar.RData")        #load

############## Section 1 ################
# Implementation of reduced ARIMA(p,d,q=0)
# with autoregression and differencing
# fit, forecast, evaluate
#########################################
# core functions

# fit AR(p)
AR =function(x,p)
{
  t = length(x)
  n = t-p # observation number
  k = p+1 # coefficient number
  df = n # variable number
  
  Y = x[(1+p):t]
  X= matrix(rep(0,n*k),nrow=n,ncol=k)
  for (i in 1:n)
  {
    X[i,1]=1
    for (j in 2:k)
    {
      X[i,j]=x[p+i-j+1] 
    }
  }
  
  # estimated coefficient
  b = solve(t(X)%*%X)%*%t(X)%*%Y
  rownames(b) = c('a',paste("AR", 1:p, sep = ""))
  
  # estimated mean
  mean = b[1]/(1-sum(b[-1]))
  
  # estimated variance
  SSE = t(Y)%*%Y-t(b)%*%t(X)%*%Y
  var_mle = SSE/ n
  
  #fit
  y_fit = as.vector(X%*%b)
  
  # metrics
  AIC = log(var_mle)+ (n+2*k)/n
  AICc = log(var_mle) + (n+k)/(n-k-2)
  BIC = log(var_mle)+ k*log(n)/n
  MSE = SSE/(n-p)
  
  # format output
  estimate = rbind(mean,var_mle)
  rownames(estimate) = c('mean','variance')
  metric = rbind(AIC,AICc,BIC,MSE)
  rownames(metric) = c('AIC','AICc','BIC','MSE')

  list = list(p =p,coeff = b, estimate = estimate,metric = metric,y_fit =y_fit)
  return (list)
}

# differncing of order d
differencing = function(x, d=1)
{
  y = NULL
  for (i in 1:d)
  {
    # first differencing, lose 1 data point
    x.t= x[-length(x)]
    x.tplus= x[-1]
    y = x = x.tplus - x.t
  }
  return (y)
}

# reverse differencing 
# method 1: use original data
undifferencing = function(original,diff)
{
  x=original[1]
  for (i in 1:length(diff))
  {
    x.next =  diff[i] + original[i]
    x = c(x,x.next)
  }
  return (x)
}

# reverse differencing
# method 2: use one point in original data as initial point
undifferencing2 = function(initialPoint,diff)
{
  x= initialPoint
  for (i in 1:length(diff))
  {
    x.next =  diff[i] + x[i]
    x = c(x,x.next)
  }
  return (x)
}

# one-step-ahead forecast for AR(p)
# given past data, forecast the next one
# model - fitted AR(p)
# train - past data
one_forecast = function(model,train)
{
  p= model$p
  x = tail(train, n=p)
  x= rev(x)
  X= c(1,x)
  dim = length(x)
  #X=cbind(rep(1,dim),x)
  b=model$coeff
  Y_pred = X%*%b
  Y_pred= as.vector(Y_pred)
  return (Y_pred)
}

# multiple-step ahead forecast
# using recursive one-step-ahead strategy
# m: steps to forecast
m_forecast=function(obs,fit,m)
{
  pred_list = c()
  data = obs
  for (i in 1:m)
  {
    fore = one_forecast(fit,data)
    #fore = forecast(data,model=fit,h=1)$mean
    pred_list = c(pred_list,fore)
    data= c(data[-1],fore)
  }
  return(pred_list)
}

# one-step-ahead sliding over the train set
oneStepFore = function (data,train.size,test.size,fit,p)
{
  pred_list= c()
  for (i in 0: (test.size-1) )
  {
    train = data[(1+i):(train.size+i)]
    fit1  = AR(train, p)
    pred1=one_forecast(fit,train)
    pred_list = c(pred_list ,pred1)
  }
  return (pred_list)
}

# MAPE: mean absolute percentage error
getMape = function(test,pred)
{
  test.size= length(test)
  error_list = NULL
  for (i in 1:test.size)
  {
    error  = abs(test[i] - pred[i])/test[i]
    error_list = c(error_list,error) 
  }
  mape  = 100/test.size * sum(error_list)
  return (mape)
}

# MSE: mean squared error
getMSE = function(test,pred)
{
  mse = mean((test - pred)^2)
  return (mse)
}

# MAPE + MSE
getAccuracy = function (test,pred)
{
  mse = getMSE(test,pred)
  mape =getMape(test,pred)
  return (list(MSE = mse,MAPE = mape))
}


############## Section 2 ################
# Apply the model to bitcoin data
# In this case, we use ARIMA (p=1,d=1,q=0)
# 0 read data
# 1 preprocessing
# 2 differencing
# 3 fit & forecast (AR)
# 4 undifferencing & evaluate accuracy
# 5 visualization
#########################################


# 0 read data
bitcoin=read.csv("./bitstampUSD_1-min_data_2012-01-01_to_2018-01-08.csv")
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

# daily data
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

############################
# 2 Differencing

x = close
# try differencing, not good -> need to transform
x.d1 = differencing (x); plot(x.d1,type='l')

# log transform data
x.log = log(x);

# plot: log price
plot(time, x.log,  type='l',
     xlab = 'Time', ylab = 'Price (log)',
     main = '2017-2018 Bitcoin Daily Close Price (log)')

# differencing
x.log.d1 = differencing (x.log);

# plot: first differenced log price
plot(x.log.d1,type='l', main = 'First Differenced Data',ylab = 'Return')
abline(h =mean(x.log.d1) )

# acf - before differengcing
acf(x.log,length(x.log))
# acf, pacf - first differencing
acf(x.log.d1,length(x.log.d1),ylim =c(-0.12,0.12))
pacf(x.log.d1,length(x.log.d1))


# residual analysis
sarima(x.log.d1,p=1,d=0,q=0)
#sarima(x.log,p=1,d=1,q=0)

####################################
# 3 Fit Model
# 3.1 split into test and train set 90:10
total.size = length(x.log.d1) #365 total-> train size = 328, test size = 37
train.size = floor(total.size *0.9)
test.size = total.size - train.size
train = x.log.d1[1:train.size]
test = x.log.d1[(train.size+1):total.size]

# 3.2 Use AR model
# train 
fit = AR(train,1)
f0 = fit$y_fit
#one_forecast(fit0,train) #0.005849466

# forecast
# one-step-ahead forecast over whole data
f1 = oneStepFore(x.log.d1, train.size,test.size,fit,p=1)
# m-step-ahead forecast: use train to predict test
f2 = m_forecast(train,fit=fit,m=test.size)

####################################
# 4 Post Processing 
# 4.1 undifferencing
u0 = undifferencing(x.log[2:328],f0)
u1 = undifferencing(x.log[329:365],f1)
u2 = undifferencing2(x.log[329],f2)


# 4.2 evaluation metric
#getMSE(x.log[2:329],u0)
fit$metric['MSE',] #train MSE
getAccuracy(x.log[329:366],u1) 
getAccuracy(x.log[329:366],u2)

####################################
# 5 visualization

# 5.1 plot of differenced result
ts.plot(ts(x.log.d1[329:365],start =329), 
        ts(f1,start =329), 
        ts(f2,start =329),
        col=c("black", "blue","red"),lwd=c(1,2,2),ylab = 'Price')
legend("topleft", legend = c('actual','one-step-ahead','m-step-ahead'), col = c("black", "blue","red"), 
       lty = 1,cex=0.6,text.width=4,lwd=2,y.intersp = 0.7) 

# 5.2 plot of undifferenced result (1:366)
ts.plot(ts(x.log,start =1), 
        ts(u1,start =329), 
        ts(u2,start =329),
        ts(u0,start=3),
        col=c("black",alpha("blue",0.9),alpha("red",0.9),alpha("springgreen4",0.9)),
        lwd=c(1,2,2,2),ylab = 'Price')
legend("topleft", legend = c('actual','one-step-ahead','m-step-ahead','fit'), 
       col=c("black",alpha("blue",0.9),alpha("red",0.9),alpha("springgreen4",0.9)),
       lty = 1,cex=0.6,text.width=25,lwd=2,y.intersp = 0.7) 

# 5.3 plot of undifferenced result (329:366)
ts.plot(ts(x.log[329:366],start =329), 
        ts(u1,start =329), 
        ts(u2,start =329),
        col=c("black", "blue","red"),lwd=c(1,2,2),ylab = 'Price',type='o')
legend("topleft", legend = c('actual','one-step-ahead','m-step-ahead'), col = c("black", "blue","red"), 
       lty = 1,cex=0.6,text.width=4,lwd=2,y.intersp = 0.7) 

