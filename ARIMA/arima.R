library(astsa)
library(stats)
library(ggplot2)
library(car)
library(dynlm)


# Auto-regressive Model, AR(p)
# autoregression order: p>=1
# method = 'ols' ordinary least square
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
  
  # metrics
  AIC = log(var_mle)+ (n+2*k)/n
  AICc = log(var_mle) + (n+k)/(n-k-2)
  BIC = log(var_mle)+ k*log(n)/n
  MSE = SSE/(n-p)
  
  # format output
  estimate = rbind(b,mean,var_mle)
  rownames(estimate) = c(rownames(b),'mean','variance')
  metric = rbind(AIC,AICc,BIC,MSE)
  rownames(metric) = c('AIC','AICc','BIC','MSE')
  
  list = list(estimate = estimate,metric = metric)
  return (list)
}


# Test
# imbedded dataset 'gnp'
gnpgr = diff(log(gnp)) 
# test our function: AR(x,p)
AR(x= gnpgr,p=1)
# compare with library function: sarima(x,p,0,0)
sarima(gnpgr, 1, 0, 0) 

