y<-scan("C:/Users/11514/Downloads/employ.txt")
head(y)
ts.plot(y,xlim=c(1,length(y)+10),ylim = c(20000,120000),main="original data",ylab="employment")
acf(y,lag=80)
pacf(y,lag=80)
var(y)

require(MASS)
bcTransform <- boxcox(y ~ as.numeric(1:length(y))) 
bcTransform$x[which(bcTransform$y == max(bcTransform$y))] 

y.tr<-y^0.5
ts.plot(y.tr)
var(y.tr)
acf(y.tr)
pacf(y.tr)


ydiff2<-diff(y.tr,differences = 1)
ts.plot(ydiff2)
var(ydiff2)
acf(ydiff2,lag=60)
pacf(ydiff2,lag=60)

library(qpcR)
# Calculate AICc for ARMA models with p and q running from 0 to 5
aiccs <- matrix(NA, nr = 10, nc = 10)
dimnames(aiccs) = list(p=0:9, q=0:9)
for(p in 0:9)
{
  for(q in 0:9)
  {
    aiccs[p+1,q+1] = AICc(arima(y.tr, order = c(p,1,q), method="ML"))
  }
}
aiccs
(aiccs==min(aiccs))

fit1=arima(y.tr,order=c(1,1,0),method='ML',xreg=1 : length(y.tr))
fit1
Box.test(residuals(fit1),type="Ljung-Box")
Box.test(residuals(fit1),type="Box-Pierce")
Box.test((residuals(fit1))^2,type="Ljung-Box")
shapiro.test(residuals(fit1))
ts.plot(residuals(fit1),main='fitted value')
acf(residuals(fit1))
pacf(residuals(fit1))
hist(residuals(fit1),main = "Histogram")
qqnorm(residuals(fit1))
qqline(residuals(fit1),col ="blue")


fit2=arima(y.tr,order=c(0,1,1),method='ML',xreg=1 : length(y.tr))
fit2
Box.test(residuals(fit2),type="Ljung-Box")
Box.test(residuals(fit2),type="Box-Pierce")
Box.test((residuals(fit2))^2,type="Ljung-Box")
shapiro.test(residuals(fit2))
ts.plot(residuals(fit2),main='fitted value')
acf(residuals(fit2),lag=60)
pacf(residuals(fit2),lag=60)
# Histogram
hist(residuals(fit2),main = "Histogram")
# q-q plot
qqnorm(residuals(fit2))
qqline(residuals(fit2),col ="blue")


pred.tr <- predict(fit2, n.ahead = 10, newxreg=(length(y.tr)+1) : (length(y.tr)+10))
U.tr= pred.tr$pred + 2*pred.tr$se # upper bound for the C.I. for transformed data
L.tr= pred.tr$pred - 2*pred.tr$se # lower bound
ts.plot(y.tr, xlim=c(1,length(y.tr)+10), ylim = c(140,max(U.tr))) #plot y.tr and forecast
lines(U.tr, col="blue", lty="dashed")
lines(L.tr, col="blue", lty="dashed")
points((length(y.tr)+1):(length(y.tr)+10), pred.tr$pred, col="red")

pred.orig <- pred.tr$pred^2 # back-transform to get predictions of original time series
U= U.tr^2 # bounds of the confidence intervals
L=L.tr^2
# Plot forecasts with original data
ts.plot(y, xlim=c(1,length(y)+10), ylim = c(20000,120000))
lines(U, col="blue", lty="dashed")
lines(L, col="blue", lty="dashed")
points((length(y)+1):(length(y)+10), pred.orig, col="red")


fit2
#final model: (1-B)Xt^0.5=(1+0.4133B)Zt