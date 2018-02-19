####Final Project
#Gather Data
Loans = Final.Project.Data
t = time(X1)
#Plot Time Series of Loans
X1 = ts(Loans$Loans.Billions, start = 1975, frequency = 12)
plot(X1,col="orange",lwd=4, ylab = "Billions of US Dollars", xlab = "Time (Years)", main="Real Estate Loans From All Commercial Banks")
par(mfrow=c(1,1))    
#ACF/ PACF Plots for Loans Dataset
plot(X1,main="Time Series")
acf(X1,main="ACF Model")
pacf(X1,main="PACF Model")

#Plot Time Series of Housing Price Index
X2 = ts(Loans$HPI.NSA, start = 1975, frequency = 12)
plot(X2,col="red",lwd=4, ylab = "HPI (Dec 2000 = 100)",  xlab = "Time (Years)", main="Freddie Mac Housing Pricing Index (1975-2016)")
par(mfrow=c(1,1))    
#ACF/ PACF Plots for HPI
plot(X2,main="Time Series")
acf(X2,main="ACF Model")
pacf(X2,main="PACF Model")
########
##Checking to see if seasonality is present in our data set
x=X1- mean(X1)
n=length(x)
I=abs(fft(x))^2/n
half=floor((n/2)-1)
f=0:half/n
P=(4/n)*I[1:(n/2)]
index=which(P==max(P))
w=1/f[index]
f[index]
w

par(mfrow=c(1,1))
plot(f,P,type="l",main="Periodogram for Commercial Bank Loans Data",xlab="Frequency",ylab="P(frequency)",col="blue")

#Comment: The periodocity in our Real Estate Loans data set was equal to its n.
#this means that seasonality is not evident in our data set.

###Real Estate Loans fitting
##Trend Fitting
# linear
fit1 = lm(X1~t)
summary(fit1)
# squared
tsq = t^2/factorial(2)
fit2 = lm(X1~t+tsq)
summary(fit2)
# Fourth Power
tfp = t^4/factorial(4)
fit3 = lm(X1~t+tfp)
summary(fit3)
#All
fit4= lm(X1~t +tsq+tfp)
summary(fit4)

##Plotting fits
# linear fit
plot(X1, type = "o", xlab = "Time (years)", ylab = "Loans (Billions)", main = "Real Estate Loans")
par(new = TRUE)
plot(fit1$fitted, col = "blue", lwd = 4, type = "l", ylab="",xlab="",main="",xaxt="n",yaxt="n", lty=1)
# quadratic fit squared
par(new = TRUE)
plot(fit2$fitted, col = "red", lwd = 3, type = "l", ylab="",xlab="",main="",xaxt="n",yaxt="n", lty=1)
# quadratic fit Fourth Power
par(new = TRUE)
plot(fit3$fitted, col = "green", lwd = 2, type = "l", ylab="",xlab="",main="",xaxt="n",yaxt="n", lty=1)
#t, tsq, tcb fit
par(new=TRUE)
plot(fit4$fitted, col = "turquoise", lwd = 2, type = "l", ylab="",xlab="",main="",xaxt="n",yaxt="n", lty=1)
legend("topleft", c("Linear","Squared","Fourth Power","Combined Trends"), col = c("blue","red","green","turquoise"), lwd = c(4,3,2,1), lty = c(1,1,2,1))

#Comment: the combined trends fit was our best fit for our data. It was evident
#by its high R^2adjusted (.9188)

par(mfrow = c(2,2))
plot(fit4, main = "Original - Real Estate Loans Diagnostics", which = 1:4)

#comment: Clear we need to transform this data set

##Plotting fits for HPI data

# linear
fit1b = lm(X2~t)
summary(fit1b)
# squared
fit2b = lm(X2~t+tsq)
summary(fit2b)
# Fourth Power
fit3b = lm(X2~t+tfp)
summary(fit3b)
#Combined Trends
fit4b=lm(X2~t+tsq+tfp)
summary(fit4b)

##determining if seasonality is in the data
x2=X2- mean(X2)
n2=length(x2)
I2=abs(fft(x2))^2/n2
half=floor((n2/2)-1)
f2=0:half/n2
P2=(4/n2)*I2[1:(n2/2)]
index=which(P2==max(P2))
w2=1/f2[index]
f2[index]
w2

par(mfrow=c(1,1))
plot(f2,P2,type="l",main="Periodogram for Freddie Mac Housing Price Index Data",xlab="Frequency",ylab="P(frequency)",col="blue")

#Comment: The periodocity in our Freddie Mac Housing Price data set was equal to its
#n.this means that seasonality is not evident in our data set.


# linear fit
plot(X2, type = "o", xlab = "Time (years)", ylab = "Index", main = "Housing Price Index")
par(new = TRUE)
plot(fit1b$fitted, col = "blue", lwd = 4, type = "l", ylab="",xlab="",main="",xaxt="n",yaxt="n", lty=1)
# quadratic fit squared
par(new = TRUE)
plot(fit2b$fitted, col = "red", lwd = 3, type = "l", ylab="",xlab="",main="",xaxt="n",yaxt="n", lty=1)
# quadratic fit Fourth Power
par(new = TRUE)
plot(fit3b$fitted, col = "green", lwd = 2, type = "l", ylab="",xlab="",main="",xaxt="n",yaxt="n", lty=1)
# Combined Trend fit
par(new = TRUE)
plot(fit4b$fitted, col = "orange", lwd = 1, type = "l", ylab="",xlab="",main="",xaxt="n",yaxt="n", lty=1)
legend("topleft", c("Linear","Squared","Fourth Power","Combined Trend"), col = c("blue","red","green","orange"), lwd = c(4,3,2,1), lty = c(1,1,2,1))

# fit 4b had highest R-squared A

par(mfrow = c(2,2))
plot(fit2b, main = "Original Housing Price Index Diagnostics", which = 1:4)

#Comment:Clear from diagnostics we need a transformation

#####Transformations
###Real Estate

Y1 = log(X1)

# transformed linear
fit1c = lm(Y1~t)
summary(fit1c)
# transformed squared
fit2c = lm(Y1~t+tsq)
summary(fit2c)
# transformed Fourth Power
fit3c = lm(Y1~t+tfp)
summary(fit3c)
# transformed Seasonality
fit4c = lm(Y1 ~ t + a1 + a2)
summary(fit4c)

# Fit 2c is still best

par(mfrow = c(2,2))
plot(fit2c, main = "Transformed Real Estate Loans Diagnostics", which = 1:4)

# HPI - transformed

Y2 = log(X2)

# linear
fit1d = lm(Y2~t)
summary(fit1d)
# squared
fit2d = lm(Y2~t+tsq)
summary(fit2d)
# Fourth Power
fit3d = lm(Y2~t+tfp)
summary(fit3d)
# Seasonality
fit4d = lm(Y2 ~ t + a1 + a2)
summary(fit4d)

# fit 3d is now the best

par(mfrow = c(2,2))
plot(fit3d, main = "Transformed Housing Price Index Diagnostics", which = 1:4)













##################
X1 = ts(Loans$Loans.Billions., start = 1947, frequency = 12)
Y1 = log(X1)
plot(X1, ylab = "Loans (billions)", xlab = "Time (years)", main = "Housing Loans - Original")
plot(Y1, ylab = "Loans (billions)", xlab = "Time (years)", main = "Housing Loans - Log Transformation")
t = time(X1)

# Linear
fit1 = lm(X1~t)
summary(fit1)
abline(fit1, col = "orange", lwd = 3)
# Quadratic
tsq = t^2/factorial(2)
fit2 = lm(X1~t+tsq)
# TRANSFORMED FIT***
fit2b = lm(Y1~t+tsq)
summary(fit2)
# Seasonality
a1 = cos(2*pi*t)
a2 = sin(2*pi*t)
fit3 = lm(X1 ~ t + a1 + a2)
summary(fit3)

# linear fit
plot(X1, type = "o", xlab = "Time (year)", ylab = "Loans (Billions)", main = "Real Estate Loans")
par(new = TRUE)
plot(fit1$fitted, col = "blue", lwd = 2, type = "l", main = "", xlab = "", ylab = "")

# quadratic fit
par(new = TRUE)
plot(fit2$fitted, col = "red", lwd = 2, type = "l", main = "", xlab = "", ylab = "")
legend("topleft", c("Linear","Quadratic"), col = c("blue","red"), lty = c(1,1), lwd = c(3,3))

# Residuals Plot
plot.ts(fit2$resid, type = "o", main = "Fit 2 Detrend Series", ylab = "Residuals", xlab = "Fitted Values")
plot.ts(fit2b$resid, type = "o", main = "Fit 2 Detrend Series", ylab = "Residuals", xlab = "Fitted Values")

# Fit Diagnostics
par(mfrow = c(2,2))
plot(fit1, main = "Fit 1 Diagnostics", which = 1:4)
plot(fit2, main = "Fit 2 Diagnostics", which = 1:4)
plot(fit3, main = "Fit 3 Diagnostics", which = 1:4)
# tranformed
plot(fit2b, main = "Fit 2b Diagnostics", which = 1:4)

par(mfrow = c(2,2))
plot.ts(X1, main = "Real Estate Loans", xlab = "time (years)", ylab = "Loans (Billions)")
# smoothing MA3
X1.MA3 = filter(X1, sides = 2, rep(1,3)/3)
plot.ts(X1.MA3, main = "3-Point MA Filter", xlab = "time (years)", ylab = "Loans (Billions)")
# MA5
X1.MA5 = filter(X1, sides = 2, rep(1,5)/5)
plot.ts(X1.MA5, main = "5-Point MA Filter", xlab = "time (years)", ylab = "Loans (Billions)")
# MA9
X1.MA9 = filter(X1, sides = 2, rep(1,9)/9)
plot.ts(X1.MA9, main = "9-Point MA Filter", xlab = "time (years)", ylab = "Loans (Billions)")

# 9 point detrended

par(mfrow = c(2,2))
d9 = X1-X1.MA9
plot(d9, main = "Detrended MA9", ylab = "residuals")

#####gaetzys code
Loans = Final.Project.Data
X1 = ts(Loans$Loans.Billions, start = 1975, frequency = 12)
X2 = ts(Loans$HPI.NSA, start = 1975, frequency = 12)
Y1 = log(X1)
Y2 = log(X2)
plot(X2, ylab = "HPI", xlab = "Time (years)", main = "Housing Price Index - Original")
plot(Y2, ylab = "HPI", xlab = "Time (years)", main = "Housing Price Index - Log Transformation")
plot(X1, ylab = "Loans (billions)", xlab = "Time (years)", main = "Housing Loans - Original")
plot(Y1, ylab = "Loans (billions)", xlab = "Time (years)", main = "Housing Loans - Log Transformation")

plot(Y1, ylab = "Loans (billions)", xlab = "Time (years)", main = "Housing Loans - Log Transformation")
lines(Y2, ylab = "HPI", xlab = "Time (years)", main = "Housing Price Index - Log Transformation")

# Quadratic Original
t = time(X1)
tsq = t^2/factorial(2)
fit2 = lm(X1~t+tsq)
summary(fit2)
# TRANSFORMED FIT Loans.Billions
fit2b = lm(Y1~t+tsq)
summary(fit2b)
# TRANSFORMED FIT2 HPI.NSA
fit2c = lm(Y2~t+tsq)
summary(fit2c)


