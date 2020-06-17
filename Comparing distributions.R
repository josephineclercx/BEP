#Compare MA

#Distributions
n = 5000
theta = 2
X1 <- arima.sim(list(ma=theta), n, sd=sqrt(10))
X2 <- rnorm(n, mean=0, sd=sqrt((1+theta^2)*10))
par(mfrow=c(1,1))
plot(X1, type='p', main="MA(1) and N(0,5) distribution", ylab='X')
points(X2, col='red')
legend("topleft", legend=c(expression("MA(1)", "N(0,5)")), col=c("black", "red"), pch=21, cex=1)

#Maxima
theta = 2
M1 = rep(NA, 100)
M2 = rep(NA, 100)
XX1 = rep(NA, n*100)
XX2 = rep(NA, n*100)
for (i in 1:100){
  X1 <- arima.sim(list(ma=theta), n)
  X2 <- rnorm(n, mean=0, sd=sqrt(1+theta^2))
  M1[i] = max(X1) 
  M2[i] = max(X2)
  XX1[(1+(i-1)*n):(i*n)] = X1
  XX2[(1+(i-1)*n):(i*n)] = X2
}
plot(M1, main="Maximum values of MA(1) and N(0,5) distribution", ylab=expression(M[n]))
points(M2, col='red')
legend("topleft", legend=c(expression("MA(1)", "N(0,5)")), col=c("black", "red"), pch=21, cex=1)


#F(maxima)
Fn1 <- ecdf(XX1)
Fn2 <- ecdf(XX2)
plot(Fn1(M1), main="Emperical cdf of the maximum values", ylab="Emperical cdf")
points(Fn2(M2), col='red')
legend("bottomright", legend=c(expression("MA(1)", "N(0,5)")), col=c("black", "red"), pch=21, cex=1)
hist(Fn1(M1),  prob=TRUE, breaks=10, ylim = c(0,5000), col=rgb(1,0,0,1/2), border=F, main=expression(paste("Density plot of ", F[n](M[n]))), xlab=expression(F[n](M[n])))
hist(Fn2(M2), prob=TRUE, add=T, col=rgb(0,0,1,1/2), border=F)
legend("topleft", legend=c(expression("MA(1)", "N(0,5)")), col=c(rgb(1,0,0,1/2),rgb(0,0,1,1/2)), lwd=10)


#Compare Moving Maxima

#Distributions
n = 5000
m = 10
E1 = rfrechet(n+m, shape=1, loc=0, scale=(1/m))
Z1 = rep(NA,n)
for (i in 1:n){
  Z1[i] = max(E1[i:(i+m)])
}
Z2 <- rfrechet(n, shape=1, loc=0, scale=(1/m))
plot(Z1, main="Moving Maxima and Std. Fr\u{E9}chet distribution", ylab='Y')
points(Z2, col = 'red')
legend("topleft", legend=c(expression("Moving Maxima", "Std. Frechet")), col=c("black", "red"), pch=21, cex=1)

#Maxima
M1 = rep(NA, 100)
M2 = rep(NA, 100)
ZZ1 = rep(NA, n*100)
ZZ2 = rep(NA, n*100)
for (i in 1:100){
  E1 = rfrechet(n+m, shape=1, loc=0, scale=(1/m))
  Z1 = rep(NA,n)
  for (j in 1:n){
    Z1[j] = max(E1[j:(j+m)])
  }
  Z2 <- rfrechet(n, shape=1, loc=0, scale=1/m)
  M1[i] = max(Z1) 
  M2[i] = max(Z2)
  ZZ1[(1+(i-1)*n):(i*n)] = Z1
  ZZ2[(1+(i-1)*n):(i*n)] = Z2
}
plot(log(M1), main="Maximum values of Moving Maxima and Std. Fr\u{E9}chet distribution", ylab=expression(log(M[n])))
points(log(M2), col='red')
legend("topleft", legend=c(expression("Moving Maxima", "Std. Frechet")), col=c("black", "red"), pch=21, cex=1)

#F(maxima)
Fn1 <- ecdf(ZZ1)
Fn2 <- ecdf(ZZ2)
plot(Fn1(M1), main="Emperical di of the maximum values", ylab="Emperical cdf")
points(Fn2(M2), col='red')
legend("bottomright", legend=c(expression("Moving Maxima", "Std. Frechet")), col=c("black", "red"), pch=21, cex=1)
hist(Fn1(M1), prob=TRUE, breaks=100, ylim=c(0,4500), col=rgb(1,0,0,1/2), border=F, main=expression(paste("Density plot of ", F[n](M[n]))), xlab=expression(F[n](M[n])))
hist(Fn2(M2),  prob=TRUE, add=T, col=rgb(0,0,1,1/2), border=F)
legend("topleft", legend=c(expression("Moving Maxima", "Std.Frechet")), col=c(rgb(1,0,0,1/2),rgb(0,0,1,1/2)), lwd=10)


#Compare AR-C

#Distributions
n = 5000
z = 0.7
X01 <- rcauchy(1, location = 0, scale = 1)
Y2 <- rcauchy(n, location=0, scale=(1-abs(z)))
E1 <- rcauchy(n, location = 0, scale = (1-abs(z)))
Y1 = rep(NA, n)
for (i in 1:n){
  Y1[i] = z*X01 + E1[i]
  X01 = Y1[i]
}

plot(Y1, main="AR-C and Std. Cauchy distribution", ylab='Z')
points(Y2, col = 'red')
legend("topleft", legend=c(expression("AR-C", "Std. Cauchy")), col=c("black", "red"), pch=21, cex=1)


M1 = rep(NA, 100)
M2 = rep(NA, 100)
YY1 = rep(NA, n*100)
YY2 = rep(NA, n*100)
for (i in 1:100){
  X01 <- rcauchy(1, location = 0, scale = 1)
  E1 <- rcauchy(n, location = 0, scale = (1-abs(z)))
  Y1 = rep(NA, n)
  for (j in 1:n){
    Y1[j] = z*X01 + E1[j]
    X01 = Y1[j]
  }
  Y2 <- rcauchy(n, location = 0, scale = (1-abs(z)))
  M1[i] = max(Y1) 
  M2[i] = max(Y2)
  YY1[(1+(i-1)*n):(i*n)] = Y1
  YY2[(1+(i-1)*n):(i*n)] = Y2
}
plot(log(M1), main="Maximum values of AR-C and Std. Cauchy distribution", ylab=expression(log(M[n])))
points(log(M2), col='red')
legend("topleft", legend=c(expression("AR-C", "Std. Cauchy")), col=c("black", "red"), pch=21, cex=1)


#F(maxima)
Fn1 <- ecdf(YY1)
Fn2 <- ecdf(YY2)
plot(Fn1(M1), main="Emperical cdf of the maximum values", ylab="Emperical cdf")
points(Fn2(M2), col='red')
legend("bottomright", legend=c(expression("AR-C", "Std. Cauchy")), col=c("black", "red"), pch=21, cex=1)
hist(Fn1(M1), prob=TRUE, breaks = 20, ylim=c(0,4500), col=rgb(1,0,0,1/2), border=F, main=expression(paste("Density plot of ", F[n](M[n]))), xlab=expression(F[n](M[n])))
hist(Fn2(M2), prob=TRUE, add=T, col=rgb(0,0,1,1/2), border=F)
legend("topleft", legend=c(expression("AR-C", "Std. Cauchy")), col=c(rgb(1,0,0,1/2),rgb(0,0,1,1/2)), lwd=10)
