#BEP

#Independent distributions

#IID disitributions

#U(0,1)
X <- ts(runif(5000))
plot(X, main='U(0,1)', type='p')
s = 0.98
abline(h=s, col='red')
for (i in 1:5000){
  if (X[i] < s){
    X[i] = NA
  }
}

plot(X, main="Extreme values of a U(0,1) distribution", ylab='X', type='p')

#N(0,1)
Y <- rnorm(5000)
plot(Y, main='N(0,1)', type='p', ylab='X')
q = 2
abline(h=q, col='red')
for (i in 1:5000){
  if (Y[i] < q){
    Y[i] = NA
  }
}

plot(Y,main="Extreme values of a N(0,1) distribution", ylab='X', type='p')

#exp(1)
W <- rexp(5000)
plot(W, main='Exp(1)', type='p', ylab='X')
m = 4
abline(h=m, col='red')
for (i in 1:5000){
  if (W[i] < m){
    W[i] = NA
  }
}

plot(W,main="Extreme values of an Exp(1) distribution", ylab='X', type='p')

#gamma(2,3)
V <- rgamma(5000, shape=2, rate=3)
plot(V, main='Gamma(2,3)', type='p', ylab='X')
n = 1.8
abline(h=n, col='red')
for (i in 1:5000){
  if (V[i] < n){
    V[i] = NA
  }
}

plot(V,main="Extreme values of a Gamma(2,3) distribution", ylab='X', type='p')

#MA
theta = 2
Z <- arima.sim(list(ma=theta), 5000)
plot(Z, main= expression(paste("MA(1), a=2")), type='p', ylab='X')
p=4.3
abline(h=p, col='red')
for (i in 1:5000){
  if (Z[i] < p){
    Z[i] = NA
  }
}

plot(Z,main="Extreme values of a MA(1) distribution", ylab='X', type='p')

#Dependent distributions

#Moving Maxima
n = 5000
m = 25
U <- runif(n+m)
E = -1/(m*log(U))
X = rep(NA,n)
for (i in 1:n){
  X[i] = max(E[i:(i+m)])
}

plot(X, main='Moving maxima with m=10', type='p', ylab='X')
p = 13
abline(h=p, col='red')

for (i in 1:n){
  if (X[i] < p){
    X[i] = NA
  }
}

plot(X,main="Extremes values of a Moving Maxima distribution", ylab='X', type='p')

#ARCH

n=5000
nn = n + (n/5)
E = rnorm(nn)
XX = rep(NA,nn)
X0 = 0
for (i in 1:nn){
  XX[i] = sqrt((2*10^(-5)+0.7*X0^2))*E[i]
  X0 = XX[i]
}
X = XX[(n/5):nn]
s = 0.02
plot(X, main='ARCH', type='p', ylab='X')
abline(h=s, col='red')

for (i in 1:n){
  if (X[i] < s){
    X[i] = NA
  }
}

plot(X, main="Extremes values of an ARCH distribution", ylab='X', type='p')

#sARCH
n = 5000
nn = n + (n/5)
E <- rnorm(nn)
X0 = 0
s = 0.00035
XX = rep(NA, nn)
for (i in 1:nn){
  XX[i] = (2*10^{-5} + 0.5*X0)*E[i]^2
  X0 = XX[i]
}
X = XX[(n/5):nn]
plot(X, main='sARCH', type='p', ylab='X')
abline(h=s, col='red')

for (i in 1:n){
  if (X[i] < s){
    X[i] = NA
  }
}

plot(X, main="Extreme values of a sARCH distribution", ylab='X', type='p')

#AR-C
n = 5000
z = 0.7
X0 <- rcauchy(1, location = 0, scale = 1)
E <- rcauchy(n, location = 0, scale = (1-abs(z)))
X = rep(NA, n)
for (i in 1:n){
  X[i] = z*X0 + E[i]
  X0 = X[i]
}

plot(X, main='AR-C with z=0.7', type='p', ylab='X')
p = 100
abline(h=p, col='red')

for (i in 1:n){
  if (X[i] < p){
    X[i] = NA
  }
}

plot(X,main="Extreme values of an AR-C distribution", ylab='X', type='p')
