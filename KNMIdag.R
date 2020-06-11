#KNMI windsnelheid per uur voor een maand
KNMIuurwind = KNMI_20200607_hourly.9[,4]
KNMIuurregen = KNMI_20200607_hourly.9[,5]
plot(KNMIuurwind, type='l', main="Highest wind gust per hour", ylab="Windspeed (0.1 m/s)", xlab="Hour starting from 2019-01-10 00:00")
plot(KNMIuurregen, type='l', main="Hour sum of the precipitation", ylab="Percipitation (0.1 mm)", xlab="Hour starting from 2019-01-10 00:00")

n = 744
y = 360 #max blocklengte
X = KNMIuurwind
b = 1
o = 1 #stepsize
B <- rep(NA,y*o) #different block lengths 2, 3, 4 etc.
Bs <- rep(NA,y*o) #different block lengths 2, 3, 4 etc
theta1 = rep(NA,y*o)
theta1s = rep(NA,y*o)
theta2 = rep(NA,y*o)
theta2s = rep(NA,y*o)
for (i in 1:y){
  b = b + o
  #Disjoint
  B[b] = b
  k = floor(n/b) #amount of blocks
  M <- rep(NA,k)
  for (j in 1:k){
    M[j] = max(X[(1+((j-1)*b)):(j*b)])
  }
  Fn <- ecdf(X)
  Fhat = Fn(M)
  Nhat = Fhat
  Yhat = -b*log(Nhat)
  theta1[b] = ((1/k)*sum(Yhat))^(-1)
  Zhat = b*(1-Nhat)
  theta2[b] = ((1/k)*sum(Zhat))^(-1)
  
  #Sliding
  Bs[b] = b
  Ms <- rep(NA,n-b+1)
  for (r in 1:(n-b+1)){
    Ms[r] = max(X[r:(r+b-1)])
  }
  Fns <- ecdf(X)
  Fhats = Fns(Ms)
  Nhats = Fhats
  Yhats = -b*log(Nhats)
  theta1s[b] = ((1/(n-b+1))*sum(Yhats))^(-1)
  Zhats = b*(1-Nhats)
  theta2s[b] = ((1/(n-b+1))*sum(Zhats))^(-1)
}


par(mar=c(5,5,3,1)+.01)
plot(theta1, ylim=c(0,1), main=expression(paste("Estimator ", ~hat(theta)[n]^N, " with different block lengths with precipitation data")), xlab=expression(b[n]), ylab=expression(~ hat(theta)[n]^N))
points(theta1s, ylim=c(0,1), col='red')
legend("bottomright", legend=c(expression("Disjoint blocks", "Sliding blocks")), col=c("black", "red"), pch=21, cex=1)
plot(theta2, ylim=c(0,1), main=expression(paste("Estimator ", ~hat(theta)[n]^B, " with different block lengths with precipitation data")), xlab=expression(b[n]), ylab=expression( ~ hat(theta)[n]^B))
points(theta2s, ylim=c(0,1), col='red')
legend("bottomright", legend=c(expression("Disjoint blocks", "Sliding blocks")), col=c("black", "red"), pch=21, cex=1)

