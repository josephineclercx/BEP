#Oud
KNMI = KNMI_20191231
X = KNMI[,3]
n = 365
datum = (1:n)
plot(datum,X)

b = 1
theta1 = rep(NA,n)
theta1s = rep(NA,n)
for (i in 1:364){
  b = b + 1
  #Disjunct
  k = floor(n/b) #amount of blocks
  M <- rep(NA,k)
  for (j in 1:k){
    M[j] = max(X[(1+((j-1)*b)):(j*b)])
  }
  Fn <- ecdf(X)
  Fhat = Fn(M)
  Nhat = Fhat
  Yhat = -b*log(Nhat)
  theta1[i] = ((1/k)*sum(Yhat))^(-1)
  
  #Sliding
  Ms <- rep(NA,n-b+1)
  for (r in 1:(n-b+1)){
    Ms[r] = max(X[r:(r+b-1)])
  }
  Fns <- ecdf(X)
  Fhats = Fns(Ms)
  Nhats = Fhats
  Yhats = -b*log(Nhats)
  theta1s[i] = ((1/(n-b+1))*sum(Yhats))^(-1)
}
par(mfrow=c(1,1))
plot(theta1, ylim=c(0,1))
points(theta1s, col='red')

#Meerdere jaren
KNMIjaren = KNMI19jaarbewerkt
XX = matrix(NA, nrow=20, ncol=365)
Xplot = rep(NA, 365)
for (i in 1:20){
  XX[i,] = KNMIjaren[(1+(i-1)*365):(i*365),3]
}
for (i in 1:365){
  Xplot[i] = mean(XX[,i])
}
plot(Xplot, main="Average rainfall of the last 20 years in 0.1 mm", ylab="Rainfall (0.1 mm)", xlab="Day number in a year")
plot(KNMIjaren[,3], main="Rainfall of the last 20 years in 0.1 mm", ylab="Rainfall (0.1 mm)", xlab="Day number starting from 2000-01-01")

#Voor verschillende block lengtes
KNMIjaren = KNMI19jaarbewerkt
n = 7300
y = 500 #max blocklengte
X = KNMIjaren[,3]
b = 1
o = 1 #stepsize
B <- rep(NA,y*o) #different block lengths, 2, 7, 12 etc.
Bs <- rep(NA,y*o) #different block lengths
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
plot(theta1, ylim=c(0,1), main=expression(paste("Estimator ", ~hat(theta)[n]^N, " with different block lengths")), xlab=expression(b[n]), ylab=expression(~ hat(theta)[n]^N))
points(theta1s, ylim=c(0,1), col='red')
legend("bottomright", legend=c(expression("Disjoint blocks", "Sliding blocks")), col=c("black", "red"), pch=21, cex=1)
plot(theta2, ylim=c(0,1), main=expression(paste("Estimator ", ~hat(theta)[n]^B, " with different block lengths")), xlab=expression(b[n]), ylab=expression( ~ hat(theta)[n]^B))
points(theta2s, ylim=c(0,1), col='red')
legend("bottomright", legend=c(expression("Disjoint blocks", "Sliding blocks")), col=c("black", "red"), pch=21, cex=1)

