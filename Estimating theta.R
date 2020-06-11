#Estimating theta

n = 5000
#Optimal block lengths
b1 = 
b1s = 
b2 = 
b2s = 
est =  #true value of theta
q = 100 #times of estimate theta
theta1 <- rep(NA, q)
theta2 <- rep(NA, q)
theta1s <- rep(NA, q)
theta2s <- rep(NA, q)
for (d in 1:q){
  #Distribution
  X = runif(n)
  #Disjunct blocks N
  k = n/b1 #amount of blocks
  M <- rep(NA,k)
  for (i in 1:k){
    M[i] = max(X[(1+((i-1)*b1)):(i*b1)])
  }
  Fn <- ecdf(X)
  Fhat = Fn(M)
  Nhat = Fhat
  Yhat = -b1*log(Nhat)
  theta1[d] = ((1/k)*sum(Yhat))^(-1)
  
  #Disjunct blocks B
  k = n/b2 #amount of blocks
  M <- rep(NA,k)
  for (i in 1:k){
    M[i] = max(X[(1+((i-1)*b2)):(i*b2)])
  }
  Fn <- ecdf(X)
  Fhat = Fn(M)
  Nhat = Fhat
  Zhat = b2*(1-Nhat)
  theta2[d] = ((1/k)*sum(Zhat))^(-1)
  
  #Sliding blocks 1
  Ms <- rep(NA,n-b1s+1)
  for (is in 1:(n-b1s+1)){
    Ms[is] = max(X[is:(is+b1s-1)])
    }
  Fns <- ecdf(X)
  Fhats = Fns(Ms)
  Nhats = Fhats
  Yhats = -b1s*log(Nhats)
  theta1s[d] = ((1/(n-b1s+1))*sum(Yhats))^(-1)
  
  #Sliding blocks 2
  Ms <- rep(NA,n-b2s+1)
  for (is in 1:(n-b2s+1)){
    Ms[is] = max(X[is:(is+b2s-1)])
  }
  Fns <- ecdf(X)
  Fhats = Fns(Ms)
  Nhats = Fhats
  Zhats = b2s*(1-Nhats)
  theta2s[d] = ((1/(n-b2s+1))*sum(Zhats))^(-1)
}


par(mfrow=c(2,2))
hist(theta1, las=1, breaks=20, xlab=expression(theta[n]^N), main=expression(paste("Moving Maxima distribution with \n disjoint blocks")))
abline(v=est, lwd=3)
hist(theta1s, las=1, breaks=20, xlab=expression(theta[n]^N), main=expression(paste("Moving Maxima distribution with \n sliding blocks")))
abline(v=est, lwd=3)
hist(theta2, las=1, breaks=20, xlab=expression(theta[n]^B), main=expression(paste("Moving Maxima distribution with \n disjoint blocks")))
abline(v=est, lwd=3)
hist(theta2s, las=1, breaks=20, xlab=expression(theta[n]^B), main=expression(paste("Moving Maxima distribution with \n sliding blocks")))
abline(v=est, lwd=3)
mean(theta1)
mean(theta1s)
mean(theta2)
mean(theta2s)
mse(theta1,est)
mse(theta1s,est)
mse(theta2,est)
mse(theta2s,est)
       
