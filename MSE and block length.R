  #Optimal MSE and blocklength combination
  #for different distributions, change line 5, 22 and titles
  
  n = 5000 #sample size
  est = 0.3 #true value of theta
  o = 5 #stepsize block length
  w =  500  #times of looking at MSE 
  mse1 <- rep(NA, w*o)
  mse2 <- rep(NA, w*o)
  mse1s <- rep(NA, w*o)
  mse2s <- rep(NA, w*o)
  B <- rep(NA,w*o) #different block lengths, 2, 7, 12 etc.
  Bs <- rep(NA,w*o) #different block lengths
  b = 2 #block length
  q = 100 #times of estimate theta
  for (r in 1:w){
      theta1 <- rep(NA, q)
      theta2 <- rep(NA, q)
      theta1s <- rep(NA, q)
      theta2s <- rep(NA, q)
      for (d in 1:q){
        #Distribution
        z = 0.7
        X0 <- rcauchy(1, location = 0, scale = 1)
        E <- rcauchy(n, location = 0, scale = (1-abs(z)))
        X = rep(NA, n)
        for (i in 1:n){
          X[i] = z*X0 + E[i]
          X0 = X[i]
        }
        #Disjoint
        B[b] = b
        k = floor(n/b) #amount of blocks
        M <- rep(NA,k)
        for (i in 1:k){
          M[i] = max(X[(1+((i-1)*b)):(i*b)])
        }
        Fn <- ecdf(X)
        Fhat = Fn(M)
        Nhat = Fhat
        Yhat = -b*log(Nhat)
        theta1[d] = ((1/k)*sum(Yhat))^(-1)
        Zhat = b*(1-Nhat)
        theta2[d] = ((1/k)*sum(Zhat))^(-1) 
        
        #Sliding
        Bs[b] = b
        Ms <- rep(NA,n-b+1)
        for (i in 1:(n-b+1)){
          Ms[i] = max(X[i:(i+b-1)])
        }
        Fn <- ecdf(X)
        Fhat = Fn(Ms)
        Nhat = Fhat
        Yhat = -b*log(Nhat)
        theta1s[d] = ((1/(n-b+1))*sum(Yhat))^(-1)
        Zhat = b*(1-Nhat)
        theta2s[d] = ((1/(n-b+1))*sum(Zhat))^(-1)
      }
  
      mse1[b] = mse(theta1,est)
      mse2[b] = mse(theta2,est)
      mse1s[b] = mse(theta1s,est)
      mse2s[b] = mse(theta2s,est)
      b = b + o #block length steps
  } 
  
  
  
  par(mfrow=c(1,1))
  plot(na.omit(B), na.omit(mse1), xlim=c(0,500), ylim=c(0,0.1), col='black', type='l', xlab=expression(b[n]), ylab=expression(MSE), main=expression(paste("MSE of ", ~hat(theta)[n]^N, " against block length for AR-C")))
  lines(na.omit(Bs), na.omit(mse1s), col='red')
  legend("topright", legend=c(expression("Disjoint blocks", "Sliding blocks")), col=c("black", "red"), lty=1:1, cex=1)
  plot(na.omit(B), na.omit(mse2),  xlim=c(0,500), ylim=c(0,0.1), col='black', type='l', xlab=expression(b[n]), ylab=expression(MSE), main=expression(paste("MSE of " , ~hat(theta)[n]^B, " against block length for AR-C",)))
  lines(na.omit(Bs), na.omit(mse2s), col='red')
  legend("topright", legend=c(expression("Disjoint blocks", "Sliding blocks")), col=c("black", "red"), lty=1:1, cex=1)
  print(B[which.min(mse1)]) #optimal block length
  print(Bs[which.min(mse1s)]) #optimal block length
  print(B[which.min(mse2)]) #optimal block length
  print(Bs[which.min(mse2s)]) #optimal block length

  beep(3)
