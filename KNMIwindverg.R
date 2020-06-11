#Comparing De Bilt with Vlissingen wind gusts
#Data
KNMIwindBilt = KNMI_20200607_hourly.10[1:744,4]
KNMIregenBilt = KNMI_20200607_hourly.10[1:744,5]
KNMIwindVlis = KNMI_20200607_hourly.10[745:1488,4]
KNMIregenVlis = KNMI_20200607_hourly.10[745:1488,5]

par(mfrow=c(1,1))
plot(KNMIwindBilt, type='l', main="Highest windgust per hour", ylab="Windspeed (0.1 m/s)")
lines(KNMIwindVlis, col='red')
legend("topright", legend=c(expression("De Bilt", "Vlissingen")), col=c("black", "red"), lwd=1, cex=1)

#Estimating theta
n = 744
y = 360 #max blocklengte
Xbilt = KNMIwindBilt
Xvlis = KNMIwindVlis
b = 1
o = 1 #stepsize
Bbilt <- rep(NA,y*o) #different block lengths, 2, 3, 4 etc.
Bsbilt <- rep(NA,y*o) #different block lengths 2, 3, 4 etc.
theta1bilt = rep(NA,y*o)
theta1sbilt = rep(NA,y*o)
theta2bilt = rep(NA,y*o)
theta2sbilt = rep(NA,y*o)
Bvlis <- rep(NA,y*o) #different block lengths, 2, 3, 4 etc.
Bsvlis <- rep(NA,y*o) #different block lengths 2, 3, 4 etc.
theta1vlis = rep(NA,y*o)
theta1svlis = rep(NA,y*o)
theta2vlis = rep(NA,y*o)
theta2svlis = rep(NA,y*o)
for (i in 1:y){
  b = b + o
  #Bilt
  #Disjoint
  Bbilt[b] = b
  kbilt = floor(n/b) #amount of blocks
  Mbilt <- rep(NA,kbilt)
  for (j in 1:kbilt){
    Mbilt[j] = max(Xbilt[(1+((j-1)*b)):(j*b)])
  }
  Fnbilt <- ecdf(Xbilt)
  Fhatbilt = Fnbilt(Mbilt)
  Nhatbilt = Fhatbilt
  Yhatbilt = -b*log(Nhatbilt)
  theta1bilt[b] = ((1/kbilt)*sum(Yhatbilt))^(-1)
  Zhatbilt = b*(1-Nhatbilt)
  theta2bilt[b] = ((1/kbilt)*sum(Zhatbilt))^(-1)
  
  #Sliding
  Bsbilt[b] = b
  Msbilt <- rep(NA,n-b+1)
  for (r in 1:(n-b+1)){
    Msbilt[r] = max(Xbilt[r:(r+b-1)])
  }
  Fnsbilt <- ecdf(Xbilt)
  Fhatsbilt = Fnsbilt(Msbilt)
  Nhatsbilt = Fhatsbilt
  Yhatsbilt = -b*log(Nhatsbilt)
  theta1sbilt[b] = ((1/(n-b+1))*sum(Yhatsbilt))^(-1)
  Zhatsbilt = b*(1-Nhatsbilt)
  theta2sbilt[b] = ((1/(n-b+1))*sum(Zhatsbilt))^(-1)
  
  #Vlissingen
  #Disjoint
  Bvlis[b] = b
  kvlis = floor(n/b) #amount of blocks
  Mvlis <- rep(NA,kvlis)
  for (j in 1:kvlis){
    Mvlis[j] = max(Xvlis[(1+((j-1)*b)):(j*b)])
  }
  Fnvlis <- ecdf(Xvlis)
  Fhatvlis = Fnvlis(Mvlis)
  Nhatvlis = Fhatvlis
  Yhatvlis = -b*log(Nhatvlis)
  theta1vlis[b] = ((1/kvlis)*sum(Yhatvlis))^(-1)
  Zhatvlis = b*(1-Nhatvlis)
  theta2vlis[b] = ((1/kvlis)*sum(Zhatvlis))^(-1)
  
  #Sliding
  Bsvlis[b] = b
  Msvlis <- rep(NA,n-b+1)
  for (r in 1:(n-b+1)){
    Msvlis[r] = max(Xvlis[r:(r+b-1)])
  }
  Fnsvlis <- ecdf(Xvlis)
  Fhatsvlis = Fnsvlis(Msvlis)
  Nhatsvlis = Fhatsvlis
  Yhatsvlis = -b*log(Nhatsvlis)
  theta1svlis[b] = ((1/(n-b+1))*sum(Yhatsvlis))^(-1)
  Zhatsvlis = b*(1-Nhatsvlis)
  theta2svlis[b] = ((1/(n-b+1))*sum(Zhatsvlis))^(-1)
  
}

par(mar=c(5,5,3,1)+.01)
plot(theta1bilt, ylim=c(0,1), xlab=expression(b[n]), main=expression(paste("Estimator ", ~hat(theta)[n]^Ndj, " with different block lengths with wind gust data")), ylab=expression(~theta[n]^Ndj))
points(theta1vlis, col='red')
legend("bottomright", legend=c(expression("De Bilt", "Vlissingen")), col=c("black", "red"), pch=21, cex=1)
plot(theta1sbilt, ylim=c(0,1), xlab=expression(b[n]), main=expression(paste("Estimator ", ~hat(theta)[n]^Nsl, " with different block lengths with wind gust data")), ylab=expression(~theta[n]^Nsl))
points(theta1svlis, col='red')
legend("bottomright", legend=c(expression("De Bilt", "Vlissingen")), col=c("black", "red"), pch=21, cex=1)
plot(theta2bilt, ylim=c(0,1), xlab=expression(b[n]), main=expression(paste("Estimator ", ~hat(theta)[n]^Bdj, " with different block lengths with wind gust data")), ylab=expression(~theta[n]^Bdj))
points(theta2vlis, col='red')
legend("bottomright", legend=c(expression("De Bilt", "Vlissingen")), col=c("black", "red"), pch=21, cex=1)
plot(theta2sbilt, ylim=c(0,1), xlab=expression(b[n]), main=expression(paste("Estimator ", ~hat(theta)[n]^Bsl, " with different block lengths with wind gust data")), ylab=expression(~theta[n]^Bsl))
points(theta2svlis, col='red')
legend("bottomright", legend=c(expression("De Bilt", "Vlissingen")), col=c("black", "red"), pch=21, cex=1)
