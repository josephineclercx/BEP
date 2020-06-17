#Comparing De Bilt with Vlissingen wind gusts
#Change lines 20 and 21 to change between wind and rain data for estimating theta, also change the title of plot
#Data
n = nrow(KNMI_20200614_hourly)/2
KNMIwindBilt = KNMI_20200614_hourly[1:n,4]
KNMIregenBilt = KNMI_20200614_hourly[1:n,5]
KNMIwindVlis = KNMI_20200614_hourly[(n+1):(2*n),4]
KNMIregenVlis = KNMI_20200614_hourly[(n+1):(2*n),5]

par(mfrow=c(1,1))
plot(KNMIwindBilt, type='l', main="Highest windgust per hour", ylab="Windspeed (0.1 m/s)")
lines(KNMIwindVlis, col='red')
legend("topright", legend=c(expression("De Bilt", "Vlissingen")), col=c("black", "red"), lwd=1, cex=1)
plot(KNMIregenBilt, type='l', main="Hour sum of precipitaion", ylab="Precipitation (0.1 mm)")
lines(KNMIregenVlis, col='red')
legend("topright", legend=c(expression("De Bilt", "Vlissingen")), col=c("black", "red"), lwd=1, cex=1)

#Estimating theta
y = n/2 #max blocklengte
Xbilt = KNMIwindBilt
Xvlis = KNMIwindVlis
b = 1
o = 1 #stepsize
Bsbilt <- rep(NA,y*o) #different block lengths 2, 3, 4 etc.
theta1sbilt = rep(NA,y*o)
Bsvlis <- rep(NA,y*o) #different block lengths 2, 3, 4 etc.
theta1svlis = rep(NA,y*o)
for (i in 1:y){
  b = b + o
  #Bilt
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
  
  #Vlissingen
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
}

par(mar=c(5,5,3,1)+.01)
plot(theta1sbilt, ylim=c(0,1), xlab=expression(b[n]), main=expression(paste("Estimator ", ~hat(theta)[n]^Nsl, " with different block lengths with the wind gust data")), ylab=expression(~theta[n]^Nsl))
points(theta1svlis, col='red')
legend("bottomright", legend=c(expression("De Bilt", "Vlissingen")), col=c("black", "red"), pch=21, cex=1)
