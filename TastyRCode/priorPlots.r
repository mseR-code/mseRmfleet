# plotting prior distribution for MSY and Fmsy

msy <- seq(0.1,10,length=100)

muMSY <- 3
sdMSY <- 0.5
dist <- dlnorm(msy,meanlog=log(muMSY),sdlog=sdMSY)
plot(msy,dist, type="l",lwd=2, ylab="Prior density",xlab="MSY")
dmuMSY <- dlnorm(muMSY,meanlog=log(muMSY),sdlog=sdMSY)
lines(c(muMSY,muMSY), c(0,dmuMSY), col="red", lty="dashed")

fmsy <- seq(0.01,0.5,length=100)

muMSY <- 3
sdMSY <- 0.5
dist <- dlnorm(msy,meanlog=log(muMSY),sdlog=sdMSY)
plot(msy,dist, type="l",lwd=2, ylab="Prior density",xlab="MSY")
dmuMSY <- dlnorm(muMSY,meanlog=log(muMSY),sdlog=sdMSY)
lines(c(muMSY,muMSY), c(0,dmuMSY), col="red", lty="dashed")
