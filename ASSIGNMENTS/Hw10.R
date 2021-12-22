#Author: Mitchell Krystofiak
#Class: Math 490 - HW10
#Date: November 5, 2021

#Read in Tsla Data, Reformat Dates
tsla <- read.csv('TSLA.csv')

tsla$Date <- as.Date(tsla$Date,format='%Y-%m-%d')
#View(tsla)

#Select out dates and closing prices
tslats <- data.frame(tsla[,c(1,5)])
tslats <- tsla[,5]
tslada <- tsla[,1]

#Compute 10 day SMA
tsla10day <- c()
imax <- length(tslats)

for (i in 10:imax) 
{
  tsla10day[i] <- 0
  jmin <- (i - 9)
  for (j in jmin:i)
  {
    tsla10day[i] <- tsla10day[i] + tslats[j]
  }
  tsla10day[i] <- tsla10day[i]/10
}

#Compute 50 day SMA
tsla50day <- c()

for (i in 50:imax)
{
  tsla50day[i] <- 0
  jmin <- (i - 49)
  for (j in jmin:i)
  {
    tsla50day[i] <- tsla50day[i] + tslats[j]
  }
  tsla50day[i] <- tsla50day[i]/50
}

#Plot closing prices, 10 day SMA, 50 day SMA
plot(tslada[1:200], tslats[1:200], 
     type='l',
     main="10 Day and 50 Day SMA for TSLA",
     xlab="Date",
     ylab="Closing Price",
     col='black')
lines(tslada[50:imax], tsla50day[50:imax], type='l', col='blue')
lines(tslada[10:imax], tsla10day[10:imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Closing Price','50 Day','10 Day'), 
       col=c('black','blue','red'),
       lwd=.5)

#Find the intersections from the 10 day and 50 day SMA
#while also storing the dates.

iMACctr <- 0
tslaMAC1ret <- c()
tslaMAC1fut <- c()
icmax <- imax-1
poscount <- 0
tslaMACda <- as.Date(c())

for (i in 51:icmax)
{
  if(tsla10day[i-1] < tsla50day[i-1] && tsla10day[i] > tsla50day[i])
  {
    iMACctr <- iMACctr + 1
    tslaMAC1ret[iMACctr] = (tslats[i+1] - tslats[i])/tslats[i]*100
    if (tslaMAC1ret[iMACctr] > 0)
    {
      poscount <- poscount + 1
    }
    tslaMAC1fut[iMACctr] = (tslats[i+2] - tslats[i+1])/tslats[i+1]*100
    tslaMACda[iMACctr] = tslada[i]
  }
}

plot(tslaMac1t,
     main='MAC 1 Day After',
     xlab='Dates',
     ylab='Percent Return')
abline(0,0)

cat("The mean of the MAC 1 day is",mean(tslaMAC1ret), "and the mean\n")
cat("of the MAC 1 day in the future is", mean(tslaMAC1fut), "\n\n")

cat("The winning percentage of the MAC 1 day is", poscount/iMACctr,"\n\n")


#Checking what would happen 5 days after a MAC occurs

iMACctr <- 0
poscount <- 0
tslaMAC5ret <- c()
tslaMAC5fut <- c()
tslaMACda <- as.Date(c())
icmax <- imax-1
for (i in 51: icmax)
{
  if (tsla10day[i-1] < tsla50day[i-1] && tsla10day[i] > tsla50day[i])
  {
    iMACctr <- iMACctr + 1
    tslaMAC5ret[iMACctr] = (tslats[i+5] - tslats[i])/tslats[i]*100
    if (tslaMAC5ret[iMACctr] > 0)
    {
      poscount <- poscount + 1
    }
    tslaMAC5fut[iMACctr] = (tslats[i+10] - tslats[i+5])/tslats[i+5]*100
    tslaMACda[iMACctr] = tslada[i]
  }
}
plot(tslaMac5t,
     main='MAC 5 Days After',
     xlab='Dates',
     ylab='Percent Return')
abline(0,0)

cat("The mean of the MAC 5 day is",mean(tslaMAC5ret), "and the mean\n")
cat("of the MAC 5 day in the future is", mean(tslaMAC5fut), "\n\n")

cat("The winning percentage of the MAC 5 day is", poscount/iMACctr,"\n\n")

#Checking what would happen 10 days after a MAC occurs

iMACctr <- 0
poscount <- 0
tslaMAC10ret <- c()
tslaMAC10fut <- c()
tslaMACda <- as.Date(c())
icmax <- imax-1
for (i in 51: icmax)
{
  if (tsla10day[i-1] < tsla50day[i-1] && tsla10day[i] > tsla50day[i])
  {
    iMACctr <- iMACctr + 1
    tslaMAC10ret[iMACctr] = (tslats[i+10] - tslats[i])/tslats[i]*100
    if (tslaMAC10ret[iMACctr] > 0)
    {
      poscount <- poscount + 1
    }
    tslaMAC10fut[iMACctr] = (tslats[i+20] - tslats[i+10])/tslats[i+10]*100
    tslaMACda[iMACctr] = tslada[i]
  }
}
plot(tslaMac10t,
     main='MAC 10 Days After',
     xlab='Dates',
     ylab='Percent Return')
abline(0,0)

cat("The mean of the MAC 10 day is",mean(tslaMAC10ret), "and the mean\n")
cat("of the MAC 10 day in the future is", mean(tslaMAC10fut), "\n\n")

cat("The winning percentage of the MAC 10 day is", poscount/iMACctr,"\n\n")

#Checking what would happen 15 days after a MAC occurs

iMACctr <- 0
poscount <- 0
tslaMAC15ret <- c()
tslaMAC15fut <- c()
tslaMACda <- as.Date(c())
icmax <- imax-1
for (i in 51: icmax)
{
  if (tsla10day[i-1] < tsla50day[i-1] && tsla10day[i] > tsla50day[i])
  {
    iMACctr <- iMACctr + 1
    tslaMAC15ret[iMACctr] = (tslats[i+15] - tslats[i])/tslats[i]*100
    if (tslaMAC15ret[iMACctr] > 0)
    {
      poscount <- poscount + 1
    }
    tslaMAC15fut[iMACctr] = (tslats[i+30] - tslats[i+15])/tslats[i+15]*100
    tslaMACda[iMACctr] = tslada[i]
  }
}
plot(tslaMac15t,
     main='MAC 15 Days After',
     xlab='Dates',
     ylab='Percent Return')
abline(0,0)

cat("The mean of the MAC 15 day is",mean(tslaMAC15ret), "and the mean\n")
cat("of the MAC 15 day in the future is", mean(tslaMAC15fut), "\n\n")

cat("The winning percentage of the MAC 15 day is", poscount/iMACctr,"\n\n")

#Checking what would happen 20 days after a MAC occurs

iMACctr <- 0
poscount <- 0
tslaMAC20ret <- c()
tslaMAC20fut <- c()
tslaMACda <- as.Date(c())
icmax <- imax-1
for (i in 51: icmax)
{
  if (tsla10day[i-1] < tsla50day[i-1] && tsla10day[i] > tsla50day[i])
  {
    iMACctr <- iMACctr + 1
    tslaMAC20ret[iMACctr] = (tslats[i+20] - tslats[i])/tslats[i]*100
    if (tslaMAC20ret[iMACctr] > 0)
    {
      poscount <- poscount + 1
    }
    tslaMAC20fut[iMACctr] = (tslats[i+40] - tslats[i+20])/tslats[i-20]*100
    tslaMACda[iMACctr] = tslada[i]
  }
}
plot(tslaMac20t,
     main='MAC 20 Days After',
     xlab='Dates',
     ylab='Percent Return')
abline(0,0)

cat("The mean of the MAC 20 day is",mean(tslaMAC20ret), "and the mean\n")
cat("of the MAC 20 day in the future is", mean(tslaMAC20fut), "\n\n")
cat("The winning percentage of the MAC 20 day is", poscount/iMACctr,"\n\n")

