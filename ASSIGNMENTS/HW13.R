#Author: Mitchell Krystofiak
#Class: MATH490 - HW13
#Date: Decemeber 2, 2021
#Description: Implement Bollinger Bands and perform technical 
#             analysis on Tesla Stock Data (updated 12/1/2021).

library(flextable)
tsla <- read.csv("./TSLA.csv")

flextable(head(tsla))

#Fix dates

tsla$Date <- as.Date(tsla$Date, format="%Y-%m-%d")

#Select out returns

tslats <- data.frame(tsla[,c(1,5)])
tslada <- tsla[,1]
tslats <- tsla[,5]

#Typical Price

tslaHigh <- tsla[,3]
tslaLow <- tsla[,4]

tslaTP <- (tslaHigh + tslaLow + tslats)/3

#Compute the Moving Averages and Bollinger Bands

#Bollinger Band Goal: For short term, use a 10 day moving average
#with 1.5 standard deviations.

tsla10day <- c()
Bolupper10 <- c()
Bollower10 <- c()
imax <- length(tslaTP)

for (i in 10:imax)
{
  jmin <- (i - 9) 
  tsla10day[i] <- 0
  Bolupper10[i] <- 0
  Bollower10[i] <- 0
  for (j in jmin:i)
  {
    tsla10day[i] <- tsla10day[i] + tslaTP[j]
  }
  tsla10day[i] <- tsla10day[i]/10
  Bolupper10[i] <- tsla10day[i] + (1.5 * sd(tslaTP[jmin:i]))
  Bollower10[i] <- tsla10day[i] - (1.5 * sd(tslaTP[jmin:i]))
}

#Bollinger Band Goal: For medium term, use a 20 day moving average
#with 2 standard deviations.

tsla20day <- c()
Bolupper20 <- c()
Bollower20 <- c()
imax <- length(tslaTP)

for (i in 20:imax)
{
  jmin <- (i - 19)
  tsla20day[i] <- 0
  Bolupper20[i] <- 0
  Bollower20[i] <- 0
  for (j in jmin:i)
  {
    tsla20day[i] <- tsla20day[i] + tslats[j]
  }
  tsla20day[i] <- tsla20day[i]/20
  Bolupper20[i] <- tsla20day[i] + (2 * sd(tslaTP[jmin:i]))
  Bollower20[i] <- tsla20day[i] - (2 * sd(tslaTP[jmin:i]))
}

#Bollinger Band Goal: For long term, use a 50 day moving average
#with 2.5 standard deviations.

tsla50day <- c()
Bolupper50 <- c()
Bollower50 <- c()
imax <- length(tslaTP)

for (i in 50:imax)
{
  jmin <- (i - 49)
  tsla50day[i] <- 0
  Bolupper50[i] <- 0
  Bollower50[i] <- 0
  for (j in jmin:i)
  {
    tsla50day[i] <- tsla50day[i] + tslats[j]
  }
  tsla50day[i] <- tsla50day[i]/50
  Bolupper50[i] <- tsla50day[i] + (2.5 * sd(tslaTP[jmin:i]))
  Bollower50[i] <- tsla50day[i] - (2.5 * sd(tslaTP[jmin:i]))
}

#Plot moving averages and typical price.

val <- 100

plot(tslada[1:imax], tslaTP[1:imax], 
     type='l',
     main="10 day SMA for TSLA (Alltime)",
     xlab="Date",
     ylab="Typical Price",
     col='black')
lines(tslada[10:imax], tsla10day[10:imax], type='l', col='blue')
lines(tslada[10:imax], Bolupper10[10:imax], type='l', col='red')
lines(tslada[10:imax], Bollower10[10:imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Typical Price','10 Day SMA','Bollinger'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[(imax-val):imax], tslaTP[(imax-val):imax], 
     type='l',
     main="10 Day SMA for TSLA (Zoomed in)",
     xlab="Date",
     ylab="Typical Price",
     col='black')
lines(tslada[(imax-val):imax], tsla10day[(imax-val):imax], type='l', col='blue')
lines(tslada[(imax-val):imax], Bolupper10[(imax-val):imax], type='l', col='red')
lines(tslada[(imax-val):imax], Bollower10[(imax-val):imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Typical Price','10 Day','Bollinger'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[1:imax], tslaTP[1:imax], 
     type='l',
     main="20 day SMA for TSLA (Alltime)",
     xlab="Date",
     ylab="Typical Price",
     col='black')
lines(tslada[20:imax], tsla20day[20:imax], type='l', col='blue')
lines(tslada[20:imax], Bolupper20[20:imax], type='l', col='red')
lines(tslada[20:imax], Bollower20[20:imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Typical Price','20 Day SMA','Bollinger'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[(imax-val):imax], tslaTP[(imax-val):imax], 
     type='l',
     main="20 Day SMA for TSLA (Zoomed in)",
     xlab="Date",
     ylab="Typical Price",
     col='black')
lines(tslada[(imax-val):imax], tsla20day[(imax-val):imax], type='l', col='blue')
lines(tslada[(imax-val):imax], Bolupper20[(imax-val):imax], type='l', col='red')
lines(tslada[(imax-val):imax], Bollower20[(imax-val):imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Typical Price','20 Day','Bollinger'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[1:imax], tslaTP[1:imax], 
     type='l',
     main="50 day SMA for TSLA (Alltime)",
     xlab="Date",
     ylab="Typical Price",
     col='black')
lines(tslada[50:imax], tsla50day[50:imax], type='l', col='blue')
lines(tslada[50:imax], Bolupper50[50:imax], type='l', col='red')
lines(tslada[50:imax], Bollower50[50:imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Typical Price','50 Day SMA','Bollinger'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[(imax-val):imax], tslaTP[(imax-val):imax], 
     type='l',
     main="50 Day SMA for TSLA (Zoomed in)",
     xlab="Date",
     ylab="Typical Price",
     col='black')
lines(tslada[(imax-val):imax], tsla50day[(imax-val):imax], type='l', col='blue')
lines(tslada[(imax-val):imax], Bolupper50[(imax-val):imax], type='l', col='red')
lines(tslada[(imax-val):imax], Bollower50[(imax-val):imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Typical Price','50 Day','Bollinger'), 
       col=c('black','blue','red'),
       lwd=.5)

# Calculate when the price crosses below the lower band.

icmax <- (imax-1)
count <- 0
countNot <- 0

tslaBol1ret <- c()
tslaBol5ret <- c()
tslaBol10ret <- c()
tslaBol15ret <- c()
tslaBol20ret <- c()

tslaNotBol1ret <- c()
tslaNotBol5ret <- c()
tslaNotBol10ret <- c()
tslaNotBol15ret <- c()
tslaNotBol20ret <- c()

tslaU1 <- c()
tslaU5 <- c()
tslaU10 <- c()
tslaU15 <- c()
tslaU20 <- c()

tslaBolda <- as.Date(c())
tslaNotBolda <- as.Date(c())

for (i in 21:icmax)
{
  if (Bollower20[i-1] < tslaTP[i-1] && Bollower20[i] > tslaTP[i])
  {
    count <- count + 1
    tslaBol1ret[count] <- ((tslaTP[i+1] - tslaTP[i])/tslaTP[i])*100
    tslaBol5ret[count] <- ((tslaTP[i+5] - tslaTP[i])/tslaTP[i])*100
    tslaBol10ret[count] <- ((tslaTP[i+10] - tslaTP[i])/tslaTP[i])*100
    tslaBol15ret[count] <- ((tslaTP[i+15] - tslaTP[i])/tslaTP[i])*100
    tslaBol20ret[count] <- ((tslaTP[i+20] - tslaTP[i])/tslaTP[i])*100
    tslaBolda[count] <- tslada[i]
  }
  else
  {
    countNot <- countNot + 1
    tslaNotBol1ret[countNot] <- ((tslaTP[i+1] - tslaTP[i])/tslaTP[i])*100
    tslaNotBol5ret[countNot] <- ((tslaTP[i+5] - tslaTP[i])/tslaTP[i])*100
    tslaNotBol10ret[countNot] <- ((tslaTP[i+10] - tslaTP[i])/tslaTP[i])*100
    tslaNotBol15ret[countNot] <- ((tslaTP[i+15] - tslaTP[i])/tslaTP[i])*100
    tslaNotBol20ret[countNot] <- ((tslaTP[i+20] - tslaTP[i])/tslaTP[i])*100
    tslaNotBolda[countNot] <- tslada[i]
  }
}

for (i in 1:icmax)
{
  tslaU1[i] <- ((tslaTP[i+1] - tslaTP[i])/tslaTP[i])*100
  tslaU5[i] <- ((tslaTP[i+5] - tslaTP[i])/tslaTP[i])*100
  tslaU10[i] <- ((tslaTP[i+10] - tslaTP[i])/tslaTP[i])*100
  tslaU15[i] <- ((tslaTP[i+15] - tslaTP[i])/tslaTP[i])*100
  tslaU20[i] <- ((tslaTP[i+20] - tslaTP[i])/tslaTP[i])*100
}

#Plot them. NOTE: MAC implies the upper bound of 
# the envelope crosses the closing price!!

plot(tslaBolda, tslaBol1ret,
     main='1 Day After Lower Bound Crossover',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaBolda, tslaBol5ret,
     main='5 Days After Lower Bound Crossover',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaBolda, tslaBol10ret,
     main='10 Days After Lower Bound Crossover',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaBolda, tslaBol15ret,
     main='15 Days After Lower Bound Crossover',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaBolda, tslaBol20ret,
     main='20 Days After Lower Bound Crossover',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)

#Compute summary statistics for each vector of returns.

Stats <- c('Min','Q1','Q2','Mean','Q3','Max')

U1 <- as.vector(summary(tslaU1)[1:6])
Bol1 <- as.vector(summary(tslaBol1ret)[1:6])
NBol1 <- as.vector(summary(tslaNotBol1ret)[1:6])
UData <- data.frame(Stats, U1, Bol1, NBol1)
flextable(UData)

U5 <- as.vector(summary(tslaU5)[1:6])
Bol5 <- as.vector(summary(tslaBol5ret)[1:6])
NBol5 <- as.vector(summary(tslaNotBol5ret)[1:6])
UData <- data.frame(Stats, U5, Bol5, NBol5)
flextable(UData)

U10 <- as.vector(summary(tslaU10)[1:6])
Bol10 <- as.vector(summary(tslaBol10ret)[1:6])
NBol10 <- as.vector(summary(tslaNotBol10ret)[1:6])
UData <- data.frame(Stats, U10, Bol10, NBol10)
flextable(UData)

U15 <- as.vector(summary(tslaU15)[1:6])
Bol15 <- as.vector(summary(tslaBol15ret)[1:6])
NBol15 <- as.vector(summary(tslaNotBol15ret)[1:6])
UData <- data.frame(Stats, U15, Bol15, NBol15)
flextable(UData)

U20 <- as.vector(summary(tslaU20)[1:6])
Bol20 <- as.vector(summary(tslaBol20ret)[1:6])
NBol20 <- as.vector(summary(tslaNotBol20ret)[1:6])
UData <- data.frame(Stats, U20, Bol20, NBol20)
flextable(UData)

#Compare Boxplots

par(mfrow = c(1,3))
boxplot(tslaU1, main='Usual 1-day Returns')
boxplot(tslaBol1ret, main='1-day Returns after Lower Bound Crossover')
boxplot(tslaNotBol1ret, main='1-day Returns after Non-Lower Bound Crossover')

boxplot(tslaU5, main='Usual 5-day Returns')
boxplot(tslaBol5ret, main='5-day Returns after Lower Bound Crossover')
boxplot(tslaNotBol5ret, main='5-day Returns after Non-Lower Bound Crossover')

boxplot(tslaU10, main='Usual 10-day Returns')
boxplot(tslaBol10ret, main='10-day Returns after Lower Bound Crossover')
boxplot(tslaNotBol10ret, main='10-day Returns after Non-Lower Bound Crossover')

boxplot(tslaU15, main='Usual 15-day Returns')
boxplot(tslaBol15ret, main='15-day Returns after Lower Bound Crossover')
boxplot(tslaNotBol15ret, main='15-day Returns after Non-Lower Bound Crossover')

boxplot(tslaU20, main='Usual 20-day Returns')
boxplot(tslaBol20ret, main='20-day Returns after Lower Bound Crossover')
boxplot(tslaNotBol20ret, main='20-day Returns after Non-Lower Bound Crossover')

# Compare Histograms.

hist(tslaU1, main='Usual 1-day Returns',breaks=40)
hist(tslaBol1ret, main='1-day Returns after Lower Bound Crossover',breaks=10)
hist(tslaNotBol1ret, main='1-day Returns after Non-Lower Bound Crossover',breaks=40)

hist(tslaU5, main='Usual 5-day Returns',breaks=40)
hist(tslaBol5ret, main='5-day Returns after Lower Bound Crossover',breaks=10)
hist(tslaNotBol5ret, main='5-day Returns after Non-Lower Bound Crossover',breaks=40)

hist(tslaU10, main='Usual 10-day Returns',breaks=40)
hist(tslaBol10ret, main='10-day Returns after Lower Bound Crossover',breaks=10)
hist(tslaNotBol10ret, main='10-day Returns after Non-Lower Bound Crossover',breaks=40)

hist(tslaU15, main='Usual 15-day Returns',breaks=40)
hist(tslaBol15ret, main='15-day Returns after Lower Bound Crossover',breaks=10)
hist(tslaNotBol15ret, main='15-day Returns after Non-Lower Bound Crossover',breaks=40)

hist(tslaU20, main='Usual 20-day Returns',breaks=40)
hist(tslaBol20ret, main='20-day Returns after Lower Bound Crossover',breaks=10)
hist(tslaNotBol20ret, main='20-day Returns after Non-Lower Bound Crossover',breaks=40)
par(mfrow=c(1,1))

#Check the sample sizes and variances.

Stats <- c('Usual 1-day','1-day After LBC','1-day After NLBC')
Lengths <- c(length(tslaU1),length(tslaBol1ret),length(tslaNotBol1ret))
Variances <- c(var(tslaU1,na.rm=T),var(tslaBol1ret,na.rm=T), var(tslaNotBol1ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 5-day','5-day After LBC','5-day After NLBC')
Lengths <- c(length(tslaU5),length(tslaBol5ret),length(tslaNotBol5ret))
Variances <- c(var(tslaU5,na.rm=T),var(tslaBol5ret,na.rm=T), var(tslaNotBol5ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 10-day','10-day After LBC','10-day After NLBC')
Lengths <- c(length(tslaU10),length(tslaBol10ret),length(tslaNotBol10ret))
Variances <- c(var(tslaU10,na.rm=T),var(tslaBol10ret,na.rm=T), var(tslaNotBol10ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 15-day','15-day After LBC','15-day After NLBC')
Lengths <- c(length(tslaU15),length(tslaBol15ret),length(tslaNotBol15ret))
Variances <- c(var(tslaU15,na.rm=T),var(tslaBol15ret,na.rm=T), var(tslaNotBol15ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 20-day','20-day After LBC','20-day After NLBC')
Lengths <- c(length(tslaU20),length(tslaBol20ret),length(tslaNotBol20ret))
Variances <- c(var(tslaU20,na.rm=T),var(tslaBol20ret,na.rm=T), var(tslaNotBol20ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

# View Quantile-Quantile plots to check normality.

par(mfrow=c(1,3))
qqnorm(tslaU1)
qqline(tslaU1)
qqnorm(tslaBol1ret)
qqline(tslaBol1ret)
qqnorm(tslaNotBol1ret)
qqline(tslaNotBol1ret)

qqnorm(tslaU5)
qqline(tslaU5)
qqnorm(tslaBol5ret)
qqline(tslaBol5ret)
qqnorm(tslaNotBol5ret)
qqline(tslaNotBol5ret)

qqnorm(tslaU10)
qqline(tslaU10)
qqnorm(tslaBol10ret)
qqline(tslaBol10ret)
qqnorm(tslaNotBol10ret)
qqline(tslaNotBol10ret)

qqnorm(tslaU15)
qqline(tslaU15)
qqnorm(tslaBol15ret)
qqline(tslaBol15ret)
qqnorm(tslaNotBol15ret)
qqline(tslaNotBol15ret)

qqnorm(tslaU20)
qqline(tslaU20)
qqnorm(tslaBol20ret)
qqline(tslaBol20ret)
qqnorm(tslaNotBol20ret)
qqline(tslaNotBol20ret)
par(mfrow=c(1,1))

# Use Shapiro test to further test normality.
# If p value is less than threshold, reject null hypothesis.
# For shapiro test, the null hypothesis is that the data is normal, and
# the alternative hyptothesis is that the data is not normal.
# If our p value is less than say .05 or .01, we reject that the 
# data is normal.

shapiro.test(tslaU1) #not normal
shapiro.test(tslaBol1ret) #not normal
shapiro.test(tslaNotBol1ret) #not normal

shapiro.test(tslaU5) #not normal
shapiro.test(tslaBol5ret) #not normal
shapiro.test(tslaNotBol5ret) #not normal

shapiro.test(tslaU10) #not normal
shapiro.test(tslaBol10ret) #normal****
shapiro.test(tslaNotBol10ret) #not normal

shapiro.test(tslaU15) #not normal
shapiro.test(tslaBol15ret) #not normal
shapiro.test(tslaNotBol15ret) #not normal

shapiro.test(tslaU20) #not normal
shapiro.test(tslaBol20ret) #not normal
shapiro.test(tslaNotBol20ret) #not normal

#Conduct t-test with Welch test for unequal variances.

t.test(tslaU1,tslaBol1ret, Welch=T) #true difference in means is equal to 0
t.test(tslaU1,tslaNotBol1ret, Welch=T) #true difference in means is equal to 0

t.test(tslaU5, tslaBol5ret, Welch=T) #true difference in means is equal to 0
t.test(tslaU5, tslaNotBol5ret, Welch=T) #true difference in means is equal to 0

t.test(tslaU10, tslaBol10ret, Welch=T) #true difference in means is equal to 0
t.test(tslaU10, tslaNotBol10ret, Welch=T) #true difference in means is equal to 0

t.test(tslaU15, tslaBol15ret, Welch=T)
t.test(tslaU15, tslaNotBol15ret, Welch=T)

t.test(tslaU20, tslaBol20ret, Welch=T)
t.test(tslaU20, tslaNotBol20ret, Welch=T)

wilcox.test(tslaU1, tslaBol1ret)
wilcox.test(tslaU1, tslaNotBol1ret)

#may be the only positive trend indicator
wilcox.test(tslaU5, tslaBol5ret)
wilcox.test(tslaU5, tslaNotBol5ret)

wilcox.test(tslaU10, tslaBol10ret)
wilcox.test(tslaU10, tslaNotBol10ret)

wilcox.test(tslaU15, tslaBol15ret)
wilcox.test(tslaU15, tslaNotBol15ret)

wilcox.test(tslaU20, tslaBol20ret)
wilcox.test(tslaU20, tslaNotBol20ret)


#Other way to look at Balinger Bands! Look at yahoo.com


















