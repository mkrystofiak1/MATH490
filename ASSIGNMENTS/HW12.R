#Author: Mitchell Krystofiak
#MATH 490 - Hw 12
#Date: November 19, 2021
#Description: Technical analysis using hypothesis testing for Moving Average
#             Crossovers (10 and 50 day, x and x day). Do the same thing for 
#             Moving Average Envelopes.

#1a) Finalized indicator analysis procedure on 10 and 50 day MAC.
# Import Tesla stock data.

library(flextable)
tsla <- read.csv("TSLA.csv")
flextable(head(tsla))

#Fix dates

tsla$Date <- as.Date(tsla$Date, format="%Y-%m-%d")

#Select out returns

tslats <- data.frame(tsla[,c(1,5)])
tslada <- tsla[,1]
tslats <- tsla[,5]

#Compute the Moving Averages (10 Day and 50 Day)

tsla10day <- c()
imax <- length(tslats)

for (i in 10:imax)
{
  jmin <- (i - 9)
  tsla10day[i] <- 0
  for (j in jmin:i)
  {
    tsla10day[i] <- tsla10day[i] + tslats[j]
  }
  tsla10day[i] <- tsla10day[i]/10
}

tsla50day <- c()
imax <- length(tslats)

for (i in 50:imax)
{
  jmin <- (i - 49)
  tsla50day[i] <- 0
  for (j in jmin:i)
  {
    tsla50day[i] <- tsla50day[i] + tslats[j]
  }
  tsla50day[i] <- tsla50day[i]/50
}

plot(tslada[1:imax], tslats[1:imax], 
     type='l',
     main="10 Day and 50 Day SMA for TSLA (Alltime)",
     xlab="Date",
     ylab="Closing Price",
     col='black')
lines(tslada[50:imax], tsla50day[50:imax], type='l', col='blue')
lines(tslada[10:imax], tsla10day[10:imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Closing Price','50 Day','10 Day'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[1:200], tslats[1:200], 
     type='l',
     main="10 Day and 50 Day SMA for TSLA (Zoomed in)",
     xlab="Date",
     ylab="Closing Price",
     col='black')
lines(tslada[50:imax], tsla50day[50:imax], type='l', col='blue')
lines(tslada[10:imax], tsla10day[10:imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Closing Price','50 Day','10 Day'), 
       col=c('black','blue','red'),
       lwd=.5)

#Calculate Moving Average Crossovers between the
#10 day and 50 day SMAs and getting 1, 5, 10, 15 and 20 days after.
# Also compute usual 1, 5, 10, 15, and 20 days after.

icmax <- (imax-1)
count <- 0
countNot <- 0

tslaMAC1ret <- c()
tslaMAC5ret <- c()
tslaMAC10ret <- c()
tslaMAC15ret <- c()
tslaMAC20ret <- c()

tslaNotMAC1ret <- c()
tslaNotMAC5ret <- c()
tslaNotMAC10ret <- c()
tslaNotMAC15ret <- c()
tslaNotMAC20ret <- c()

tslaU1 <- c()
tslaU5 <- c()
tslaU10 <- c()
tslaU15 <- c()
tslaU20 <- c()

tslaMACda <- as.Date(c())
tslaNotMACda <- as.Date(c())

for (i in 51:icmax)
{
  if (tsla10day[i-1] < tsla50day[i-1] && tsla10day[i] > tsla50day[i])
  {
    count <- count + 1
    tslaMAC1ret[count] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
    tslaMAC5ret[count] <- ((tslats[i+5] - tslats[i])/tslats[i])*100
    tslaMAC10ret[count] <- ((tslats[i+10] - tslats[i])/tslats[i])*100
    tslaMAC15ret[count] <- ((tslats[i+15] - tslats[i])/tslats[i])*100
    tslaMAC20ret[count] <- ((tslats[i+20] - tslats[i])/tslats[i])*100
    tslaMACda[count] <- tslada[i]
  }
  else
  {
    countNot <- countNot + 1
    tslaNotMAC1ret[countNot] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
    tslaNotMAC5ret[countNot] <- ((tslats[i+5] - tslats[i])/tslats[i])*100
    tslaNotMAC10ret[countNot] <- ((tslats[i+10] - tslats[i])/tslats[i])*100
    tslaNotMAC15ret[countNot] <- ((tslats[i+15] - tslats[i])/tslats[i])*100
    tslaNotMAC20ret[countNot] <- ((tslats[i+20] - tslats[i])/tslats[i])*100
    tslaNotMACda[countNot] <- tslada[i]
  }
}

for (i in 1:icmax)
{
  tslaU1[i] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
  tslaU5[i] <- ((tslats[i+5] - tslats[i])/tslats[i])*100
  tslaU10[i] <- ((tslats[i+10] - tslats[i])/tslats[i])*100
  tslaU15[i] <- ((tslats[i+15] - tslats[i])/tslats[i])*100
  tslaU20[i] <- ((tslats[i+20] - tslats[i])/tslats[i])*100
}

#Plot them.

plot(tslaMACda, tslaMAC1ret,
     main='1 Day After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC5ret,
     main='5 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC10ret,
     main='10 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC15ret,
     main='15 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC20ret,
     main='20 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)

#Compute summary statistics for each vector of returns.

Stats <- c('Min','Q1','Q2','Mean','Q3','Max')

U1 <- as.vector(summary(tslaU1)[1:6])
MAC1 <- as.vector(summary(tslaMAC1ret)[1:6])
NMAC1 <- as.vector(summary(tslaNotMAC1ret)[1:6])
UData <- data.frame(Stats, U1, MAC1, NMAC1)
flextable(UData)

U5 <- as.vector(summary(tslaU5)[1:6])
MAC5 <- as.vector(summary(tslaMAC5ret)[1:6])
NMAC5 <- as.vector(summary(tslaNotMAC5ret)[1:6])
UData <- data.frame(Stats, U5, MAC5, NMAC5)
flextable(UData)

U10 <- as.vector(summary(tslaU10)[1:6])
MAC10 <- as.vector(summary(tslaMAC10ret)[1:6])
NMAC10 <- as.vector(summary(tslaNotMAC10ret)[1:6])
UData <- data.frame(Stats, U10, MAC10, NMAC10)
flextable(UData)

U15 <- as.vector(summary(tslaU15)[1:6])
MAC15 <- as.vector(summary(tslaMAC15ret)[1:6])
NMAC15 <- as.vector(summary(tslaNotMAC15ret)[1:6])
UData <- data.frame(Stats, U15, MAC15, NMAC15)
flextable(UData)

U20 <- as.vector(summary(tslaU20)[1:6])
MAC20 <- as.vector(summary(tslaMAC20ret)[1:6])
NMAC20 <- as.vector(summary(tslaNotMAC20ret)[1:6])
UData <- data.frame(Stats, U20, MAC20, NMAC20)
flextable(UData)

#Compare Boxplots

par(mfrow = c(1,3))
boxplot(tslaU1, main='Usual 1-day Returns')
boxplot(tslaMAC1ret, main='1-day Returns after MAC')
boxplot(tslaNotMAC1ret, main='1-day Returns after NMAC')

boxplot(tslaU5, main='Usual 5-day Returns')
boxplot(tslaMAC5ret, main='5-day Returns after MAC')
boxplot(tslaNotMAC5ret, main='5-day Returns after NMAC')

boxplot(tslaU10, main='Usual 10-day Returns')
boxplot(tslaMAC10ret, main='10-day Returns after MAC')
boxplot(tslaNotMAC10ret, main='10-day Returns after NMAC')

boxplot(tslaU15, main='Usual 15-day Returns')
boxplot(tslaMAC15ret, main='15-day Returns after MAC')
boxplot(tslaNotMAC15ret, main='15-day Returns after NMAC')

boxplot(tslaU20, main='Usual 20-day Returns')
boxplot(tslaMAC20ret, main='20-day Returns after MAC')
boxplot(tslaNotMAC20ret, main='20-day Returns after NMAC')

# Compare Histograms.

hist(tslaU1, main='Usual 1-day Returns',breaks=40)
hist(tslaMAC1ret, main='1-day Returns after MAC',breaks=10)
hist(tslaNotMAC1ret, main='1-day Returns after NMAC',breaks=40)

hist(tslaU5, main='Usual 5-day Returns',breaks=40)
hist(tslaMAC5ret, main='5-day Returns after MAC',breaks=10)
hist(tslaNotMAC5ret, main='5-day Returns after NMAC',breaks=40)

hist(tslaU10, main='Usual 10-day Returns',breaks=40)
hist(tslaMAC10ret, main='10-day Returns after MAC',breaks=10)
hist(tslaNotMAC10ret, main='10-day Returns after NMAC',breaks=40)

hist(tslaU15, main='Usual 15-day Returns',breaks=40)
hist(tslaMAC15ret, main='15-day Returns after MAC',breaks=10)
hist(tslaNotMAC15ret, main='15-day Returns after NMAC',breaks=40)

hist(tslaU20, main='Usual 20-day Returns',breaks=40)
hist(tslaMAC20ret, main='20-day Returns after MAC',breaks=10)
hist(tslaNotMAC20ret, main='20-day Returns after NMAC',breaks=40)
par(mfrow=c(1,1))

#Check the sample sizes and variances.

Stats <- c('Usual 1-day','1-day After MAC','1-day After NMac')
Lengths <- c(length(tslaU1),length(tslaMAC1ret),length(tslaNotMAC1ret))
Variances <- c(var(tslaU1,na.rm=T),var(tslaMAC1ret,na.rm=T), var(tslaNotMAC1ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 5-day','5-day After MAC','5-day After NMac')
Lengths <- c(length(tslaU5),length(tslaMAC5ret),length(tslaNotMAC5ret))
Variances <- c(var(tslaU5,na.rm=T),var(tslaMAC5ret,na.rm=T), var(tslaNotMAC5ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 10-day','10-day After MAC','10-day After NMac')
Lengths <- c(length(tslaU10),length(tslaMAC10ret),length(tslaNotMAC10ret))
Variances <- c(var(tslaU10,na.rm=T),var(tslaMAC10ret,na.rm=T), var(tslaNotMAC10ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 15-day','15-day After MAC','15-day After NMac')
Lengths <- c(length(tslaU15),length(tslaMAC15ret),length(tslaNotMAC15ret))
Variances <- c(var(tslaU15,na.rm=T),var(tslaMAC15ret,na.rm=T), var(tslaNotMAC15ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 20-day','20-day After MAC','20-day After NMac')
Lengths <- c(length(tslaU20),length(tslaMAC20ret),length(tslaNotMAC20ret))
Variances <- c(var(tslaU20,na.rm=T),var(tslaMAC20ret,na.rm=T), var(tslaNotMAC20ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

# View Quantile-Quantile plots to check normality.

par(mfrow=c(1,3))
qqnorm(tslaU1)
qqline(tslaU1)
qqnorm(tslaMAC1ret)
qqline(tslaMAC1ret)
qqnorm(tslaNotMAC1ret)
qqline(tslaNotMAC1ret)

qqnorm(tslaU5)
qqline(tslaU5)
qqnorm(tslaMAC5ret)
qqline(tslaMAC5ret)
qqnorm(tslaNotMAC5ret)
qqline(tslaNotMAC5ret)

qqnorm(tslaU10)
qqline(tslaU10)
qqnorm(tslaMAC10ret)
qqline(tslaMAC10ret)
qqnorm(tslaNotMAC10ret)
qqline(tslaNotMAC10ret)

qqnorm(tslaU15)
qqline(tslaU15)
qqnorm(tslaMAC15ret)
qqline(tslaMAC15ret)
qqnorm(tslaNotMAC15ret)
qqline(tslaNotMAC15ret)

qqnorm(tslaU20)
qqline(tslaU20)
qqnorm(tslaMAC20ret)
qqline(tslaMAC20ret)
qqnorm(tslaNotMAC20ret)
qqline(tslaNotMAC20ret)
par(mfrow=c(1,1))
# Use Shapiro test to further test normality.
# If p value is less than threshold, reject null hypothesis.
# For shapiro test, the null hypothesis is that the data is normal, and
# the alternative hyptothesis is that the data is not normal.
# If our p value is less than say .05 or .01, we reject that the 
# data is normal.

shapiro.test(tslaU1) #not normal
shapiro.test(tslaMAC1ret) #normal
shapiro.test(tslaNotMAC1ret) #not normal

shapiro.test(tslaU5) #not normal
shapiro.test(tslaMAC5ret) #not normal
shapiro.test(tslaNotMAC5ret) #not normal

shapiro.test(tslaU10) #not normal
shapiro.test(tslaMAC10ret) #not normal
shapiro.test(tslaNotMAC10ret) #not normal

shapiro.test(tslaU15) #not normal
shapiro.test(tslaMAC15ret) #not normal
shapiro.test(tslaNotMAC15ret) #not normal

shapiro.test(tslaU20) #not normal
shapiro.test(tslaMAC20ret) #normal
shapiro.test(tslaNotMAC20ret) #not normal

# Conduct t-test with Welch test for unequal variances.
# If p value is less than threshold, reject the null hypothesis.
# For the Welch Two Sample T-Test, the null hypothesis is that the
# true difference in the means is equal to zero (equal means), otherwise
# the means are diferent.

# Welch test is for unequal variances.

t.test(tslaU1,tslaMAC1ret, Welch=T) #depends if we use p=0.1 or p=0.05..
t.test(tslaU1,tslaNotMAC1ret, Welch=T) #variances are close! (.15 dif)

t.test(tslaU5, tslaMAC5ret, Welch=T)
t.test(tslaU5, tslaNotMAC5ret, Welch=T)

t.test(tslaU10, tslaMAC10ret, Welch=T)
t.test(tslaU10, tslaNotMAC10ret, Welch=T) #variances are close-ish (.44 dif)
t.test(tslaU10, tslaNotMAC10ret, Welch=F)

t.test(tslaU15, tslaMAC15ret, Welch=T)
t.test(tslaU15, tslaNotMAC15ret, Welch=T)

t.test(tslaU20, tslaMAC20ret, Welch=T)
t.test(tslaU20, tslaNotMAC20ret, Welch=T)

Stats <- c("Sample Size", "Variance")
dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU1, length(tslaMAC1ret))
  n <- c(length(su),length(tslaMAC1ret))
  variance <- c(var(su),var(tslaMAC1ret))
  dif <- abs(var(su) - var(tslaMAC1ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

# Wilcox test is for when we have questionably normal data, that is 
# the data non-parametric. This requires that we have the same sample
# size and the same variance. If the p value is less than threshold,
# we reject the null hypothesis, and thus we do not have the same distribution
# with equal medians.

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC1ret)
wilcox.test(su, tslaMAC1ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU5, length(tslaMAC5ret))
  n <- c(length(su),length(tslaMAC5ret))
  variance <- c(var(su),var(tslaMAC5ret))
  dif <- abs(var(su) - var(tslaMAC5ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC5ret)
wilcox.test(su, tslaMAC5ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU10, length(tslaMAC10ret))
  n <- c(length(su),length(tslaMAC10ret))
  variance <- c(var(su),var(tslaMAC10ret))
  dif <- abs(var(su) - var(tslaMAC10ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC10ret)
wilcox.test(su, tslaMAC10ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU15, length(tslaMAC15ret))
  n <- c(length(su),length(tslaMAC15ret))
  variance <- c(var(su),var(tslaMAC15ret))
  dif <- abs(var(su) - var(tslaMAC15ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC15ret)
wilcox.test(su, tslaMAC15ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU20, length(tslaMAC20ret))
  n <- c(length(su),length(tslaMAC20ret))
  variance <- c(var(su),var(tslaMAC20ret))
  dif <- abs(var(su) - var(tslaMAC20ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC20ret)
wilcox.test(su, tslaMAC20ret)


#1b) Using two moving averages other than 10 and 50 day.
#Compute the Moving Averages (100 Day and 150 Day)

tsla100day <- c()
imax <- length(tslats)

for (i in 100:imax)
{
  jmin <- (i - 99)
  tsla100day[i] <- 0
  for (j in jmin:i)
  {
    tsla100day[i] <- tsla100day[i] + tslats[j]
  }
  tsla100day[i] <- tsla100day[i]/100
}

tsla150day <- c()
imax <- length(tslats)

for (i in 150:imax)
{
  jmin <- (i - 149)
  tsla150day[i] <- 0
  for (j in jmin:i)
  {
    tsla150day[i] <- tsla150day[i] + tslats[j]
  }
  tsla150day[i] <- tsla150day[i]/150
}

plot(tslada[1:imax], tslats[1:imax], 
     type='l',
     main="100 Day and 150 Day SMA for TSLA (Alltime)",
     xlab="Date",
     ylab="Closing Price",
     col='black')
lines(tslada[50:imax], tsla50day[50:imax], type='l', col='blue')
lines(tslada[10:imax], tsla10day[10:imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Closing Price','150 Day','100 Day'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[1:200], tslats[1:200], 
     type='l',
     main="100 Day and 150 Day SMA for TSLA (Zoomed in)",
     xlab="Date",
     ylab="Closing Price",
     col='black')
lines(tslada[50:imax], tsla50day[50:imax], type='l', col='blue')
lines(tslada[10:imax], tsla10day[10:imax], type='l', col='red')
legend('topleft', inset=.05,
       legend=c('Closing Price','150 Day','100 Day'), 
       col=c('black','blue','red'),
       lwd=.5)

#Calculate Moving Average Crossovers between the
#100 day and 150 day SMAs and getting 1, 5, 10, 15 and 20 days after.
# Also compute usual 1, 5, 10, 15, and 20 days after.

icmax <- (imax-1)
count <- 0
countNot <- 0

tslaMAC1ret <- c()
tslaMAC5ret <- c()
tslaMAC10ret <- c()
tslaMAC15ret <- c()
tslaMAC20ret <- c()

tslaNotMAC1ret <- c()
tslaNotMAC5ret <- c()
tslaNotMAC10ret <- c()
tslaNotMAC15ret <- c()
tslaNotMAC20ret <- c()

tslaU1 <- c()
tslaU5 <- c()
tslaU10 <- c()
tslaU15 <- c()
tslaU20 <- c()

tslaMACda <- as.Date(c())
tslaNotMACda <- as.Date(c())

for (i in 151:icmax)
{
  if (tsla100day[i-1] < tsla150day[i-1] && tsla100day[i] > tsla150day[i])
  {
    count <- count + 1
    tslaMAC1ret[count] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
    tslaMAC5ret[count] <- ((tslats[i+5] - tslats[i])/tslats[i])*100
    tslaMAC10ret[count] <- ((tslats[i+10] - tslats[i])/tslats[i])*100
    tslaMAC15ret[count] <- ((tslats[i+15] - tslats[i])/tslats[i])*100
    tslaMAC20ret[count] <- ((tslats[i+20] - tslats[i])/tslats[i])*100
    tslaMACda[count] <- tslada[i]
  }
  else
  {
    countNot <- countNot + 1
    tslaNotMAC1ret[countNot] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
    tslaNotMAC5ret[countNot] <- ((tslats[i+5] - tslats[i])/tslats[i])*100
    tslaNotMAC10ret[countNot] <- ((tslats[i+10] - tslats[i])/tslats[i])*100
    tslaNotMAC15ret[countNot] <- ((tslats[i+15] - tslats[i])/tslats[i])*100
    tslaNotMAC20ret[countNot] <- ((tslats[i+20] - tslats[i])/tslats[i])*100
    tslaNotMACda[countNot] <- tslada[i]
  }
}

for (i in 1:icmax)
{
  tslaU1[i] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
  tslaU5[i] <- ((tslats[i+5] - tslats[i])/tslats[i])*100
  tslaU10[i] <- ((tslats[i+10] - tslats[i])/tslats[i])*100
  tslaU15[i] <- ((tslats[i+15] - tslats[i])/tslats[i])*100
  tslaU20[i] <- ((tslats[i+20] - tslats[i])/tslats[i])*100
}

#Plot them.

plot(tslaMACda, tslaMAC1ret,
     main='1 Day After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC5ret,
     main='5 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC10ret,
     main='10 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC15ret,
     main='15 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC20ret,
     main='20 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)

#Compute summary statistics for each vector of returns.

Stats <- c('Min','Q1','Q2','Mean','Q3','Max')

U1 <- as.vector(summary(tslaU1)[1:6])
MAC1 <- as.vector(summary(tslaMAC1ret)[1:6])
NMAC1 <- as.vector(summary(tslaNotMAC1ret)[1:6])
UData <- data.frame(Stats, U1, MAC1, NMAC1)
flextable(UData)

U5 <- as.vector(summary(tslaU5)[1:6])
MAC5 <- as.vector(summary(tslaMAC5ret)[1:6])
NMAC5 <- as.vector(summary(tslaNotMAC5ret)[1:6])
UData <- data.frame(Stats, U5, MAC5, NMAC5)
flextable(UData)

U10 <- as.vector(summary(tslaU10)[1:6])
MAC10 <- as.vector(summary(tslaMAC10ret)[1:6])
NMAC10 <- as.vector(summary(tslaNotMAC10ret)[1:6])
UData <- data.frame(Stats, U10, MAC10, NMAC10)
flextable(UData)

U15 <- as.vector(summary(tslaU15)[1:6])
MAC15 <- as.vector(summary(tslaMAC15ret)[1:6])
NMAC15 <- as.vector(summary(tslaNotMAC15ret)[1:6])
UData <- data.frame(Stats, U15, MAC15, NMAC15)
flextable(UData)

U20 <- as.vector(summary(tslaU20)[1:6])
MAC20 <- as.vector(summary(tslaMAC20ret)[1:6])
NMAC20 <- as.vector(summary(tslaNotMAC20ret)[1:6])
UData <- data.frame(Stats, U20, MAC20, NMAC20)
flextable(UData)

#Compare Boxplots

par(mfrow = c(1,3))
boxplot(tslaU1, main='Usual 1-day Returns')
boxplot(tslaMAC1ret, main='1-day Returns after MAC')
boxplot(tslaNotMAC1ret, main='1-day Returns after NMAC')

boxplot(tslaU5, main='Usual 5-day Returns')
boxplot(tslaMAC5ret, main='5-day Returns after MAC')
boxplot(tslaNotMAC5ret, main='5-day Returns after NMAC')

boxplot(tslaU10, main='Usual 10-day Returns')
boxplot(tslaMAC10ret, main='10-day Returns after MAC')
boxplot(tslaNotMAC10ret, main='10-day Returns after NMAC')

boxplot(tslaU15, main='Usual 15-day Returns')
boxplot(tslaMAC15ret, main='15-day Returns after MAC')
boxplot(tslaNotMAC15ret, main='15-day Returns after NMAC')

boxplot(tslaU20, main='Usual 20-day Returns')
boxplot(tslaMAC20ret, main='20-day Returns after MAC')
boxplot(tslaNotMAC20ret, main='20-day Returns after NMAC')

# Compare Histograms.

hist(tslaU1, main='Usual 1-day Returns',breaks=40)
hist(tslaMAC1ret, main='1-day Returns after MAC',breaks=10)
hist(tslaNotMAC1ret, main='1-day Returns after NMAC',breaks=40)

hist(tslaU5, main='Usual 5-day Returns',breaks=40)
hist(tslaMAC5ret, main='5-day Returns after MAC',breaks=10)
hist(tslaNotMAC5ret, main='5-day Returns after NMAC',breaks=40)

hist(tslaU10, main='Usual 10-day Returns',breaks=40)
hist(tslaMAC10ret, main='10-day Returns after MAC',breaks=10)
hist(tslaNotMAC10ret, main='10-day Returns after NMAC',breaks=40)

hist(tslaU15, main='Usual 15-day Returns',breaks=40)
hist(tslaMAC15ret, main='15-day Returns after MAC',breaks=10)
hist(tslaNotMAC15ret, main='15-day Returns after NMAC',breaks=40)

hist(tslaU20, main='Usual 20-day Returns',breaks=40)
hist(tslaMAC20ret, main='20-day Returns after MAC',breaks=10)
hist(tslaNotMAC20ret, main='20-day Returns after NMAC',breaks=40)
par(mfrow=c(1,1))

#Check the sample sizes and variances.

Stats <- c('Usual 1-day','1-day After MAC','1-day After NMac')
Lengths <- c(length(tslaU1),length(tslaMAC1ret),length(tslaNotMAC1ret))
Variances <- c(var(tslaU1,na.rm=T),var(tslaMAC1ret,na.rm=T), var(tslaNotMAC1ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 5-day','5-day After MAC','5-day After NMac')
Lengths <- c(length(tslaU5),length(tslaMAC5ret),length(tslaNotMAC5ret))
Variances <- c(var(tslaU5,na.rm=T),var(tslaMAC5ret,na.rm=T), var(tslaNotMAC5ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 10-day','10-day After MAC','10-day After NMac')
Lengths <- c(length(tslaU10),length(tslaMAC10ret),length(tslaNotMAC10ret))
Variances <- c(var(tslaU10,na.rm=T),var(tslaMAC10ret,na.rm=T), var(tslaNotMAC10ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 15-day','15-day After MAC','15-day After NMac')
Lengths <- c(length(tslaU15),length(tslaMAC15ret),length(tslaNotMAC15ret))
Variances <- c(var(tslaU15,na.rm=T),var(tslaMAC15ret,na.rm=T), var(tslaNotMAC15ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 20-day','20-day After MAC','20-day After NMac')
Lengths <- c(length(tslaU20),length(tslaMAC20ret),length(tslaNotMAC20ret))
Variances <- c(var(tslaU20,na.rm=T),var(tslaMAC20ret,na.rm=T), var(tslaNotMAC20ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

# View Quantile-Quantile plots to check normality.

par(mfrow=c(1,3))
qqnorm(tslaU1)
qqline(tslaU1)
qqnorm(tslaMAC1ret)
qqline(tslaMAC1ret)
qqnorm(tslaNotMAC1ret)
qqline(tslaNotMAC1ret)

qqnorm(tslaU5)
qqline(tslaU5)
qqnorm(tslaMAC5ret)
qqline(tslaMAC5ret)
qqnorm(tslaNotMAC5ret)
qqline(tslaNotMAC5ret)

qqnorm(tslaU10)
qqline(tslaU10)
qqnorm(tslaMAC10ret)
qqline(tslaMAC10ret)
qqnorm(tslaNotMAC10ret)
qqline(tslaNotMAC10ret)

qqnorm(tslaU15)
qqline(tslaU15)
qqnorm(tslaMAC15ret)
qqline(tslaMAC15ret)
qqnorm(tslaNotMAC15ret)
qqline(tslaNotMAC15ret)

qqnorm(tslaU20)
qqline(tslaU20)
qqnorm(tslaMAC20ret)
qqline(tslaMAC20ret)
qqnorm(tslaNotMAC20ret)
qqline(tslaNotMAC20ret)
par(mfrow=c(1,1))
# Use Shapiro test to further test normality.

shapiro.test(tslaU1) #not normal
shapiro.test(tslaMAC1ret) #normal
shapiro.test(tslaNotMAC1ret) #not normal

shapiro.test(tslaU5) #not normal
shapiro.test(tslaMAC5ret) #not normal
shapiro.test(tslaNotMAC5ret) #not normal

shapiro.test(tslaU10) #not normal
shapiro.test(tslaMAC10ret) #not normal
shapiro.test(tslaNotMAC10ret) #not normal

shapiro.test(tslaU15) #not normal
shapiro.test(tslaMAC15ret) #not normal
shapiro.test(tslaNotMAC15ret) #not normal

shapiro.test(tslaU20) #not normal
shapiro.test(tslaMAC20ret) #normal
shapiro.test(tslaNotMAC20ret) #not normal

#Conduct t-test with Welch test for unequal varainces.

t.test(tslaU1,tslaMAC1ret, Welch=T) #depends if we use p=0.1 or p=0.05..
t.test(tslaU1,tslaNotMAC1ret, Welch=T) #variances are close! (.15 dif)

t.test(tslaU5, tslaMAC5ret, Welch=T)
t.test(tslaU5, tslaNotMAC5ret, Welch=T)

t.test(tslaU10, tslaMAC10ret, Welch=T)
t.test(tslaU10, tslaNotMAC10ret, Welch=T) #variances are close-ish (.44 dif)
t.test(tslaU10, tslaNotMAC10ret, Welch=F)

t.test(tslaU15, tslaMAC15ret, Welch=T)
t.test(tslaU15, tslaNotMAC15ret, Welch=T)

t.test(tslaU20, tslaMAC20ret, Welch=T)
t.test(tslaU20, tslaNotMAC20ret, Welch=T)

Stats <- c("Sample Size", "Variance")
dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU1, length(tslaMAC1ret))
  n <- c(length(su),length(tslaMAC1ret))
  variance <- c(var(su),var(tslaMAC1ret))
  dif <- abs(var(su) - var(tslaMAC1ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC1ret)
wilcox.test(su, tslaMAC1ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU5, length(tslaMAC5ret))
  n <- c(length(su),length(tslaMAC5ret))
  variance <- c(var(su),var(tslaMAC5ret))
  dif <- abs(var(su) - var(tslaMAC5ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC5ret)
wilcox.test(su, tslaMAC5ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU10, length(tslaMAC10ret))
  n <- c(length(su),length(tslaMAC10ret))
  variance <- c(var(su),var(tslaMAC10ret))
  dif <- abs(var(su) - var(tslaMAC10ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC10ret)
wilcox.test(su, tslaMAC10ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU15, length(tslaMAC15ret))
  n <- c(length(su),length(tslaMAC15ret))
  variance <- c(var(su),var(tslaMAC15ret))
  dif <- abs(var(su) - var(tslaMAC15ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC15ret)
wilcox.test(su, tslaMAC15ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU20, length(tslaMAC20ret))
  n <- c(length(su),length(tslaMAC20ret))
  variance <- c(var(su),var(tslaMAC20ret))
  dif <- abs(var(su) - var(tslaMAC20ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC20ret)
wilcox.test(su, tslaMAC20ret)

#1c) Perform the same analysis on the Moving Average Envelope.

#Compute the Moving Averages (10 Day and 50 Day) with 
# +/- 2% envelopes.

tsla10day <- c()
tsla10dayAbove <- c()
tsla10dayBelow <- c()
imax <- length(tslats)

for (i in 10:imax)
{
  jmin <- (i - 9)
  tsla10day[i] <- 0
  tsla10dayBelow[i] <- 0
  tsla10dayAbove[i] <- 0
  for (j in jmin:i)
  {
    tsla10day[i] <- tsla10day[i] + tslats[j]
  }
  tsla10day[i] <- tsla10day[i]/10
  tsla10dayBelow[i] <- .98*tsla10day[i]
  tsla10dayAbove[i] <- 1.02*tsla10day[i]
}

tsla50day <- c()
tsla50dayBelow <- c()
tsla50dayAbove <- c()
imax <- length(tslats)

for (i in 50:imax)
{
  jmin <- (i - 49)
  tsla50day[i] <- 0
  tsla50dayBelow[i] <- 0
  tsla50dayAbove[i] <- 0
  for (j in jmin:i)
  {
    tsla50day[i] <- tsla50day[i] + tslats[j]
  }
  tsla50day[i] <- tsla50day[i]/50
  tsla50dayBelow[i] <- .98*tsla50day[i]
  tsla50dayAbove[i] <- 1.02*tsla50day[i]
}

plot(tslada[1:imax], tslats[1:imax], 
     type='l',
     main="10 Day MA with +/- 2% Envelope",
     xlab="Date",
     ylab="Closing Price",
     col='black')
lines(tslada[10:imax], tsla10dayBelow[10:imax], type='l', col='blue')
lines(tslada[10:imax], tsla10dayAbove[10:imax], type='l', col='red')
lines(tslada[10:imax], tsla10day[10:imax],type='l',col='red')
legend('topleft', inset=.05,
       legend=c('Closing Price','2% Envelope','10 Day MA'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[1:200], tslats[1:200], 
     type='l',
     main="10 Day MA with +/- 2% Envelope (Zoomed in)",
     xlab="Date",
     ylab="Closing Price",
     col='black')
lines(tslada[10:imax], tsla10dayBelow[10:imax], type='l', col='blue')
lines(tslada[10:imax], tsla10day[10:imax], type='l', col='red')
lines(tslada[10:imax], tsla10dayAbove[10:imax], type='l', col='blue')
legend('topleft', inset=.05,
       legend=c('Closing Price','2% Envelope','10 Day MA'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[1:imax], tslats[1:imax], 
     type='l',
     main="50 Day MA with +/- 2% Envelope",
     xlab="Date",
     ylab="Closing Price",
     col='black')
lines(tslada[50:imax], tsla50dayBelow[50:imax], type='l', col='blue')
lines(tslada[50:imax], tsla50dayAbove[50:imax], type='l', col='blue')
lines(tslada[50:imax], tsla50day[50:imax],type='l',col='red')
legend('topleft', inset=.05,
       legend=c('Closing Price','2% Envelope','50 Day MA'), 
       col=c('black','blue','red'),
       lwd=.5)

plot(tslada[1:200], tslats[1:200], 
     type='l',
     main="50 Day MA with +/- 2% Envelope (Zoomed in)",
     xlab="Date",
     ylab="Closing Price",
     col='black')
lines(tslada[50:imax], tsla50dayBelow[50:imax], type='l', col='blue')
lines(tslada[50:imax], tsla50day[50:imax], type='l', col='red')
lines(tslada[50:imax], tsla50dayAbove[50:imax], type='l', col='blue')
legend('topleft', inset=.05,
       legend=c('Closing Price','2% Envelope','50 Day MA'), 
       col=c('black','blue','red'),
       lwd=.5)

#Calculate Moving Average Crossovers between the
#10 day upper envelope and closing price and 
#getting 1, 5, 10, 15 and 20 days after.
# Also compute usual 1, 5, 10, 15, and 20 days after.

icmax <- (imax-1)
count <- 0
countNot <- 0

tslaMAC1ret <- c()
tslaMAC5ret <- c()
tslaMAC10ret <- c()
tslaMAC15ret <- c()
tslaMAC20ret <- c()

tslaNotMAC1ret <- c()
tslaNotMAC5ret <- c()
tslaNotMAC10ret <- c()
tslaNotMAC15ret <- c()
tslaNotMAC20ret <- c()

tslaU1 <- c()
tslaU5 <- c()
tslaU10 <- c()
tslaU15 <- c()
tslaU20 <- c()

tslaMACda <- as.Date(c())
tslaNotMACda <- as.Date(c())

for (i in 11:icmax)
{
  if (tsla10dayAbove[i-1] < tslats[i-1] && tsla10dayAbove[i] > tslats[i])
  {
    count <- count + 1
    tslaMAC1ret[count] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
    tslaMAC5ret[count] <- ((tslats[i+5] - tslats[i])/tslats[i])*100
    tslaMAC10ret[count] <- ((tslats[i+10] - tslats[i])/tslats[i])*100
    tslaMAC15ret[count] <- ((tslats[i+15] - tslats[i])/tslats[i])*100
    tslaMAC20ret[count] <- ((tslats[i+20] - tslats[i])/tslats[i])*100
    tslaMACda[count] <- tslada[i]
  }
  else
  {
    countNot <- countNot + 1
    tslaNotMAC1ret[countNot] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
    tslaNotMAC5ret[countNot] <- ((tslats[i+5] - tslats[i])/tslats[i])*100
    tslaNotMAC10ret[countNot] <- ((tslats[i+10] - tslats[i])/tslats[i])*100
    tslaNotMAC15ret[countNot] <- ((tslats[i+15] - tslats[i])/tslats[i])*100
    tslaNotMAC20ret[countNot] <- ((tslats[i+20] - tslats[i])/tslats[i])*100
    tslaNotMACda[countNot] <- tslada[i]
  }
}

for (i in 1:icmax)
{
  tslaU1[i] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
  tslaU5[i] <- ((tslats[i+5] - tslats[i])/tslats[i])*100
  tslaU10[i] <- ((tslats[i+10] - tslats[i])/tslats[i])*100
  tslaU15[i] <- ((tslats[i+15] - tslats[i])/tslats[i])*100
  tslaU20[i] <- ((tslats[i+20] - tslats[i])/tslats[i])*100
}

#Plot them. NOTE: MAC implies the upper bound of 
# the envelope crosses the closing price!!

plot(tslaMACda, tslaMAC1ret,
     main='1 Day After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC5ret,
     main='5 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC10ret,
     main='10 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC15ret,
     main='15 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)
plot(tslaMACda, tslaMAC20ret,
     main='20 Days After MAC',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)

#Compute summary statistics for each vector of returns.

Stats <- c('Min','Q1','Q2','Mean','Q3','Max')

U1 <- as.vector(summary(tslaU1)[1:6])
MAC1 <- as.vector(summary(tslaMAC1ret)[1:6])
NMAC1 <- as.vector(summary(tslaNotMAC1ret)[1:6])
UData <- data.frame(Stats, U1, MAC1, NMAC1)
flextable(UData)

U5 <- as.vector(summary(tslaU5)[1:6])
MAC5 <- as.vector(summary(tslaMAC5ret)[1:6])
NMAC5 <- as.vector(summary(tslaNotMAC5ret)[1:6])
UData <- data.frame(Stats, U5, MAC5, NMAC5)
flextable(UData)

U10 <- as.vector(summary(tslaU10)[1:6])
MAC10 <- as.vector(summary(tslaMAC10ret)[1:6])
NMAC10 <- as.vector(summary(tslaNotMAC10ret)[1:6])
UData <- data.frame(Stats, U10, MAC10, NMAC10)
flextable(UData)

U15 <- as.vector(summary(tslaU15)[1:6])
MAC15 <- as.vector(summary(tslaMAC15ret)[1:6])
NMAC15 <- as.vector(summary(tslaNotMAC15ret)[1:6])
UData <- data.frame(Stats, U15, MAC15, NMAC15)
flextable(UData)

U20 <- as.vector(summary(tslaU20)[1:6])
MAC20 <- as.vector(summary(tslaMAC20ret)[1:6])
NMAC20 <- as.vector(summary(tslaNotMAC20ret)[1:6])
UData <- data.frame(Stats, U20, MAC20, NMAC20)
flextable(UData)

#Compare Boxplots

par(mfrow = c(1,3))
boxplot(tslaU1, main='Usual 1-day Returns')
boxplot(tslaMAC1ret, main='1-day Returns after MAC')
boxplot(tslaNotMAC1ret, main='1-day Returns after NMAC')

boxplot(tslaU5, main='Usual 5-day Returns')
boxplot(tslaMAC5ret, main='5-day Returns after MAC')
boxplot(tslaNotMAC5ret, main='5-day Returns after NMAC')

boxplot(tslaU10, main='Usual 10-day Returns')
boxplot(tslaMAC10ret, main='10-day Returns after MAC')
boxplot(tslaNotMAC10ret, main='10-day Returns after NMAC')

boxplot(tslaU15, main='Usual 15-day Returns')
boxplot(tslaMAC15ret, main='15-day Returns after MAC')
boxplot(tslaNotMAC15ret, main='15-day Returns after NMAC')

boxplot(tslaU20, main='Usual 20-day Returns')
boxplot(tslaMAC20ret, main='20-day Returns after MAC')
boxplot(tslaNotMAC20ret, main='20-day Returns after NMAC')

# Compare Histograms.

hist(tslaU1, main='Usual 1-day Returns',breaks=40)
hist(tslaMAC1ret, main='1-day Returns after MAC',breaks=10)
hist(tslaNotMAC1ret, main='1-day Returns after NMAC',breaks=40)

hist(tslaU5, main='Usual 5-day Returns',breaks=40)
hist(tslaMAC5ret, main='5-day Returns after MAC',breaks=10)
hist(tslaNotMAC5ret, main='5-day Returns after NMAC',breaks=40)

hist(tslaU10, main='Usual 10-day Returns',breaks=40)
hist(tslaMAC10ret, main='10-day Returns after MAC',breaks=10)
hist(tslaNotMAC10ret, main='10-day Returns after NMAC',breaks=40)

hist(tslaU15, main='Usual 15-day Returns',breaks=40)
hist(tslaMAC15ret, main='15-day Returns after MAC',breaks=10)
hist(tslaNotMAC15ret, main='15-day Returns after NMAC',breaks=40)

hist(tslaU20, main='Usual 20-day Returns',breaks=40)
hist(tslaMAC20ret, main='20-day Returns after MAC',breaks=10)
hist(tslaNotMAC20ret, main='20-day Returns after NMAC',breaks=40)
par(mfrow=c(1,1))

#Check the sample sizes and variances.

Stats <- c('Usual 1-day','1-day After MAC','1-day After NMac')
Lengths <- c(length(tslaU1),length(tslaMAC1ret),length(tslaNotMAC1ret))
Variances <- c(var(tslaU1,na.rm=T),var(tslaMAC1ret,na.rm=T), var(tslaNotMAC1ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 5-day','5-day After MAC','5-day After NMac')
Lengths <- c(length(tslaU5),length(tslaMAC5ret),length(tslaNotMAC5ret))
Variances <- c(var(tslaU5,na.rm=T),var(tslaMAC5ret,na.rm=T), var(tslaNotMAC5ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 10-day','10-day After MAC','10-day After NMac')
Lengths <- c(length(tslaU10),length(tslaMAC10ret),length(tslaNotMAC10ret))
Variances <- c(var(tslaU10,na.rm=T),var(tslaMAC10ret,na.rm=T), var(tslaNotMAC10ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 15-day','15-day After MAC','15-day After NMac')
Lengths <- c(length(tslaU15),length(tslaMAC15ret),length(tslaNotMAC15ret))
Variances <- c(var(tslaU15,na.rm=T),var(tslaMAC15ret,na.rm=T), var(tslaNotMAC15ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 20-day','20-day After MAC','20-day After NMac')
Lengths <- c(length(tslaU20),length(tslaMAC20ret),length(tslaNotMAC20ret))
Variances <- c(var(tslaU20,na.rm=T),var(tslaMAC20ret,na.rm=T), var(tslaNotMAC20ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

# View Quantile-Quantile plots to check normality.

par(mfrow=c(1,3))
qqnorm(tslaU1)
qqline(tslaU1)
qqnorm(tslaMAC1ret)
qqline(tslaMAC1ret)
qqnorm(tslaNotMAC1ret)
qqline(tslaNotMAC1ret)

qqnorm(tslaU5)
qqline(tslaU5)
qqnorm(tslaMAC5ret)
qqline(tslaMAC5ret)
qqnorm(tslaNotMAC5ret)
qqline(tslaNotMAC5ret)

qqnorm(tslaU10)
qqline(tslaU10)
qqnorm(tslaMAC10ret)
qqline(tslaMAC10ret)
qqnorm(tslaNotMAC10ret)
qqline(tslaNotMAC10ret)

qqnorm(tslaU15)
qqline(tslaU15)
qqnorm(tslaMAC15ret)
qqline(tslaMAC15ret)
qqnorm(tslaNotMAC15ret)
qqline(tslaNotMAC15ret)

qqnorm(tslaU20)
qqline(tslaU20)
qqnorm(tslaMAC20ret)
qqline(tslaMAC20ret)
qqnorm(tslaNotMAC20ret)
qqline(tslaNotMAC20ret)
par(mfrow=c(1,1))
# Use Shapiro test to further test normality.

shapiro.test(tslaU1) #not normal
shapiro.test(tslaMAC1ret) #normal
shapiro.test(tslaNotMAC1ret) #not normal

shapiro.test(tslaU5) #not normal
shapiro.test(tslaMAC5ret) #not normal
shapiro.test(tslaNotMAC5ret) #not normal

shapiro.test(tslaU10) #not normal
shapiro.test(tslaMAC10ret) #not normal
shapiro.test(tslaNotMAC10ret) #not normal

shapiro.test(tslaU15) #not normal
shapiro.test(tslaMAC15ret) #not normal
shapiro.test(tslaNotMAC15ret) #not normal

shapiro.test(tslaU20) #not normal
shapiro.test(tslaMAC20ret) #normal
shapiro.test(tslaNotMAC20ret) #not normal

#Conduct t-test with Welch test for unequal varainces.

t.test(tslaU1,tslaMAC1ret, Welch=T) #depends if we use p=0.1 or p=0.05..
t.test(tslaU1,tslaNotMAC1ret, Welch=T) #variances are close! (.15 dif)

t.test(tslaU5, tslaMAC5ret, Welch=T)
t.test(tslaU5, tslaNotMAC5ret, Welch=T)

t.test(tslaU10, tslaMAC10ret, Welch=T)
t.test(tslaU10, tslaNotMAC10ret, Welch=T) #variances are close-ish (.44 dif)

t.test(tslaU15, tslaMAC15ret, Welch=T)
t.test(tslaU15, tslaNotMAC15ret, Welch=T)

t.test(tslaU20, tslaMAC20ret, Welch=T)
t.test(tslaU20, tslaNotMAC20ret, Welch=T)

wilcox.test(tslaU1, tslaMAC1ret)
wilcox.test(tslaU1, tslaNotMAC1ret)

wilcox.test(tslaU5, tslaMAC5ret)
wilcox.test(tslaU5, tslaNotMAC5ret)

wilcox.test(tslaU10, tslaMAC10ret)
wilcox.test(tslaU10, tslaNotMAC10ret)

wilcox.test(tslaU15, tslaMAC15ret)
wilcox.test(tslaU15, tslaNotMAC15ret)

wilcox.test(tslaU20, tslaMAC20ret)
wilcox.test(tslaU20, tslaNotMAC20ret)

Stats <- c("Sample Size", "Variance")
dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU1, length(tslaMAC1ret))
  n <- c(length(su),length(tslaMAC1ret))
  variance <- c(var(su),var(tslaMAC1ret))
  dif <- abs(var(su) - var(tslaMAC1ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC1ret)
wilcox.test(su, tslaMAC1ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU5, length(tslaMAC5ret))
  n <- c(length(su),length(tslaMAC5ret))
  variance <- c(var(su),var(tslaMAC5ret))
  dif <- abs(var(su) - var(tslaMAC5ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC5ret)
wilcox.test(su, tslaMAC5ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU10, length(tslaMAC10ret))
  n <- c(length(su),length(tslaMAC10ret))
  variance <- c(var(su),var(tslaMAC10ret))
  dif <- abs(var(su) - var(tslaMAC10ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC10ret)
wilcox.test(su, tslaMAC10ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU15, length(tslaMAC15ret))
  n <- c(length(su),length(tslaMAC15ret))
  variance <- c(var(su),var(tslaMAC15ret))
  dif <- abs(var(su) - var(tslaMAC15ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC15ret)
wilcox.test(su, tslaMAC15ret)

dif <- 1
tol <- 0.01
count <- 0
while (dif > tol)
{
  count <- count + 1
  su <- sample(tslaU20, length(tslaMAC20ret))
  n <- c(length(su),length(tslaMAC20ret))
  variance <- c(var(su),var(tslaMAC20ret))
  dif <- abs(var(su) - var(tslaMAC20ret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)

qqnorm(su)
qqline(su)
shapiro.test(su)
shapiro.test(tslaMAC20ret)
wilcox.test(su, tslaMAC20ret)




