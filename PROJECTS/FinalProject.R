#Author: Mitchell Krystofiak
#Class: Math 490 - Final Project
#Date: Decemeber 15, 2021

#Project Goals
#*************#
# 1. Perform technical analysis on either one or some of:
#    (a) Relative Strength Index (RSI)
#    (b) On-Balance Volume (OBV)
#
# 2. Fundamental Analysis:
#    (a) Price-to-Earnings ratio,
#    (b) Price-to-Earnings ratio + company growth
# 
# 3. Perform a basic rating procedure on stocks using the
#    Markov Method, Massey Method, and/or Colley Method to
#    rate stocks based on Outstanding Shares, Stock Prices,
#    or something similar to make a mock 'PD' or 'WP'.

#Goal 1a: Compute RSI and perform technical analysis

library(flextable)
tsla <- read.csv("./TSLA.csv")

ft <- flextable(head(tsla))
ft <- set_caption(ft, caption='Tesla Stock Data')
ft <- theme_vader(ft)
ft

#Fix dates

tsla$Date <- as.Date(tsla$Date, format="%Y-%m-%d")

#Select out returns

tslats <- data.frame(tsla[,c(1,5)])
tslada <- tsla[,1]
tslaClose <- tsla[,5]

#Compute Change in Close prices.

change <- c()
imax <- length(tslaClose)

for (i in 1:(imax-1))
{
  change[i] <- tslaClose[i+1] - tslaClose[i]
}

# Determine if gain or loss.

gain <- c()
loss <- c()

for (i in 1:(imax-1))
{
  if (change[i] > 0)
  {
    gain[i] <- change[i]
    loss[i] <- 0
  }
  else
  {
    loss[i] <- abs(change[i])
    gain[i] <- 0
  }
}

#Compute Average Gain and Average Loss over 14 day period.

avgGain <- c()
avgLoss <- c()

for (i in 14:(imax-1))
{
  jmin <- (i - 13)
  avgGain[i] <- 0
  avgLoss[i] <- 0
  for (j in jmin:i)
  {
    avgGain[i] <- avgGain[i] + gain[j]
    avgLoss[i] <- avgLoss[i] + loss[j]
  }
  avgGain[i] <- avgGain[i]/14
  avgLoss[i] <- avgLoss[i]/14
}

# Compute Relative Strength = AvgGain/AvgLoss

RS <- c()

for (i in 1:(imax-1))
{
  RS[i] <- avgGain[i]/avgLoss[i]
}

#Compute Relative Strength Index
# RSI = if (avgloss = 0 -> 100, else 100 - (100/(1+RS)))

RSI <- c()

for (i in 14:(imax-1))
{
  if (avgLoss[i] == 0)
  {
    RSI[i] <- 100
  }
  else
  {
    RSI[i] <- (100 - (100/(1+RS[i])))
  }
}

#Plot the RSI

val <- 100
par(mfrow = c(2,1))
plot(tslada[1:(imax-1)], tslaClose[1:(imax-1)],
     type ='l',
     main='Closing Price (Alltime)',
     xlab='Date',
     ylab='Price')
plot(tslada[1:(imax-1)], RSI[1:(imax-1)], 
     type='l',
     main="RSI (Alltime)",
     xlab="Date",
     ylab="RSI",
     col='black')
abline(h=30, col='red')
abline(h=70, col='red')

plot(tslada[1:(imax-val-1)], tslaClose[1:(imax-val-1)],
     type ='l',
     main='Closing Price (Zoomed In)',
     xlab='Date',
     ylab='Price')
plot(tslada[(imax-val-1):(imax-1)], RSI[(imax-val-1):(imax-1)], 
     type='l',
     main="RSI (Zoomed In)",
     xlab="Date",
     ylab="RSI",
     col='black')
abline(h=30, col='red')
abline(h=40, col='red')
abline(h=70, col='red')

par(mfrow = c(1,1))

# Now that we have it plotted, we want to consider potential 
# buy signals. RSI values above 70, or whatever threshold is set,
# indicate that the shares are overbought whereas RSI values below 30,
# indicate that the shares are oversold.
#
# If there is an RSI of 25, we can assume that the shares are very likely
# to rise from current levels. If we have an RSI above 70, we are very likely
# to receive downward pressure from the market.

# Though it is likely we could have more overbought shares if the RSI is below
# 30, as the RSI moves lower, this outcome grows increasingly unlikely. 
# Therefore, it is less likely to see an RSI below 20.

# We are going to set a buy signal to be when the RSI value falls below 30.

icmax <- (imax-1)
count <- 0
countNot <- 0

tsla1ret <- c()
tsla5ret <- c()
tsla10ret <- c()
tsla15ret <- c()
tsla20ret <- c()

tslaNot1ret <- c()
tslaNot5ret <- c()
tslaNot10ret <- c()
tslaNot15ret <- c()
tslaNot20ret <- c()

tslaU1 <- c()
tslaU5 <- c()
tslaU10 <- c()
tslaU15 <- c()
tslaU20 <- c()

tslaRSIda <- as.Date(c())
tslaRSINotda <- as.Date(c())

for (i in 21:icmax)
{
  if (RSI[i-1] > 30 && RSI[i] <= 30)
  {
    count <- count + 1
    tsla1ret[count] <- ((tslaClose[i+1] - tslaClose[i])/tslaClose[i])*100
    tsla5ret[count] <- ((tslaClose[i+5] - tslaClose[i])/tslaClose[i])*100
    tsla10ret[count] <- ((tslaClose[i+10] - tslaClose[i])/tslaClose[i])*100
    tsla15ret[count] <- ((tslaClose[i+15] - tslaClose[i])/tslaClose[i])*100
    tsla20ret[count] <- ((tslaClose[i+20] - tslaClose[i])/tslaClose[i])*100
    tslaRSIda[count] <- tslada[i]
  }
  else
  {
    countNot <- countNot + 1
    tslaNot1ret[countNot] <- ((tslaClose[i+1] - tslaClose[i])/tslaClose[i])*100
    tslaNot5ret[countNot] <- ((tslaClose[i+5] - tslaClose[i])/tslaClose[i])*100
    tslaNot10ret[countNot] <- ((tslaClose[i+10] - tslaClose[i])/tslaClose[i])*100
    tslaNot15ret[countNot] <- ((tslaClose[i+15] - tslaClose[i])/tslaClose[i])*100
    tslaNot20ret[countNot] <- ((tslaClose[i+20] - tslaClose[i])/tslaClose[i])*100
    tslaRSINotda[countNot] <- tslada[i]
  }
}

for (i in 1:icmax)
{
  tslaU1[i] <- ((tslaClose[i+1] - tslaClose[i])/tslaClose[i])*100
  tslaU5[i] <- ((tslaClose[i+5] - tslaClose[i])/tslaClose[i])*100
  tslaU10[i] <- ((tslaClose[i+10] - tslaClose[i])/tslaClose[i])*100
  tslaU15[i] <- ((tslaClose[i+15] - tslaClose[i])/tslaClose[i])*100
  tslaU20[i] <- ((tslaClose[i+20] - tslaClose[i])/tslaClose[i])*100
}

plot(tslaRSIda, tsla1ret,
     main='1 Day After RSIC',
     xlab='Dates',
     ylab='Price')
abline(0,0)
plot(tslaRSIda, tsla5ret,
     main='5 Days After RSIC',
     xlab='Dates',
     ylab='Price')
abline(0,0)
plot(tslaRSIda, tsla10ret,
     main='10 Days After RSIC',
     xlab='Dates',
     ylab='Price')
abline(0,0)
plot(tslaRSIda, tsla15ret,
     main='15 Days After RSIC',
     xlab='Dates',
     ylab='Price')
abline(0,0)
plot(tslaRSIda, tsla20ret,
     main='20 Days After RSIC',
     xlab='Dates',
     ylab='Price')
abline(0,0)

#Compute summary statistics for each vector of returns.

Stats <- c('Min','Q1','Q2','Mean','Q3','Max')

U1 <- as.vector(summary(tslaU1)[1:6])
Ret1 <- as.vector(summary(tsla1ret)[1:6])
NRet1 <- as.vector(summary(tslaNot1ret)[1:6])
UData <- data.frame(Stats, U1, Ret1, NRet1)
flextable(UData)

U5 <- as.vector(summary(tslaU5)[1:6])
Ret5 <- as.vector(summary(tsla5ret)[1:6])
NRet5 <- as.vector(summary(tslaNot5ret)[1:6])
UData <- data.frame(Stats, U5, Ret5, NRet5)
flextable(UData)

U10 <- as.vector(summary(tslaU10)[1:6])
Ret10 <- as.vector(summary(tsla10ret)[1:6])
NRet10 <- as.vector(summary(tslaNot10ret)[1:6])
UData <- data.frame(Stats, U10, Ret10, NRet10)
flextable(UData)

U15 <- as.vector(summary(tslaU15)[1:6])
Ret15 <- as.vector(summary(tsla15ret)[1:6])
NRet15 <- as.vector(summary(tslaNot15ret)[1:6])
UData <- data.frame(Stats, U15, Ret15, NRet15)
flextable(UData)

U20 <- as.vector(summary(tslaU20)[1:6])
Ret20 <- as.vector(summary(tsla20ret)[1:6])
NRet20 <- as.vector(summary(tslaNot20ret)[1:6])
UData <- data.frame(Stats, U20, Ret20, NRet20)
flextable(UData)

#Compare Boxplots

par(mfrow = c(1,3))
boxplot(tslaU1, main='Usual 1-day Returns')
boxplot(tsla1ret, main='1-day Returns after RSI = 30 Crossover')
boxplot(tslaNot1ret, main='1-day Returns after a Non-RSI = 30 Crossover')

par(mfrow = c(1,3))
boxplot(tslaU5, main='Usual 5-day Returns')
boxplot(tsla5ret, main='5-day Returns after RSI = 30 Crossover')
boxplot(tslaNot5ret, main='5-day Returns after a Non-RSI = 30 Crossover')

par(mfrow = c(1,3))
boxplot(tslaU10, main='Usual 10-day Returns')
boxplot(tsla10ret, main='10-day Returns after RSI = 30 Crossover')
boxplot(tslaNot10ret, main='10-day Returns after a Non-RSI = 30 Crossover')

par(mfrow = c(1,3))
boxplot(tslaU15, main='Usual 15-day Returns')
boxplot(tsla15ret, main='15-day Returns after RSI = 30 Crossover')
boxplot(tslaNot15ret, main='15-day Returns after a Non-RSI = 30 Crossover')

par(mfrow = c(1,3))
boxplot(tslaU20, main='Usual 20-day Returns')
boxplot(tsla20ret, main='20-day Returns after RSI = 30 Crossover')
boxplot(tslaNot20ret, main='20-day Returns after a Non-RSI = 30 Crossover')
par(mfrow=c(1,1))

# Compare boxplots.

par(mfrow = c(1,3))
boxplot(tslaU1, main='Usual 1-day Returns')
boxplot(tsla1ret, main='1-day Returns after RSIC')
boxplot(tslaNot1ret, main='1-day Returns after NRSIC')

boxplot(tslaU5, main='Usual 5-day Returns')
boxplot(tsla5ret, main='5-day Returns after RSIC')
boxplot(tslaNot5ret, main='5-day Returns after NRSIC')

boxplot(tslaU10, main='Usual 10-day Returns')
boxplot(tsla10ret, main='10-day Returns after RSIC')
boxplot(tslaNot10ret, main='10-day Returns after NRSIC')

boxplot(tslaU15, main='Usual 15-day Returns')
boxplot(tsla15ret, main='15-day Returns after RSIC')
boxplot(tslaNot15ret, main='15-day Returns after NRSIC')

boxplot(tslaU20, main='Usual 20-day Returns')
boxplot(tsla20ret, main='20-day Returns after RSIC')
boxplot(tslaNot20ret, main='20-day Returns after NRSIC')

# Compare histograms.

hist(tslaU1, main='Usual 1-day Returns',breaks=40)
hist(tsla1ret, main='1-day Returns after RSIC',breaks=10)
hist(tslaNot1ret, main='1-day Returns after NRSIC',breaks=40)

hist(tslaU5, main='Usual 5-day Returns',breaks=40)
hist(tsla5ret, main='5-day Returns after RSIC',breaks=10)
hist(tslaNot5ret, main='5-day Returns after NRSIC',breaks=40)

hist(tslaU10, main='Usual 10-day Returns',breaks=40)
hist(tsla10ret, main='10-day Returns after RSIC',breaks=10)
hist(tslaNot10ret, main='10-day Returns after NRSIC',breaks=40)

hist(tslaU15, main='Usual 15-day Returns',breaks=40)
hist(tsla15ret, main='15-day Returns after RSIC',breaks=10)
hist(tslaNot15ret, main='15-day Returns after NRSIC',breaks=40)

hist(tslaU20, main='Usual 20-day Returns',breaks=40)
hist(tsla20ret, main='20-day Returns after RSIC',breaks=10)
hist(tslaNot20ret, main='20-day Returns after NRSIC',breaks=40)

#Check the sample sizes and variances.

Stats <- c('Usual 1-day','1-day After RSIC','1-day After NRSIC')
Lengths <- c(length(tslaU1),length(tsla1ret),length(tslaNot1ret))
Variances <- c(var(tslaU1,na.rm=T),var(tsla1ret,na.rm=T), var(tslaNot1ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 5-day','5-day After RSIC','5-day After NRSIC')
Lengths <- c(length(tslaU5),length(tsla5ret),length(tslaNot5ret))
Variances <- c(var(tslaU5,na.rm=T),var(tsla5ret,na.rm=T), var(tslaNot5ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 10-day','10-day After RSIC','10-day After NRSIC')
Lengths <- c(length(tslaU10),length(tsla10ret),length(tslaNot10ret))
Variances <- c(var(tslaU10,na.rm=T),var(tsla10ret,na.rm=T), var(tslaNot10ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 15-day','15-day After RSIC','15-day After NRSIC')
Lengths <- c(length(tslaU15),length(tsla15ret),length(tslaNot15ret))
Variances <- c(var(tslaU15,na.rm=T),var(tsla15ret,na.rm=T), var(tslaNot15ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

Stats <- c('Usual 20-day','20-day After RSIC','20-day After NRSIC')
Lengths <- c(length(tslaU20),length(tsla20ret),length(tslaNot20ret))
Variances <- c(var(tslaU20,na.rm=T),var(tsla20ret,na.rm=T), var(tslaNot20ret,na.rm=T))
flextable(data.frame(Stats,Lengths, Variances))

# Compare qq plots.

par(mfrow=c(1,3))
qqnorm(tslaU1)
qqline(tslaU1)
qqnorm(tsla1ret)
qqline(tsla1ret)
qqnorm(tslaNot1ret)
qqline(tslaNot1ret)

par(mfrow=c(1,3))
qqnorm(tslaU5)
qqline(tslaU5)
qqnorm(tsla5ret)
qqline(tsla5ret)
qqnorm(tslaNot5ret)
qqline(tslaNot5ret)

par(mfrow=c(1,3))
qqnorm(tslaU10)
qqline(tslaU10)
qqnorm(tsla10ret)
qqline(tsla10ret)
qqnorm(tslaNot10ret)
qqline(tslaNot10ret)

par(mfrow=c(1,3))
qqnorm(tslaU15)
qqline(tslaU15)
qqnorm(tsla15ret)
qqline(tsla15ret)
qqnorm(tslaNot15ret)
qqline(tslaNot15ret)

par(mfrow=c(1,3))
qqnorm(tslaU20)
qqline(tslaU20)
qqnorm(tsla20ret)
qqline(tsla20ret)
qqnorm(tslaNot20ret)
qqline(tslaNot20ret)

# Use shapiro test.
# If p value is less than threshold, reject null hypothesis.
# For shapiro test, the null hypothesis is that the data is normal, and
# the alternative hyptothesis is that the data is not normal.
# If our p value is less than say .05 or .1, we reject that the 
# data is normal.

shapiro.test(tslaU1) 
shapiro.test(tsla1ret)
shapiro.test(tslaNot1ret)

shapiro.test(tslaU5) 
shapiro.test(tsla5ret) #normal
shapiro.test(tslaNot5ret)

shapiro.test(tslaU10) 
shapiro.test(tsla10ret) #normal
shapiro.test(tslaNot10ret)

shapiro.test(tslaU15) 
shapiro.test(tsla15ret) # between .05, .1, sort of normal, grounds for testing
shapiro.test(tslaNot15ret)

shapiro.test(tslaU20) 
shapiro.test(tsla20ret)
shapiro.test(tslaNot20ret)

# Conduct t-tests.

t.test(tslaU1,tsla1ret, Welch=T)
t.test(tslaU1,tslaNot1ret, Welch=T)

t.test(tslaU5,tsla5ret, Welch=T)
t.test(tslaU5,tslaNot5ret, Welch=T)

t.test(tslaU10,tsla10ret, Welch=T)
t.test(tslaU10,tslaNot10ret, Welch=T)

t.test(tslaU15,tsla15ret, Welch=T)
t.test(tslaU15,tslaNot15ret, Welch=T)

t.test(tslaU20,tsla20ret, Welch=T)
t.test(tslaU20,tslaNot20ret, Welch=T)

# Conduct wilcoxon tests.

# Quick note: If we want to show some sort of result, say the 
# stock prices increase after a RSIC below 30, we do need to have
# the extra assumption that the variances are equal. However,
# if we just want to state that the two sets of data are statistically
# different, we don't need equal variances.

wilcox.test(tslaU1, tsla1ret) #not equal to 0 
wilcox.test(tslaU1, tslaNot1ret) #equal to 0

wilcox.test(tslaU5, tsla5ret) #not equal to 0
wilcox.test(tslaU5, tslaNot5ret) #equal to 0

wilcox.test(tslaU10, tsla10ret) #not equal to 0 
wilcox.test(tslaU10, tslaNot10ret) #equal to 0

wilcox.test(tslaU15, tsla15ret) #not equal to 0
wilcox.test(tslaU15, tslaNot15ret) #equal to 0

wilcox.test(tslaU20, tsla20ret) #not equal to 0
wilcox.test(tslaU20, tslaNot20ret) #equal to 0

# Goal 1b: On Balance Volume (OBV)

OBV <- c()
tslaVol <- tsla[,7]

OBV[1] <- 0
for (i in 2:imax)
{
  OBV[i] <- 0
  if (tslaClose[i] > tslaClose[i-1])
  {
    OBV[i] <- OBV[i-1] + tslaVol[i]
  }
  else if (tslaClose[i] < tslaClose[i-1])
  {
    OBV[i] <- OBV[i-1] - tslaVol[i]
  }
  else if (tslaClose[i] == tslaClose[i-1])
  {
    OBV[i] <- OBV[i-1]
  }
}

val <- 100

plot(tslada, OBV, type='l',
     main='On-Balance Volume Tsla (Alltime)',
     xlab='Date',
     ylab='OBV')

plot(tslada[1:val], OBV[1:val], type='l',
     main='On-Balance Volume Tsla (Zoomed In)',
     xlab='Date',
     ylab='OBV')

plot(tslada[(imax-val-1):(imax-1)], OBV[(imax-val-1):(imax-1)], type='l',
     main='On-Balance Volume Tsla (Zoomed In)',
     xlab='Date',
     ylab='OBV')

# TODO: Still need qq plots, histograms, shapiro tests and ttest/wilcoxon

# Goal 2a: Calculate Price-To-Earnings Ratio.

# The P/E is one of the most widely used tools by which investors and 
# analysts determine a stock's relative valuation. This helps determine
# if a stock is overvalued or undervalued. This can also be benchmarked
# against other stocks in the same industry (perhaps a ranking algorithm?).

# P/E ratio is calculated by dividing the market value price
# per share by the company's earnings per share.

# Market value = current stock price (close)
# EPS: (net income - preferred dividends)/(end of Period common shares outstanding)

#Goal 3: Implement some experimental ranking algorithms!























