#Author: Mitchell Krystofiak
#MATH 490 - Hw 11
#Date: Novemeber 12, 2021
#Description: Technical analysis using hypothesis testing and flextable.

#---Notes---
#
# -Use a hypothesis test to determine whether the 2 figures we are 
# looking at (.1238% vs -.4406%) are statistically different.
#
# -Most logical test to use first:: t-test. Compares means of 2 
# independent samples.
#
# -Independent vs. paired sampling
# -If we choose a return in one group, it is paired if it automatically
#  is associated with a return in the other group.
# 
#
# -T score is a ratio between the difference between two groups and
# the difference within groups. The larger the t score, the more
# difference there is between groups. The smaller, the more similarity
# there is between groups. I.e. a score of 3 means that the groups are
# 3 times different than each other as they are within each other. 
# 
# This data is Independent: Need to use independent sample t test
# 
# -P value is the probability that the results from your sample
# data occurred by chance. Low p-values are good, they indicate
# that the data did not occur by chance.

#Upload Tsla.csv 

library(flextable)
tsla <- read.csv("TSLA.csv")
flextable(head(tsla))

#Fix dates

tsla$Date <- as.Date(tsla$Date, format="%Y-%m-%d")
flextable(head(tsla))

#Select out returns

tslats <- data.frame(tsla[,c(1,5)])
tslada <- tsla[,1]
tslats <- tsla[,5]

#Compute the MAC returns (10 Day and 50 Day)

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

#Plot them together

plot(tslada[2000:imax], tslats[2000:imax], 
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
#10 day and 50 day SMAs and getting 1 day after.

icmax <- (imax-1)
tslaMACret <- c()
tslaMACda <- as.Date(c())
count <- 0

for (i in 51:icmax)
{
  if (tsla10day[i-1] < tsla50day[i-1] && tsla10day[i] > tsla50day[i])
  {
    count <- count + 1
    tslaMACret[count] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
    tslaMACda[count] <- tslada[i]
  }
}

#Plot Moving Average Crossovers.

plot(tslaMACda, tslaMACret,
     main='Moving Average Crossover Between 10 and 50 day SMA',
     xlab='Dates',
     ylab='Moving Average')
abline(0,0)

#Compute usual 1 day returns.

tslaUsual1ret <- c()
for (i in 1:icmax)
{
  tslaUsual1ret[i] <- ((tslats[i+1] - tslats[i])/tslats[i])*100
}

#Compute summary statistics.

usual1ret <- as.vector(summary(tslaUsual1ret))
#usual1ret <- usual10ret[1:6]

MacRet <- as.vector(summary(tslaMACret))
Stats <- c('Min','Q1', 'Q2', 'Mean', 'Q3', 'Max')
retData <- data.frame(Stats, usual1ret, MacRet)
flextable(retData)

#Compare boxplots

par(mfrow = c(1,2))
boxplot(tslaUsual1ret, main='Usual 1-day Returns',ylab='Percent Returns')
boxplot(tslaMACret, main='1-day Returns After a MAC',ylab='Percent Returns')
par(mfrow = c(1,1))

#Compare histograms

par(mfrow = c(1,2))
hist(tslaUsual1ret, main='Usual 1-day Returns', breaks=40,
     xlab='Percent Returns')
hist(tslaMACret, main='1-day Returns After a MAC',breaks=10,
     xlab='Percent Returns')
par(mfrow = c(1,1))

#Check sample sizes an variances

Stats <- c("Sample Size", "Variance")
n <- c(length(tslaUsual1ret), length(tslaMACret))
variance <- c(var(tslaUsual1ret), var(tslaMACret))
samplevar <- data.frame(Stats, n, variance)
flextable(samplevar)

#Check if the data is normally distributed using QQ plots

par(mfrow=c(1,2))
qqnorm(tslaUsual1ret)
qqline(tslaUsual1ret)
qqnorm(tslaMACret)
qqline(tslaMACret)
par(mfrow=c(1,1))

#Use statistical testing to check normality == Shapiro Test

shapiro.test(tslaUsual1ret)
shapiro.test(tslaMACret)

#For the Usual 1 ret data, since the p value is less than
#.1/.05, we reject that the data is normally distributed.

#For 1 day after the MAC, since the p value is greater than
#.1/.05, we accept that the data is normally distributed.\


#Use Welch Two-Sample t-test to address unequal variances.

t.test(tslaUsual1ret, tslaMACret, Welch=TRUE)

#Randomly sample from the Usual Returns to get a vector
#that has the same length as the MAC1ret vector.

Stats <- c("Sample Size", "Variance")
dif <- 1
tol <- 0.001
count <- 0
while (dif > tol)
{
  count <- count + 1
  sampleUsual <- sample(tslaUsual1ret, length(tslaMACret))
  n <- c(length(sampleUsual),length(tslaMACret))
  variance <- c(var(sampleUsual),var(tslaMACret))
  dif <- abs(var(sampleUsual) - var(tslaMACret))
  samplevar <- data.frame(Stats, n, variance)
}
flextable(samplevar)
cat("Total tries:",count,"\n")

#Perform a t-test with the selected data.

t.test(sampleUsual, tslaMACret, Welch=TRUE)

#Check normality of sets with equal variance.

par(mfrow=c(1,2))
qqnorm(sampleUsual)
qqline(sampleUsual)
qqnorm(tslaMACret)
qqline(tslaMACret)
par(mfrow=c(1,1))

shapiro.test(sampleUsual)
shapiro.test(tslaMACret)

#We still see errors in our testing for normality, results
#may not be as accurate as we want.

#Time to use a non-parametric test == Wilcoxon test.

wilcox.test(tslaUsual1ret,tslaMACret)
wilcox.test(sampleUsual,tslaMACret)

#This test compares the medians of the data sets.
#With the whole data, we get a p value < .1, and thus
#we reject that the 10&50 day MAC does not indicate
#an upward momentum over a one day time interval.

#The small sample however does indicate a relationship,
#however this is a very 'cheated' way to look at it.

#In summary, the 10 day and 50 day Moving Average
#Crossovers do not indicate an upward trend in the usual
#1 day after returns. We sort of predicted this already,
#when we saw that our statistical data was not indicating 
#a relationship i.e. different variances/means. The shapiro 
#tests also told us that our data was not normally distributed.












