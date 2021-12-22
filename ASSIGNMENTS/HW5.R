#Author: Mitchell Krystofiak
#Class: COSC 490 - HW5
#Date: September 30, 2021

#1. Sports Data

Ravens <- read.csv('./RavensData.csv')
View(Ravens)

cols <- c('Week','Day', 'Date', 'Time', 'BS', 'WL',
          'OT', 'Rec', 'X', 'Opp', 'Tm', 'Opp1',
          '1stD', 'TotYd', 'PassY', 'RushY', 'TO',
          '1stD1', 'TotYd1', 'PassY1', 'RushY1',
          'TO1', 'Offense','Defense','SpTms')

colnames(Ravens) <- cols
Dates <- c('2020-09-13','2020-09-20','2020-09-28',
           '2020-10-04','2020-10-11','2020-10-18',
           '2020-11-01','2020-11-08','2020-11-15',
           '2020-11-22','2020-12-02','2020-12-08',
           '2020-12-14','2020-12-20','2020-12-27',
           '2021-01-03','2021-01-10','2021-01-16')

Ravens$Date <- Dates
Ravens$Date <- as.Date(Ravens$Date, format="%Y-%m-%d")

# Total Yards Gained on Offense
totalYdsO <- c(Ravens$TotYd)
totalYdsD <- c(Ravens$TotYd1)
hist(totalYdsO,
     main="Total Yards Gained on Offense",
     xlab="Number of Yards",
     col="green",
     breaks=20)

boxplot(totalYdsO,
     main="Total Yards Gained on Offense",
     col="red",
     ylab="Number of Yards")

print("Summary of Raven Offense TotalYds:")
print(summary(totalYdsO))
cat("\n\n")

offP <- c(Ravens$Tm)
defP <- c(Ravens$Opp1)
rdates <- c(Ravens$Date)

plot(rdates,offP,
     main='Offensive Points earned by Ravens and Opponents',
     xlab='Months',
     ylab='Score',
     type='l',
     col='red',
     )
lines(rdates,defP, col='blue')
legend(x="top",
      legend=c('Ravens Points','Opponent Points'),
      col=c('red','blue'),
      lwd=2)

plot(rdates, totalYdsO,
     main="Total Yards Gained on Offense over the Season",
     col='darkgreen',
     xlab='Months',
     ylab='Yards Covered',
     type='b')

plot(rdates, totalYdsD,
     main="Total Yards Gained on Defense over the Season",
     col='darkgreen',
     xlab='Months',
     ylab='Yards Covered',
     type='b')

#Notes on Ravens Data:
# 1. Not very interesting... Only thing recorded is the number of yards gained,
#    scores, dates, times and opponent information. I should have picked a diff
#    erent set that had more diverse data.
# 2. The Offensive vs. Defensive points plot shows everytime the Raven's won and
#    lost, the red over blue means a win.
# 3. An interesting conclusion could be said, it appears that the number of yards
#    covered by the Ravens is greater on days they win than on days they lose.

#2. Finance Data
# 
# Part (a)

Tesla <- read.csv('./TSLA.csv')

#Change column names..

vnames <- c('fda', 'fop', 'fhi', 'flo', 'fcl', 'fadjcl', 'fvol')
colnames(Tesla) <- vnames

#Change date format

Tesla$fda <- as.Date(Tesla$fda, format="%Y-%m-%d")

#Sort Tesla data

Tsorted <- Tesla[order(Tesla$fda,decreasing=T),]
print("Tesla Data Before Alterations:")
print(Tsorted[1:5,])
cat("\n\n")

#number of rows
trows = nrow(Tsorted)
#closing column
clt = data.frame(Tsorted$fcl)
#from 2 to trows, column 5
cly = data.frame(Tsorted[2:trows,5])
#add a zero column to end
cly = rbind(cly, 0)

#Percent difference 
fret = ((clt - cly)/cly) * 100
fret[trows,1] <- NA

#Add column to Tsorted

Tsorted$fdret <- fret
colnames(Tsorted[,8]) <- 'fdret'
print("Tesla Data After Alterations:")
print(Tsorted[1:5,])
cat("\n\n")

dret = c(fret[1:trows,])
hist(dret, col='blue',
     main="Histogram of Tesla's Daily % Returns",
     xlab="Daily % Returns",
     breaks=40)

boxplot(dret, col='darkgreen',
        main="Boxplot of Tesla's Daily % Returns",
        ylab="Daily % Returns")

print("Summary of Percentage returns of Tesla Data:")
print(summary(dret))
cat("\n\n")

# Some Notes on the Data: 
# 1. The max percentage return is 24.4%. I want to know what day this is.
# 2. On average, Tesla is a profitable stock (only .24% average return).
# 3. There are a lot of outliers below 25% of the data and above 75% of the data
#    meaning there are lots of days where the average % return is not the most
#    realistic return to expect (Lots of good days, lots of bad days).

# Part (b)

fdates <- c(Tesla[,1])
fcloses <- c(Tesla[,5])
plot(fdates, fcloses,
     main='Tesla Stock Prices',
     xlab='Year',
     ylab='Closing Price',
     type='l',
     col='red')

#Some Notes on Plot:
# 1. The more flat the line from year to year, the less the annual percent 
#    return. the steeper the line, the more the annual percent return.
# 2. The time to buy would have been somewhere in 2019, since it jumped from 
#    below $200 to over $800 a share.
