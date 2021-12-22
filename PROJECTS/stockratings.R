library(flextable)

# Read in Stock Data
tsla <- read.csv("./TSLA.csv")
fb <- read.csv("./FB.csv")
msft <- read.csv("./MSFT.csv")
sunw <- read.csv("./SUNW.csv")
goog <- read.csv("./GOOGL.csv")

tsla$Date <- as.Date(tsla$Date, format="%Y-%m-%d")
fb$Date <- as.Date(fb$Date, format="%Y-%m-%d")
msft$Date <- as.Date(msft$Date, format="%Y-%m-%d")
sunw$Date <- as.Date(sunw$Date, format="%Y-%m-%d")
goog$Date <- as.Date(goog$Date, format="%Y-%m-%d")

flextable(head(tsla))
flextable(head(fb))
flextable(head(msft))
flextable(head(sunw))
flextable(head(goog))

min_data <- min(nrow(tsla),nrow(fb),nrow(msft),nrow(sunw),nrow(goog))

tsla1 <- tsla[(nrow(tsla) - min_data + 1):nrow(tsla),]
fb1 <- fb
msft1 <- msft[(nrow(msft) - min_data + 1):nrow(msft),]
sunw1 <- sunw[(nrow(sunw) - min_data + 1):nrow(sunw),]
goog1 <- goog[(nrow(goog) - min_data + 1):nrow(goog),]

#Select out returns and dates

tslaClose <- tsla1[,5]
tslaDates <- tsla1[,1]

fbClose <- fb1[,5]
fbDates <- fb1[,1]

msftClose <- msft1[,5]
msftDates <- msft1[,1]

sunwClose <- sunw1[,5]
sunwDates <- sunw1[,1]

googClose <- goog1[,5]
googDates <- goog1[,1]

#Plot the data we are working with

plot(tslaDates, tslaClose, 
     type='l',
     main="TSLA Closing Prices",
     xlab="Date",
     ylab="Closing Price",
     col='black')
plot(fbDates, fbClose, 
     type='l',
     main="FB Closing Prices",
     xlab="Date",
     ylab="Closing Price",
     col='black')
plot(msftDates, msftClose, 
     type='l',
     main="MSFT Closing Prices",
     xlab="Date",
     ylab="Closing Price",
     col='black')
plot(sunwDates, sunwClose, 
     type='l',
     main="SUNW Closing Prices",
     xlab="Date",
     ylab="Closing Price",
     col='black')
plot(googDates, googClose, 
     type='l',
     main="GOOG Closing Prices",
     xlab="Date",
     ylab="Closing Price",
     col='black')

# Now the experimental part: how do we say 'Stock A competed and won/lost
# against Stock B'? 

# What we did to try and make the data equal is only pick out the most
# recent data with our highest minimum amount of data.

# Current thoughts on quantifying a 'competition':
# 1. If tslaClose[i-1] < tslaClose[i+1] && fbClose[i-1] > fbClose[i]: 
#    win for fb, loss for tsla since fb's price went up and tsla's didn't.
# 
#    This would give us 2408 games.

# 2. We can use a win percentage s. t.:
#    Count(price Increase)/Count(# of days)
#    It might make sense for the Win percentage to be the entire dataset
#    so we can really see an 'alltime' w/p.

# 3. We can also use a bunch of other statistics for something like a
#    point differential. For example, our 'pd' could be outstanding shares,
#    volume, P/E ratio, etc.

#    We could compare the highest price/ current price but it seems pretty
#    unfair for the variance in price. We could normalize the price, i.e.,
#    we take each closing price of a particular day into a vector, and then
#    divide each by the norm of the vector to put everything into perspective.

# Calculate W/P of a stock itself

WP <- c(0,0,0,0,0)
wins <- 0

for (i in 2:min_data)
{
  if (tslaClose[i-1] < tslaClose[i])
  {
    wins <- wins + 1
  }
}
WP[1] <- wins/min_data

wins <- 0

for (i in 2:min_data)
{
  if (fbClose[i-1] < fbClose[i])
  {
    wins <- wins + 1
  }
}
WP[2] <- wins/min_data

wins <- 0

for (i in 2:min_data)
{
  if (msftClose[i-1] < msftClose[i])
  {
    wins <- wins + 1
  }
}
WP[3] <- wins/min_data

wins <- 0

for (i in 2:min_data)
{
  if (sunwClose[i-1] < sunwClose[i])
  {
    wins <- wins + 1
  }
}
WP[4] <- wins/min_data

wins <- 0

for (i in 2:min_data)
{
  if (googClose[i-1] < googClose[i])
  {
    wins <- wins + 1
  }
}
WP[5] <- wins/min_data

labels <- c('GOOG','MSFT','FB','TSLA','SUNW')
rankings <- c(1,2,3,4,5)

WP <- WP[order(WP, decreasing=T)]
WP <- as.matrix(WP)
colnames(WP) <- 'Ratings'
rankings <- row(WP)
colnames(rankings) <- 'Ranking'
results <- cbind(WP, rankings)
rownames(results) <-labels
print(results)

# Calculate W/P against other stocks: 
# # of Games = min_data
# Wins = if a stock goes up while the other goes down
# Ties = if two stocks both go up or down

tslaW <- 0
fbW <- 0
msftW <- 0
sunwW <- 0
googW <- 0

#TSLA vs. FB
for (i in 2:min_data)
{
  if ((tslaClose[i-1] < tslaClose[i]) && (fbClose[i-1] > fbClose[i]))
  {
    tslaW <- tslaW + 1
  }
  else if ((tslaClose[i-1] > tslaClose[i]) && (fbClose[i-1] < fbClose[i]))
  {
    fbW <- fbW + 1
  }
}














