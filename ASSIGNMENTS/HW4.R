#Program: HW4.R
#Author: Mitchell Krystofiak
#Date: September 24, 2021
#Description: Working with DataFrames.


#1. DataFrame

# Need to specify row names for the dataframe: Want first 
# column "Dates" to be the rows
#
# Needed to do a little manipulation

homedata <- read.csv('./Data1.csv')

cat("homedata without any editing:\n\n")
print(homedata)

dates = c(homedata[,c(1)])
row.names(homedata) <- dates

homedata = subset(homedata, select = c(2:6))

cat("homedata with rownames changed:\n\n")
print(homedata)

# Creating a 'dummy' variable for the cbind function

test <- c(seq(1,29))
homedata = cbind(homedata, test)

cat("homedata after dummy column 'test' added:\n\n")
print(homedata)

# Creating two 'dummy' variables for the rbind function

Meter <- c(187366, 187400)
Air.Type <- c('Cool','Cool')
THigh <- c(78, 86)
TLow <- c(66, 71)
Usage <- c(60, 34)
test <- c(30, 31)
rowdata <- data.frame(Meter, Air.Type, THigh, TLow, Usage, test)
row.names(rowdata) <- c('9/21/2021','9/22/2021')

homedata = rbind(homedata, rowdata)

cat("homedata after dummy rows '9/21/2021' and '9/22/2021' added:\n\n")
print(homedata)

# Say we just wanted the high and low temps of each day...

temps <- subset(homedata, select = c(3,4))
cat("Subset of homedata, temps, which includes just the high and low temps:\n\n")
print(temps)


#2. Yahoo Finance
#   -Using Telsa Stock data

tesladata <- read.csv('./TSLA.csv')
#cat("Tesla stock data:\n\n")
#print(tesladata)

#Not sure if it should be a col called "ticker' with TSLA in every row,
# or if it should be a col called "TSLA" with empty values...
Ticker <- 'TSLA'
newtesladata <- cbind(Ticker,tesladata)
#View(newtesladata)


#3. Sports Data
# Baltimore Ravens 2020 stats

ravensdata <- read.csv('./RavensData.csv')
View(ravensdata)
cat("Ravens' data without any edits:\n\n")
print(ravensdata)

#Renaming the column names

names <- c('a', 'b', 'c', 'd', 'e',
              'f', 'g', 'h', 'i', 'j',
              'k', 'l', 'm', 'n', 'o',
              'p', 'q', 'r', 's', 't',
              'u', 'v', 'w', 'x', 'y'
              )
colnames(ravensdata) <- names
cat("Raven's data with new column names:\n\n")
print(ravensdata)

#top and lowest scores

topScore <- max(ravensdata$k, na.rm=TRUE)
cat("Raven's top score: ", topScore, "\n\n")

lowScore <- min(ravensdata$k, na.rm=TRUE)
cat("Raven's lowest score: ", lowScore, "\n\n")

#subset using grepl and &

wins = subset(ravensdata, f == 'W' & grepl("Mon",b))
cat("The Raven's wins on Mondays:\n\n")
print(wins)

#two more operations using subset grepl and &

#ravens losses on games at 1:00pm

losses = subset(ravensdata, f == 'L' & grepl("1:00PM ET", d))
cat("The Raven's losses for games at 1:00PM:\n\n")
print(losses)

#games played in November

novgames = subset(ravensdata, grepl("November",c))

cat("the Raven's games played in November:\n\n")
print(novgames)



