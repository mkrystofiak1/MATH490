# Author: Mitchell Krystofiak
# Class: MATH 490 - HW6
# Date: October 8, 2021
# Description: Ranking and winning percentages of 6 teams; point differentials;
#              financial data; empirical rule

sports <- read.csv('./Sports2020.csv')
#View(sports)

#Rename columns because the first column was wacky..

names <- c('Ravens','RavensO','Browns','BrownsO','Texans','TexansO',
           'Chiefs','ChiefsO','Wash','WashO','Bengals','BengalsO')

colnames(sports) <- names
print(sports)

#calculate  the number of teams

nteams<- ncol(sports)/2 #since there are two columns for each team
cat("\nThere are ", nteams, " teams reported.\n\n")

#Find the number of wins for each team (returns a logical vector)

R.wins <- sports[,1] > sports[,2]
Br.wins <- sports[,3] > sports[,4]
T.wins <- sports[,5] > sports[,6]
C.wins <- sports[,7] > sports[,8]
W.wins <- sports[,9] > sports[,10]
Be.wins <- sports[,11] > sports[,12]

#find the winning percentage of each team 
# Winning Percentage = number of wins / number of games

ratings <- c(
           sum(R.wins)/length(R.wins),
           sum(Br.wins)/length(Br.wins),
           sum(T.wins)/length(T.wins),
           sum(C.wins)/length(C.wins),
           sum(W.wins)/length(W.wins),
           sum(Be.wins)/length(Be.wins))

#convert to a matrix to make reading easy

ratings <- as.matrix(ratings)
colnames(ratings) <- 'Ratings'
rownames(ratings) <- c('Ravens','Browns','Texans',
                       'Chiefs','Washington','Bengals')

#sort to prepare for ranking

ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings <- as.matrix(ratings,nrow=nteams)
colnames(ratings) <- 'Ratings'

#add ranking and print final results

ranking <- row(ratings)

results <- cbind(ratings,ranking)
colnames(results) <- c('Win %','Ranking')

cat("Ranking by Win Percentage\n\n")
print(results)
cat("\n\n")
#use point differentials to rank the teams
# point differential = (teams total points) - (total opponents points)

R.pd <- sum(sports[,1])-sum(sports[,2])
Br.pd <- sum(sports[,3])-sum(sports[,4])
T.pd <- sum(sports[,5])-sum(sports[,6])
C.pd <- sum(sports[,7])-sum(sports[,8])
W.pd <- sum(sports[,9])-sum(sports[,10])
Be.pd <- sum(sports[,11])-sum(sports[,12])

#convert to a matrix to make reading easy

pds <- c(R.pd,Br.pd,T.pd,C.pd,W.pd,Be.pd)
pds <- as.matrix(pds, nrow=nteams)
colnames(pds) <- 'PDs'
rownames(pds) <- c('Ravens','Browns','Texans',
                       'Chiefs','Washington','Bengals')

#sort to prepare for ranking

pds <- pds[order(pds[,1], decreasing=T),]
pds <- as.matrix(pds,nrow=nteams)
colnames(pds) <- 'PDs'

#add ranking and print final results

ranking1 <- row(pds)

results1 <- cbind(pds,ranking1)
colnames(results1) <- c('PDs','Ranking')
cat("Ranking by Point Differentials\n\n")
print(results1)
cat("\n\n")


#Tesla Data

tesla <- read.csv('./TSLA.csv')

#prepare the data by sorting and making date friendly

tesla <- tesla[order(tesla$Date,decreasing=T),]
tesla$Date <- as.Date(tesla$Date,format = "%Y-%m-%d")

#prepare to calculate the percent change
#  percent change = (new-initial)/initial * 100

trows = nrow(tesla)                 #number of rows
tcl1 = data.frame(tesla$Close)      #grabs the closing prices
tcl2 = data.frame(tesla[2:trows,5]) #creates a one day offset
tcl2 = rbind(tcl2, 0)               #add a zero column for the returns

#perform calculations

Returns = ((tcl1 - tcl2)/tcl2) * 100
Returns[trows,1] <- NA


tesla$Returns <- Returns
colnames(tesla[,8]) <- 'Returns'

print(tesla[1:5,])

dret = c(Returns[1:trows,])
hist(dret, col='blue',
     main="Histogram of Tesla's Daily % Returns",
     xlab="Daily % Returns",
     breaks=40)

boxplot(dret, col='darkgreen',
        main="Boxplot of Tesla's Daily % Returns",
        ylab="Daily % Returns")

cat("\nSummary of Percentage returns of Tesla Data:\n\n")
print(summary(dret))
cat("\n\n")

#Judging by the shape of the histogram and the box plot, the daily 
#percent returns appears to be normally distributed. Thus, we can use 
#the empirical rule to calculate how much data lies in each interval
std <- sd(dret,na.rm=T)
mean <- mean(dret,na.rm=T)
cat("The standard deviation is ", std , " and the mean is ", mean, ".\n\n")
cat("Theoretically, by the Empirical Rule:\n")
cat("68% of the data lies between ", (mean-std), "and", (mean+std),"\n")
cat("95% of the data lies between ", (mean-(2*std)), "and", (mean+(2*std)),"\n")
cat("99% of the data lies between ", (mean-(3*std)), "and", (mean+(3*std)),"\n\n")

#What % of data points are in each interval?
count1 <- 0
count2 <- 0
count3 <- 0
totalc <- length(dret)
i <- 1

dret <- dret[!is.na(dret)]
while (i < length(dret))
{
        if (dret[i] >= (mean-std) && dret[i] <= (mean+std)) 
        {
                count1 <- count1 + 1
        } 
        else if (dret[i] >= (mean-(2*std)) && dret[i] <= (mean+(2*std)))
        {
                count2 <- count2 + 1
        } 
        else if (dret[i] >= (mean-(3*std)) && dret[i] <= (mean+(3*std)))
        {
                count3 <- count3 + 1
        }
        i <- i + 1
}
p1 <- (count1/totalc)*100
p2 <- (count2/totalc)*100
p3 <- (count3/totalc)*100


cat("The actual distribution of data:\n")
cat(p1,"% of the data lies between", (mean-std),"and", (mean+std),"\n")
cat(p1+p2,"% of the data lies between", (mean-(2*std)),"and",(mean+(2*std)),"\n")
cat(p1+p2+p3,"% of the data lies between,",(mean-(3*std)),"and",(mean+(3*std)),"\n\n")



