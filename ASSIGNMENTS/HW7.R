#Author: Mitchell Krystofiak
#Class: MATH 490 - HW7
#Date: 10/15/2021
#Description: A program to pratice Massey's Method,
#             and understand the normality of stock return %'s.

#1. Massey's Method to rank sports teams
#   6 teams, 10 games, no repeats

Massey1 <- read.csv('./Massey1-2019.csv')
Massey1 <- as.matrix(Massey1)
colnames(Massey1) <- c('R','D','C','Br','S','Be','PPD')
print(Massey1)
cat('\n')
xm <- Massey1[,1:6]
ym <- Massey1[,7]
ym <- as.matrix(ym)
colnames(ym) <- 'PPD'
print(xm)
print(ym)
xmt = t(xm)
M = xmt%*%xm
p = xmt%*%ym
print(M)
print(p)

cat("The determinate of xmt*xm, the Massey Matrix, is",det(M), "meaning it is\n")
cat("linearly dependent.\n\n")

M[6,] <- c(1,1,1,1,1,1)
p[6,] <- 0
ratings <- solve(M,p)
ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings = as.matrix(ratings, nrow=6) 
ranking <- row(ratings)
results <- cbind(ratings, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')


#2. Massey's Method to rank sports teams
#   4 teams, 10 games, repeats

Massey2 <- read.csv('./Massey2-2019.csv')
Massey2 <- as.matrix(Massey2)
colnames(Massey2) <- c('R','Be','S','Br','PPD')
print(Massey2)
cat('\n')
xm <- Massey2[,1:4]
ym <- Massey2[,5]
ym <- as.matrix(ym)
print(xm)
print(ym)
colnames(ym) <- 'PPD'
xmt = t(xm)
M = xmt%*%xm
p = xmt%*%ym
print(M)
print(p)
cat("The determinate of xmt*xm, the Massey Matrix, is", det(M), "meaning it is\n")
cat("linearly dependent.\n\n")

M[4,] <- c(1,1,1,1)
p[4,] <- 0
ratings <- solve(M,p)
ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings = as.matrix(ratings, nrow=6) 
ranking <- row(ratings)
results <- cbind(ratings, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')


#3. Financial Data, Normality

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

#Using rnorm to form a set:

normal <- rnorm(trows, mean, std)
nmean <- mean(normal, na.rm=T)
nstd <- sd(normal, na.rm=T)

hist(normal,
     main="Normal Distribution Example",
     col = 'cyan',
     xlab='Percent Returns')

boxplot(normal,
        main="Normal Distribution Example",
        col ='cyan',
        ylab='Percent Returns')

print(summary(normal))

#Note how the median is almost perfectly in the center for the boxplot and the
#distance between q1 and the median, and the median and q3 are visually equal.

count1 <- 0
count2 <- 0
count3 <- 0
totalc <- length(normal)
i <- 1

normal <- normal[!is.na(normal)]
while (i < length(normal))
{
  if (normal[i] >= (nmean-nstd) && normal[i] <= (nmean+nstd)) 
  {
    count1 <- count1 + 1
  } 
  else if (normal[i] >= (nmean-(2*nstd)) && normal[i] <= (nmean+(2*nstd)))
  {
    count2 <- count2 + 1
  } 
  else if (normal[i] >= (nmean-(3*nstd)) && normal[i] <= (nmean+(3*nstd)))
  {
    count3 <- count3 + 1
  }
  i <- i + 1
}
np1 <- (count1/totalc)*100
np2 <- (count2/totalc)*100
np3 <- (count3/totalc)*100


cat("The actual distribution of data:\n")
cat(np1,"% of the data lies between", (nmean-nstd),"and", (nmean+nstd),"\n")
cat(np1+np2,"% of the data lies between", (nmean-(2*nstd)),"and",(nmean+(2*nstd)),"\n")
cat(np1+np2+np3,"% of the data lies between,",(nmean-(3*nstd)),"and",(nmean+(3*nstd)),"\n\n")

cat("Difference between the real data set and normal data set:\n\n")
cat("For one standard deviation, ",  abs(np1-p1), " is the difference in percentages.\n")
cat("For two standard deviations, ", abs((np1+np2)-(p1+p2)), " is the difference in percentages.\n")
cat("For three standard deviations, ", abs((np1+np2+np3)-(p1+p2+p3)), " is the difference in percentages.\n")


#Testing the thing Sebastian tested
cat('\n')
Massey1 <- read.csv('./Massey1-2019.csv')
Massey1 <- as.matrix(Massey1)
colnames(Massey1) <- c('R','D','C','Br','S','Be','PPD')
xm <- Massey1[,1:6]
ym <- Massey1[,7]
ym <- as.matrix(ym)
colnames(ym) <- 'PPD'
xmt = t(xm)
M = xmt%*%xm
p = xmt%*%ym
M[5,] <- c(1,1,1,1,1,1)
p[5,] <- 0

ratings <- solve(M,p)
ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings = as.matrix(ratings, nrow=6) 
ranking <- row(ratings)
results <- cbind(ratings, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')

Massey1 <- read.csv('./Massey1-2019.csv')
Massey1 <- as.matrix(Massey1)
colnames(Massey1) <- c('R','D','C','Br','S','Be','PPD')
xm <- Massey1[,1:6]
ym <- Massey1[,7]
ym <- as.matrix(ym)
colnames(ym) <- 'PPD'
xmt = t(xm)
M = xmt%*%xm
p = xmt%*%ym
M[4,] <- c(1,1,1,1,1,1)
p[4,] <- 0
ratings <- solve(M,p)
ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings = as.matrix(ratings, nrow=6) 
ranking <- row(ratings)
results <- cbind(ratings, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')

Massey1 <- read.csv('./Massey1-2019.csv')
Massey1 <- as.matrix(Massey1)
colnames(Massey1) <- c('R','D','C','Br','S','Be','PPD')
xm <- Massey1[,1:6]
ym <- Massey1[,7]
ym <- as.matrix(ym)
colnames(ym) <- 'PPD'
xmt = t(xm)
M = xmt%*%xm
p = xmt%*%ym
M[3,] <- c(1,1,1,1,1,1)
p[3,] <- 0
ratings <- solve(M,p)
ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings = as.matrix(ratings, nrow=6) 
ranking <- row(ratings)
results <- cbind(ratings, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')

Massey1 <- read.csv('./Massey1-2019.csv')
Massey1 <- as.matrix(Massey1)
colnames(Massey1) <- c('R','D','C','Br','S','Be','PPD')
xm <- Massey1[,1:6]
ym <- Massey1[,7]
ym <- as.matrix(ym)
colnames(ym) <- 'PPD'
xmt = t(xm)
M = xmt%*%xm
p = xmt%*%ym
M[2,] <- c(1,1,1,1,1,1)
p[2,] <- 0
ratings <- solve(M,p)
ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings = as.matrix(ratings, nrow=6) 
ranking <- row(ratings)
results <- cbind(ratings, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')

Massey1 <- read.csv('./Massey1-2019.csv')
Massey1 <- as.matrix(Massey1)
colnames(Massey1) <- c('R','D','C','Br','S','Be','PPD')
xm <- Massey1[,1:6]
ym <- Massey1[,7]
ym <- as.matrix(ym)
colnames(ym) <- 'PPD'
xmt = t(xm)
M = xmt%*%xm
p = xmt%*%ym
M[1,] <- c(1,1,1,1,1,1)
p[1,] <- 0
ratings <- solve(M,p)
ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings = as.matrix(ratings, nrow=6) 
ranking <- row(ratings)
results <- cbind(ratings, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')

