#Author: Mitchell Krystofiak
#Class: MATH 490 - Hw9
#Date: October 28, 2021
#Description: Markov Method

#Markov Method - 4 Teams, win/loss

markov1 <- read.csv('./Markov1.csv')
markov1 <- as.matrix(markov1)
cat("Markov Method: Wins/losses\n")
colnames(markov1) <- c('R','T','C','P')
print(markov1)

r0 <- matrix(c(.25,.25,.25,.25), nrow=4)
print(r0)

tol <- .0001 
dif <- 1
count <- 0
while ( dif > tol)
{
  ri = markov1 %*% r0
  dif <- max(abs(ri-r0))
  r0 = ri
  count <- count+1
}

cat("\nSteady state after", count,"iterations with tol=", tol,"\n")
rownames(ri) <- c('R','T','C','P')

ri <- ri[order(ri[,1], decreasing=T),]
ri = as.matrix(ri, nrow=4) 
ranking <- row(ri)
results <- cbind(ri, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')

# Markov Method - 4 teams, point differentials

markov2 <- read.csv('./Markov2.csv')
markov2 <- as.matrix(markov2)
cat("Markov Method: Point Differentials\n")
colnames(markov2) <- c('R','T','C','P')
print(markov2)

r0 <- matrix(c(.25,.25,.25,.25), nrow=4)
print(r0)

tol <- .0001 
dif <- 1
count <- 0
while ( dif > tol)
{
  ri = markov2 %*% r0
  dif <- max(abs(ri-r0))
  r0 = ri
  count <- count+1
}
cat("Steady state after", count,"iterations with tol=", tol,"\n")
rownames(ri) <- c('R','T','C','P')

ri <- ri[order(ri[,1], decreasing=T),]
ri = as.matrix(ri, nrow=4) 
ranking <- row(ri)
results <- cbind(ri, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')

#Markov Method - Total Yards

markov3 <- read.csv('./Markov3.csv')
markov3 <- as.matrix(markov3)
cat("Markov Method: Total Yards\n")
colnames(markov3) <- c('R','T','C','P')
print(markov3)

r0 <- matrix(c(.25,.25,.25,.25), nrow=4)
print(r0)

tol <- .0001 
dif <- 1
count <- 0
while ( dif > tol)
{
  ri = markov3 %*% r0
  dif <- max(abs(ri-r0))
  r0 = ri
  count <- count+1
}
cat("\nSteady state after", count,"iterations with tol=", tol,"\n")
rownames(ri) <- c('R','T','C','P')

ri <- ri[order(ri[,1], decreasing=T),]
ri = as.matrix(ri, nrow=4) 
ranking <- row(ri)
results <- cbind(ri, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')

## Based on the 3 different statistics, we get three 
## different rankings.
## W/L : C, R, T, P
## PD  : C, R, P, T
## TTY : T, R, C, P

## There is some consistency:
## 1. Ravens are Ranked 2 for every different stat calculated.
## 2. Patriots are in one of the last 2 positions every time.
##
## We may be able to draw inference about if the number of total 
## yards actually plays a role into the overall win, since the
## Chiefs have stayed at the top (didn't lose a match) every ranking
## but with the Total Yards. I would further want to use correlation
## coefficients to see if we are seeing a correlation or not.

## Another observation: We are only using 6 games as reference,
## meaning, the data is very limited and might not indicate a strong
## connection at first.

allresults <- rbind()