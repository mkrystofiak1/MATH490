# Author: Mitchell Krystofiak
# Class: MATH 490 - HW8
# Date: October 22, 2021
# Description: Use Colley's method to rate sports teams.

# 1 -> 4 Teams, No repeats

c1 <- read.csv('./Colley1.csv')
c1 <- as.matrix(c1)
colnames(c1) <- c('R', 'Br', 'D', 'S', 'b')

print(c1)

colley <- c1[,1:4]
b <- c1[,5]
b <- as.matrix(b)

print(colley)
print(b)

ratings <- solve(colley,b)
ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings = as.matrix(ratings, nrow=4) 
ranking <- row(ratings)
results <- cbind(ratings, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')

# 2 -> 4 Teams, Repeats

c2 <- read.csv('./Colley2.csv')
c2 <- as.matrix(c2)
colnames(c2) <- c('R', 'Be', 'S', 'Br', 'b')

print(c2)

colley <- c2[,1:4]
b <- c2[,5]
b <- as.matrix(b)

print(colley)
print(b)

ratings <- solve(colley,b)
ratings <- ratings[order(ratings[,1], decreasing=T),]
ratings = as.matrix(ratings, nrow=4) 
ranking <- row(ratings)
results <- cbind(ratings, ranking)
colnames(results) <- c('Ratings','Rankings')

print(results)
cat('\n\n')
