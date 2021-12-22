#Author: Mitchell Krystofiak
#Class: Math 490 - HW3
#Date: September 17, 2021
#Description: Basic matrix and vector operations.

#numeric vectors

v1 <- c(1,2,3,4,5,6,7,8,9,10)
v2 <- c(2:11)
v3 <- seq(5,14, by=1)

cat("v1 = ", v1, '\n')
cat("v2 = ", v2, '\n')
cat("v3 = ", v3, '\n')
cat("\n")

#operations between vectors

t1 <- (v1 + v2)^2
t2 <- (v3-v1)*(v2)
a <- t1/t2

cat("a = ", a, '\n')
cat("\n")

#operations single number

t5 <- 5*(2*v2-v1)*(v3+.5*v1)/(v1)^2

cat("t5 = ", t5, '\n')
cat("\n")

#numeric vectors of different sizes

v4 <- c(1, 3, 5, 7, 9, 11)
v5 <- c(2, 4, 6)

cat("v4 = ", v4, '\n')
cat("v5 = ", v5, '\n')
cat("\n")

#operations

t3 <- v4 - v5
t4 <- v5 + v4

cat("t3 = ", t3, '\n')
cat("t4 = ", t4, '\n')
cat("\n")

#character vector

v6 <- c('Hello', 'Goodbye', 'Where', 'September',
        'Cake', 'Raspberry', 'Ostrich', 'Car',
        'Torque', 'Yesterday', 'Understand')

cat("v6 = ", v6, '\n')
cat("\n")

#sorted vectors

v7 <- sort(v6, decreasing=FALSE)
v8 <- sort(v6, decreasing=TRUE)

cat("v7 = ", v7, '\n')
cat("\n")
cat("v8 = ", v8, '\n')
cat("\n")

#sub-vector

v9 <- v6[c(2:8)]

cat("v9 = ", v9, '\n')
cat("\n")

#lists

l1 <- list('car', 2, TRUE, 4L)

#structure of l1

print("l1 = ")
print(l1)
cat("\n")
print("str(l1) = ")
str(l1)
cat("\n")

#elements 2, 5

print("elements 2 and 4 of l1 = ")
print(l1[c(2,4)])
cat("\n")

#matrices

A = matrix(c(71, 73, 71, 62, 59, 88, 88, 87, 80, 78),
           nrow=5,ncol=2)
print(A)

#specify rownames and colnames with functions

row = c('Sept6', 'Sept7', 'Sept8', 'Sept9', 'Sept10')
col = c('LTemp', 'HTemp')
rownames(A) <- row
colnames(A) <- col

print(A)
cat("\n")
#print 1 column and 1 row of matrix A

print(A[1,])
cat("\n")
print(A[,2])
cat("\n")

#two rows of matrix

print(A[1:2,])
cat("\n")

#identity matrix 3x3
B = matrix(c(1,0,0,0,1,0,0,0,1), 3)
#matrix 2x3
C = matrix(c(4, 3, 5, 6, 8, 7), 2)
#matrix 2x3
D = matrix(c(12, 5, 6, 0, 8, 1), 2)

print(B)
cat("\n")
print(C)
cat("\n")
print(D)
cat("\n")

#matix operations

print(2*B)
cat("\n")
print(3*C - D)
cat("\n")
print(C+D)
cat("\n")
print(C%*%B)



           






