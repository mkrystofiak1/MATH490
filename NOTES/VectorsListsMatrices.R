#Different length vectors added take the shape of 
#the bigger vector, but scale it by the second vector

p = c(2,4,6,8,20)
q = c(-1,2)

print(p+q)

a = seq(1,5, by = 0.2)
#creates a vector from 1 to 5, but stepping by 0.2
print(a)

b = 1:12
#creates a vector from 1 to 12
print(b)

#INDEXING STARTS AT 1 NOT 0
cat('b[1] = ',b[1], '\n')
cat('b[c(2,4)] =', b[c(2,4)],'\n')
#takes b[2] and b[4]

print(sort(b, decreasing = TRUE))

c = c("Jan", "Feb", "Mar", "Apr",
      "May", "Jun", "Jul", "Aug",
      "Sep", "Oct", "Nov", "Dec")

print(sort(c, decreasing=FALSE))
#Note: Sorting character vectors sorts alphabetically

#In general, vectors have to be the same data
#type. For varying data types, use lists.

d = list("apple", 3, TRUE, c(5, 0, -1))
print(d)
print(class(d))
str(d) #tells the structure of a list

#use str(d) to get info about a list

#For matrices, use matrix()
# matrix(data, nrow, ncol, byrow, dimnames)
# data - input vector
# nrow - number of rows
# ncol - number of columns
# byrow - logical input, TRUE stores by rows
# dimnames - assign names to the rows and columns
#           needs to be a list

A = matrix(b,ncol=1)
print(A)

C = matrix(1:10)
print(C)

#default options:
#   Number of rows = number of elements in input
#   Number of columns = 1 with one vector

rnames = c('day1', 'day2', 'day3')
cnames = c('ltemp', 'htemp')
v = c(42, 39, 51, 75, 72, 79)
P = matrix(v, 3, dimnames=list(rnames,cnames))
print(P)

rownames(P) <- c('month1','month2','month3')
colnames(P) <- c('temp1', 'temp2')
print(P)

D = matrix(seq(1,5, by=.5))
print(D)

#no argument names
E = matrix(v, 3, 2, TRUE, list(rnames,cnames))
print(E)

#accessing elements
print(E[3,1])#row 3, column 1
print(E[1,]) #first row
print(E[,2]) #second column, stored as a vector

#matrix subtraction  and addition can only
#be performed on same size matrices (duh)

matrix1 = matrix(c(1,-2,4,0,5,2),2)
matrix2 = matrix(c(2,2,3,-4,6,1),2)
matrix3 = matrix(c(2,1,0,3,-3,-5),3)

print(matrix1+matrix2)
print(matrix1-matrix2)
#note matrix1 * matrix2 does not work
#need to use matrix  multiplication %*%
print(matrix1%*%matrix2)
#note: print(matrix2*matrix3) this won't work
#because the inner dimensions do not match: needs
#to be (n * m) * (m * x)

exam1 = matrix(c(5,4,10,3,1,9),3,2)
exam2 = matrix(c(7,87,14,1,12,35),2,3)
print(exam1)
print(exam2)
print(exam1%*%exam2)








