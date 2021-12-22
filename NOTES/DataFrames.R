#Dataframe organizes data in tabular form

#Requirements:
# - col names should be nonempty
# - row names should be unique
# - numeric or character data in columns
# - each column should have the same amount of data

da <- c('10/30/2018','10/29/2018','10/28/2018')
op <- c(120.48, 119.34, 125.21)
hi <- c(121.50, 123.95, 125.78)
lo <- c(115.15, 118.30, 123.71)
cl <- c(115.40, 119.64, 124.79)
ivol <- c(2910, 21449, 21450)

ibm.data <- data.frame(da,op,hi,lo,cl,ivol)
print(ibm.data)

str(ibm.data)
cat("\n")

print(summary(ibm.data))
cat("\n")

cls = data.frame(ibm.data$cl) #note the $
print(cls)
print(class(cls))

#we can extract multiple columns
dancl = data.frame(ibm.data$da, ibm.data$cl)
print(dancl)

#can use c() function to extract data
#Note: [,c(1:3)] extracts all rows and columns 1-3
#Note: [,c(2,4)] extracts all rows and columns 2 and 4
t <- data.frame(ibm.data[,c(1:3)])
s <- data.frame(ibm.data[,c(2,4)])

print(t)
cat('\n')
print(s)

#Note: [2:3,] extracts row 2-3 and all columns

rng <- ibm.data[2:3,]
print(rng)

sdata <- ibm.data[c(1,3), c(3,2)]
print(sdata)

#add a column to the data frame

ibm.data$oc <- c('N','Y', 'N')
print(ibm.data)

#add rows to dataframe

da <- c('10/24/2018','10/23/2018')
op <- c(131.1, 129.02)
hi <- c(131.69, 131.9)
lo <- c(127, 128.41)
cl <- c(127.21,131.21)
ivol <- c(5710, 6463)
oc <- c('N','Y')
oibm.data <- data.frame(da,op,hi,lo,cl,ivol,oc)
newibm.data <- rbind(ibm.data, oibm.data)
print(newibm.data) #note the rbind function