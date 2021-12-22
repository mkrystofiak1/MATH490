TeslaData = read.csv('./TSLA.csv')
vnames <- c('fda', 'fop', 'fhi', 'flo', 'fcl', 'fadjcl', 'fvol')
colnames(TeslaData) <- vnames
print(TeslaData[1:5,])

TeslaData$fda <- as.Date(TeslaData$fda, format="%Y-%m-%d")

Tsorted <- TeslaData[order(TeslaData$fda,decreasing=T),]
print(Tsorted[1:5,])

cat("Number of rows: ",nrow(Tsorted), "\n")

#number of rows
trows = nrow(Tsorted)
#closing column
clt = data.frame(Tsorted$fcl)
#from 2 to trows, column 5
cly = data.frame(Tsorted[2:trows,5])
#add a zero column to end
cly = rbind(cly, 0)
print(clt[1:5,])
print(cly[1:5,])
print(nrow(clt))
print(nrow(cly))

fret = (clt-cly)/cly*100
fret[trows,1] <- NA
print(fret[1:6,1])

pbeg = trows - 10
print(fret[pbeg:trows,1])

Tsorted$fdret <- fret
print(Tsorted[1:2,])

colnames(Tsorted[,8]) <- 'fdret'
print(Tsorted[1:2,])

#can write to a file...
#write.csv(Tsorted,  "SortedTeslaData.csv",row.names=F)

dret = c(fret[1:trows,])
hist(dret,col='darkgreen',
     main="Histogram of Tesla's Daily % Returns",
     xlab="Daily % Returns",
     breaks=40)

fdates <- c(TeslaData[,1])
fcloses <- c(TeslaData[,5])
plot(fdates, fcloses,
     main='Tesla Stock Prices',
     xlab='Year',
     ylab='Closing Price',
     type='l',
     col='darkgreen')


