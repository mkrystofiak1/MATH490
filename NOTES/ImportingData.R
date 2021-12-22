#can set working directory
setwd("./R Code")

#read csv files
#Tesla Stock data
tesla.data <- read.csv('./TSLA.csv')
#View(tesla.data)

#Duke sports data
sports.data <- read.csv('./SportsData.csv')
#View(sports.data)

# Suppose we want to know Duke's best and
# Worst overall winning percentages...
# the intrinsic max() funcion does this

# Recall the dollar sign indicates a column
# and then the name of the variable after
bestwp <- max(sports.data$W.L.)

# The input argument "na.rm=TRUE" in the max and
# min functions will not consider the NA's in the data file...

# clearly, "na.rm=FALSE" is the default

# Recall there is another way to access columns and rows of data

worstwp <- min(sports.data$W.L.)

bestconfwp <- max(sports.data$Conf, na.rm=TRUE)

worstconfwp <- min(sports.data$Conf, na.rm=TRUE)

bestwp <- max(sports.data[,6])

# The subset function is another handy function
# Extracts a row of data relating to the condition
# subset(data, ...)

bseason <- subset(sports.data,W.L. == max(W.L., na.rm=TRUE))

wseason <- subset(sports.data,W.L. == min(W.L., na.rm=TRUE))

LR2 = subset(sports.data, NCAA.Tournament == "Lost Second Round")

#we can do conditions

unexp = subset(sports.data, W.L. > .8 & NCAA.Tournament == 'Lost Second ROund')


#& makes the statement a compound statement

#grepl() will find rows that contain a search word

CoachK = subset(sports.data, grepl("Mike Krz", Coach.es.))
#View(CoachK)

#this creates a dataframe with the coaches name "Mike Krz"
# We can also write these to csv files

write.csv(CoachK, "CoachK.csv", row.names=FALSE)

