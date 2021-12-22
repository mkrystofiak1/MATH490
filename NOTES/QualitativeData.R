#Qualitative/Categorical data:
#
# Religious affil
# Political affil
# Hair color
# Car ownership
# Movie genre

#Quantitative data/Numerical data:
#
# Height
# Weight
# Number of computers per household
# Number of calories burned

sports.data <- read.csv('./SportsData.csv')
tesla.data <- read.csv('./TSLA.csv')

#As a starting point or analyzing quantitaive
#data, we look at graphs.
#
#R is known for its easy graphing tools
# For example, it's easy to generate a 
# histogram in R using the hist() function
# trouble is, the data fed into the hist()
# must be a vector, but we're working with
# a dataframe

# easy to overcome

ppg <- c(sports.data$PTS)

hist(ppg, col="red",
     border='black',
     main="Histogram of Duke's Avg PPG per season since 1947",
     xlab="Average PPG per season",
     xlim=(c(0,100)),
     breaks=10)

boxplot(ppg)
summary(ppg)
#Remember the points (outcomes) are
# listed along the horizontal axis
# and the counts are on the vertical axis

# lots of ways to  customize a histrogram
# great package -- ggplot package

