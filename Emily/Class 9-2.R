#set working directory by using seeion at the top and selecting set working directory and then choose directory 
setwd("~/UTRGV/Python Class")

boston <- read.csv("BostonHousing.csv")

rooms <- boston$rm
value <- boston$medv

# calculate correlation between number of rooms and median value
cor(rooms, value)
cor.test(rooms, value) # this gives confidence intervals and pvalues 
#length(rooms)
#length(value) 

plot(rooms, value)
plot(value~rooms, xlab ="Number of Rooms", ylab = "House Value (1000s")

#use indexing to find data
rooms[1:10]
head(rooms)
boston$rm[5] <- NA #add an na so we can remove it 
#subset the data so that that the NA is removed
boston_sub <- subset(boston, is.na(boston$rm) == F)

#do regression, do linear model lm(y~x)          
result <- lm(value~rooms)
summary(result)
