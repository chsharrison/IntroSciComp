#session > set working directory > choose directory
setwd("C:/Users/srv_veralab/Downloads")

#load Boston Housing csv > use arrow or equal sign to assign
boston <- read.csv("BostonHousing.csv")

#assigning values to variables
rooms <- boston$rm
value <- boston$medv

#we can see the data if we type it
rooms

#calulate the correlation between rooms and medv
cor(rooms, value)
#or if you want more stats
cor.test(rooms,value)

#plot the data
plot(rooms, value)
#this also works, calling directly from dataset
plot(boston$rm, boston$medv)
#more complicated plot
plot(value~rooms, xlab = "Number of Rooms", ylab = "House Value ($1000)")
#this helps if adding more to one axis
plot(value~rooms+boston$age, xlab = "Number of Rooms & Age", ylab = "House Value ($1000)")

#to replace a value, you index location. ex: >boston$rm[5] <- <new_data>
#if we wanted to remove a NAN ex: >boston_sub <- subset(boston, is.na(boston$RM)==FALSE)

#linear regression
result <- lm(value~rooms)
summary(result)

result1 <- glm(value~rooms)
summary(result1)
