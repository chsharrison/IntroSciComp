boston<- read.csv("BostonHousing.csv", header = TRUE)
rooms<- boston$rm
value<- boston$medv
rooms
cor(rooms,value)
cor.test(rooms,value)
plot(rooms,value)
plot(value~rooms, xlab = "Number of Rooms", ylab = "House Value ($1000s)")
boston$rm[5]<- NA
boston_sub <- subset(boston, is.na(boston$rm)==FALSE)
result <- lm(value~rooms)
summary(result)
result2 <-glm(value~rooms)