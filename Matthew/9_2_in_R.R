
boston <- read.csv("housing.csv")

rooms <- boston$RM

values <- boston$MEDV

cor(rooms, values)
cor.test(rooms, values)

plot(rooms, values)
plot(values~rooms, xlab = "Number of Rooms", ylab="House Value ($1000")

result <- lm(values~rooms)
summary(result)
