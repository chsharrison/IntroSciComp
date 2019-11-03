# Set the working directory
## Session -> Set Working Directory -> Choose Directory
### Choose the folder containing the boston housing dataset

## To run lines of code, use "CTRL+ENTER"

# First, let's explore the RStudio environment a little bit

# Load the boston housing dataset
boston <- read.csv("boston_house_prices.csv")


# extract the number of rooms and median value columns from the data
## NOTE: Where in Python, you use the "." to access things within objects, in R you usually use the "$" to do the same thing
rooms <- boston$RM
value = boston$MEDV   # NOTE: the "<-" and "=" can be used interchangeably to define objects


# Calculate the correlation between number of rooms and median value
cor(rooms, value)
cor.test(rooms, value)

# Plot the data
plot(rooms, value)
plot(value~rooms, xlab = "Number of Rooms", ylab = "House Value ($1000s)")


# Linear regression
result <- lm(value~rooms)
summary(result)
result2 <- glm(value~rooms)
summary(result2)


plot(value~rooms, xlab = "Number of Rooms", ylab = "House Value ($1000s)")
abline(result)

plot(value~rooms, xlab = "Number of Rooms", ylab = "House Value ($1000s)")
abline(result, col = "blue")


# Installing a package from CRAN
## Let's install basicTrendline

### Can do this using code or by clicking on install
#### Packages -> Install       # Easiest way when you're using RStudio
install(basicTrendline)  # Maybe this is right. I'm not exactly sure. I made this script while running another function that took a few hours


# NOTE: Unlike python, we don't have to put package name in front of functions from particular packages
trendline(rooms, value, model = "line2P")