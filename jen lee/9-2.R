session-working directory-choose directory #adds data in files
boston<- read.csv("BostonHousing.csv")#loads data in environment
rooms<-boston$rm #creates values
values<-boston$medv
rooms
cor.test(rooms,values)#correlations test
cor(rooms,values)
#plotting
plot(rooms,values)
plot(values~rooms,xlab= "Number of rooms", ylab="Housing value (1000s)")
#labeles^
#indexingv
rooms[1:10]
max(rooms)
subset(rooms,rooms>6)
rooms_sub<-subset(rooms,rooms>6) #save
boston$rm[5]<-NA #make Nas
boston_sub<-subset(boston, is.na(boston$rm)==FALSE)#REMOVE NAs
#linear regression
result<-lm(values~rooms)
summary(result)
result2<-glm(values~rooms)
summary(result2)
#look in help and type lm to get info on how to do it 
