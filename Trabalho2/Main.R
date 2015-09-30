#Import the packages necessary to this exercise
require(nnet)
require(ggplot2)
require(ggmap)
require(hexbin)

#Read de data
data <- read.csv(file = "2015s2-mo444-assignment-2.csv")

#Create time features
#time <- strptime(x = data$Dates, format = "%Y-%m-%d %H:%M:%S")
#day <- time$mday
#month <- time$mon
#hour <- time$hour
#data <- cbind(data, day, month, hour)


#Split the date into traning and test data
test <- data[700001:nrow(data),]
data <- data[1:700000,]

#Plot the crimes above the map of the region
location <-c(mean(data$X), mean(data$Y))
map <- ggmap(get_map(location = location , scale = "auto", zoom = 12))
map + geom_point(data = test[test$Category == "NON-CRIMINAL",], aes(x = X, y = Y, colour = Category))

#Create the model for the logistic regression
model <- glm(Category ~ X + Y, data = data, family = "poisson")

#Create a model using Neural Networks
model2 <- multinom(Category ~ X + Y + Descript + DayOfWeek + PdDistrict,data = data)

#Calculate the accuracy
probs <- predict(model2, test, "class")
accur <- test$Category == probs
print("Precisao:")
sum(accur)/nrow(test)
