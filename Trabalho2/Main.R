require(nnet)
require(ggplot2)
require(ggmap)
require(hexbin)

data <- read.csv(file = "2015s2-mo444-assignment-02.csv", nrows = 5000)

location <-c(min(data$X), min(data$Y), max(data$X), max(data$Y))
map <- ggmap(get_map(location = location , scale = "auto", zoom = 11))

map + stat_density2d(data = data, aes(x = X, y = Y, colour = Category), contour = TRUE)

model <- glm(Category ~ X + Y, data = data, family = "poisson")

model2 <- multinom(Category ~ X + Y,data = data)