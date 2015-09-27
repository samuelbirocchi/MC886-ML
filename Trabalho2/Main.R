require(nnet)
require(ggplot2)

data <- read.csv(file = "2015s2-mo444-assignment-02.csv", nrows = 5000)

ggplot(data, aes(x = X, y = Y, colour = Category)) + stat_density2d()

model <- glm(Category ~ X + Y, data = data, family = "binomial")

model <- multinom(Category ~ X + Y,data = data)