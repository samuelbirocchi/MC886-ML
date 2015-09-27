GD <- function(){
  YearPredictionMSD <- read.csv("~/ML-Trabalho1/YearPredictionMSD.txt", header=FALSE, colClasses = "numeric", comment.char = "")
  YearPredictionMSD <- as.matrix(YearPredictionMSD)
  alpha <- 0.015
  y1 <- as.matrix(YearPredictionMSD[1:463715,1])
  m <- nrow(y1)
  y2 <- as.matrix(YearPredictionMSD[463716:nrow(YearPredictionMSD),1])
  x <- YearPredictionMSD[1:463715,]
  t <- YearPredictionMSD[463716:nrow(YearPredictionMSD),]
  
  # feature normalization
  x <- scale(x)
  t <- scale(t)
  
  x[,1] <- 1
  t[,1] <- 1
  
  cost <- function(theta){
    return(  (.5/m)* ((x %*% theta) - y1)  )
  }
  
  grad.descent <- function(theta){
    error <- Inf
    cont <- TRUE
    i<-1
    vectorCost <- vector()
    while (cont) {
    #for(i in 1:5000){
      print(error)
      g <- as.numeric(alpha) * grad(theta)
      c <- mean(cost(theta))
      vectorCost <- c(vectorCost, c)
      theta <- theta - g 
      if (abs(c) < 10^-9 || i > 200) {
        cont <- FALSE
      } else {
        error <- abs(c)
      }
      i <- i + 1
    }
    
    plot(1:length(vectorCost), vectorCost, type="n", xlab = "Iterations", ylab = "J(theta)") 
    lines(1:length(vectorCost), vectorCost) 
    
    return(theta)
  }
  
  grad <- function(theta){
    gradient <- (1/m)* (t(x) %*% ((x %*% theta) - y1))
    return(gradient)
  }
  
  theta <- as.matrix(rep(1, 91))
  theta[1,1] <- 2000
  print(sprintf("Alpha: %f",alpha))
  theta <- grad.descent(theta)
  
  v <- vector()
  
  for (i in 1:nrow(t)) {
    res <- round(t[i,]%*%theta)
    v <- c(v, abs(y2[i,] - res))
  }
  
  print(sprintf("Media: %f",mean(v)))
  print(sprintf("Min: %f", min(v)))
  print(sprintf("Max: %f",max(v)))
  
}

NE <- function(){
  
  YearPredictionMSD <- read.csv("~/ML-Trabalho1/YearPredictionMSD.txt", header=FALSE, colClasses = "numeric", comment.char = "")
  YearPredictionMSD <- as.matrix(YearPredictionMSD)
  y1 <- as.matrix(YearPredictionMSD[1:463715,1])
  y2 <- as.matrix(YearPredictionMSD[463716:nrow(YearPredictionMSD),1])
  x <- YearPredictionMSD[1:463715,]
  t <- YearPredictionMSD[463716:nrow(YearPredictionMSD),]
  
  # feature normalization
  x <- scale(x)
  t <- scale(t)
  
  x[,1] <- 1
  t[,1] <- 1
  
  theta <- solve(t(x)%*%x) %*% t(x) %*% y1
  
  v <- vector()
  
  for (i in 1:nrow(t)) {
    res <- round(t[i,]%*%theta)
    v <- c(v, abs(y2[i,] - res))
  }
  
  print(sprintf("Media: %f",mean(v)))
  print(sprintf("Min: %f", min(v)))
  print(sprintf("Max: %f",max(v)))
  
  
}