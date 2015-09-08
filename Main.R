GD <- function(){
  YearPredictionMSD <- read.csv("~/ML-Trabalho1/YearPredictionMSD.txt", header=FALSE, colClasses = "numeric", comment.char = "")
  YearPredictionMSD <- as.matrix(YearPredictionMSD)
  alpha <- 0.015
  y1 <- as.matrix(YearPredictionMSD[1:463715,1])
  m <- nrow(y1)
  y2 <- as.matrix(YearPredictionMSD[463716:nrow(YearPredictionMSD),1])
  x <- YearPredictionMSD[1:463715,]
  t <- YearPredictionMSD[463716:nrow(YearPredictionMSD),]
  x[,1] <- 1
  t[,1] <- 1
  nov <- 2:91
  
  # feature normalization
  x[,nov] <- (x[,nov] - mean(x[,nov]))/sd(x[,nov])
  t[,nov] <- (t[,nov] - mean(t[,nov]))/sd(t[,nov])
  
  grad.descent <- function(theta){
    error <- Inf
    cont <- TRUE
    i<-1
    while (cont) {
    #for(i in 1:5000){
      print(error)
      g <- as.numeric(alpha) * grad(theta)
      m <- mean(g)
      theta <- theta - g 
      if (abs(m) > error || i > 1000) {
        cont <- FALSE
      } else {
        error <- abs(m)
      }
      i <- i + 1
    }
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
  
  YearPredictionMSD <- read.csv("~/ML-Trabalho1/YearPredictionMSD.txt", header=FALSE, nrows = 463715, colClasses = "numeric", comment.char = "")
  YearPredictionMSD <- as.matrix(YearPredictionMSD)
  alpha <- 1
  m <- nrow(YearPredictionMSD)
  y <- as.matrix(YearPredictionMSD[,1])
  x <- YearPredictionMSD
  x <- (x - mean(x))/sd(x)
  x[,1] <- 1
  
  theta <- solve(t(x)%*%x) %*% t(x) %*% y
  
  print(y[500,])
  print(x[500,]%*%theta)
  
}