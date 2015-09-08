GD <- function(){
  YearPredictionMSD <- read.csv("~/ML-Trabalho1/YearPredictionMSD.txt", header=FALSE, nrows = 463715, colClasses = "numeric", comment.char = "")
  YearPredictionMSD <- as.matrix(YearPredictionMSD)
  alpha <- 0.00005
  y1 <- as.matrix(YearPredictionMSD[1:300000,1])
  m <- nrow(y1)
  y2 <- as.matrix(YearPredictionMSD[300001:463715,1])
  x <- YearPredictionMSD[1:300000,]
  t <- YearPredictionMSD[300001:463715,]
  x[,1] <- 1
  nov <- 2:91
  
  # feature normalization
  x[,nov] <- (x[,nov] - mean(x[,nov]))/sd(x[,nov])
  
  grad.descent <- function(theta){
    error <- Inf
    cont <- TRUE
    while (cont) {
    #for(i in 1:5000){
      print(error)
      g <- as.numeric(alpha) * grad(theta)
      m <- mean(g)
      theta <- theta - g 
      if (abs(m) > error) {
        cont <- FALSE
      } else {
        error <- abs(m)
      }
    }
    return(theta)
  }
  
  grad <- function(theta){
    gradient <- (1/m)* (t(x) %*% ((x %*% theta) - y1))
    return(gradient)
  }
  
  theta <- as.matrix(rep(1, 91))
  theta[1,1] <- 1500
  print(sprintf("Alpha: %f",alpha))
  theta <- grad.descent(theta)
  
  v <- vector()
  
  for (i in 1:nrow(t)) {
    res <- round(t[i,]%*%theta)
    print(c(y2[i,], res))
    v <- c(v, abs(y2[i,] - res))
  }
  print(mean(v))
  
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