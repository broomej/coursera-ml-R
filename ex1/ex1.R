require(ggplot2)
require(dplyr)
q1data <- read.table("ex1data1.txt", 
                     sep = ",", 
                     col.names = c("population", "profit"))
q1data$ones <- 1
q1data <- q1data %>% select(ones, population, profit)
x <- q1data$population
y <- q1data$profit
m <- length(y)

g1 <- ggplot(q1data, aes(x = population, y = profit)) +
    geom_point(shape = 4, color = "red", size = 3) +
    xlab("Population of City in 10,000s") +
    ylab("Profit in $10,000s")

g1

theta <- matrix(nrow = 2, ncol = 1)
theta[,1] <- 0

## gradient descent settings
iterations <- 1500
alpha <- 0.01

# computeCost(X, y, theta)

# run gradient descent
# theta = gradientDescent(X, y, theta, alpha, iterations)

computeCost <- function(pred, y){
    sqError <- (pred - y) ^ 2
    cost <- (1 / (2 * m)) * sum(sqError)
    return(cost)
}

gradStep <- function(thetaj, alpha, pred, y, Xj){ # make sure you're using the right theta and X
    return(
        thetaj - alpha * (1 / m) * sum((pred - y) * Xj)
    )
}

gradientDescent <- function(X0, X1, y, theta, alpha, iterations){
    J_history <- data.frame()
    for (iteration in 1:iterations){
        thetaZero <- theta[1, 1]
        thetaOne <- theta[2, 1]
        pred <- thetaZero + thetaOne * X1
        
        J <- computeCost(pred = pred, y = y)
        J_history <- rbind(J_history, c(J, iteration - 1, thetaZero, thetaOne))
        
        jZeroTemp <- gradStep(thetaZero, alpha, pred, y, X0)
        theta[2, 1] <- gradStep(thetaOne, alpha, pred, y, X1)
        theta[1, 1] <- jZeroTemp
    }
    # for final iteration
    J <- computeCost(pred = thetaZero + thetaOne * X1, y = y)
    J_history <- rbind(J_history, c(J, iteration, thetaZero, thetaOne))
    colnames(J_history) <- c("loss", "iterations", "thetaZero", "thetaOne")
    return(J_history)
}

jhist <- gradientDescent(q1data$ones, q1data$population, q1data$profit, theta, alpha, iterations)

jhist <- jhist[2:(nrow(jhist) - 1),]
ggplot(jhist, aes(x = iterations, y = loss)) + geom_line()

g1 + geom_abline(slope = tail(jhist$thetaOne, n = 1), intercept = tail(jhist$thetaZero, n = 1))
