## @knitr lin-reg-cost-func
computeCost <- function(pred, y, theta, lambda = 0){
    sqError <- (pred - y) ^ 2
    cost <- (1 / (2 * m)) * sum(sqError)
    reg <- (lambda / (2 * m)) * theta[-1]^2
    return(cost + reg)
}

## @knitr gradStep
## still need to update with the regularization term
gradStep <- function(thetaj, alpha, pred, y, Xj){ # make sure you're using the right theta and X
    thetaj - alpha * (1 / m) * sum((pred - y) * Xj)
}

## @knitr gradientDescent
gradientDescent <- function(X, y, theta, alpha, iterations){
    if(length(theta) != ncol(X)){stop("theta and X are nonconformable")}
    J_history <- data.frame()
    for (iteration in 1:iterations){
        pred <- theta %*% t(X)

        J <- computeCost(pred = pred, y = y, theta = theta)
        J_history <- rbind(J_history, c(J, iteration - 1, theta))

        thetaTemp <- vector()
        for(j in 1:length(theta)){
            thetaTemp <- c(thetaTemp, gradStep(theta[j], alpha, pred, y, X[, j]))
        }
        theta <- thetaTemp
    }
    # for final iteration
    J <- computeCost(pred = theta %*% t(X), y = y, theta = theta)
    J_history <- rbind(J_history, c(J, iteration, theta))
    thetanames <- rep("theta", times = length(theta) - 1)
    for(i in length(thetanames)){thetanames[i] <- paste("theta", i, sep = "")}
    colnames(J_history) <- c("loss", "iterations", "theta0", thetanames)
    return(J_history)
}

q1data <- read.table("../data/ex1data1.txt",
                     sep = ",",
                     col.names = c("population", "profit"))
q1data$ones <- 1
q1data <- q1data %>% select(ones, population, profit)
X <- select(q1data, -profit)
y <- q1data$profit
m <- length(y)
theta <- rep(0, times = 2)
iterations <- 1500
alpha <- 0.01

jhist <- gradientDescent(X, y, theta, alpha, iterations)
