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
pred <- theta %*% t(X)

source("ex1_chunks.R")

debug(computeCost)
temp <- computeCost(X, y, theta, lambda = 0)

debug(gradientDescent)
jhist <- gradientDescent(X, y, theta, alpha, iterations)
jhist <- jhist[2:(nrow(jhist) - 1),] #makes graph more interpretible

ggplot(jhist, aes(x = iterations, y = loss)) + geom_line()
g1 + geom_abline(slope = tail(jhist$theta1, n = 1), intercept = tail(jhist$theta0, n = 1))