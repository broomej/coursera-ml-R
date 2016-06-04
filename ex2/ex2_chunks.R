## @knitr sig
sig <- function(x){1 / (1 + exp(-x))}

## @knitr h
h <- function(theta, x){
    # matrix multiplication is pairwise multiplication, then summed
    sig(sum(theta * x))
}

## @knitr cost
# cost <- function(r, theta){ # Xs must be in order with Y at end
#     x <- r[1:(length(r) - 1)]
#     y <- r[length(r)]
#     -y * log(h(theta, x)) - (1 - y) * log(1 - h(theta, x))
# }


## @knitr costFunction
# From http://stackoverflow.com/questions/16700340/optimisation-in-r-using-ucminf-package
costFunction <- function(df, theta, lambda = 0){
    m <- nrow(df)
    X <- df[, 1:(ncol(df) - 1)]
    y <- df[, ncol(df)]

    J <- - (1 / m) * crossprod(c(y, 1 - y),
                               c(log(sig(X %*% theta)), log(1 - sig(X%*% theta)))) +
        (lambda / (2 * m)) * sum(theta ^ 2)

    grad <- (1 / m) * crossprod(X, sig(X %*% theta) - y) +
        (lambda / m) * theta
    list(J = as.vector(J), grad = as.vector(grad))
}

# ## SET LAMBDA TO ZERO FOR j==0
# gradientj <- function(df, theta, j, lambda = 0){ # Ys are in last column
#     mean(apply(df, 1, FUN = function(r){
#         x <- r[1:(length(r) - 1)]
#         xj <- r[j]
#         y <- r[length(r)]
#         (h(theta, x) - y) * xj
#     })) +
#         lambda * theta[j] / nrow(df)
# }
# ## This example yields the right results
# ## [1,]  -0.100
# ## [2,] -12.009
# ## [3,] -11.263
#
# ## for(i in 1:3){print(gradientj(ex2data1, initial_theta, i))}
# costFunction <- function(df, theta, lambda = 0){
#     costs <- apply(df, 1, FUN = function(x){cost(x, theta)})
#     costOverall <- mean(costs)
#     reg <- lambda * sum(theta ^ 2) / (2 * nrow(df))
#     J <- costOverall + reg
#
#     gradient <- gradientj(df, theta, 1, 0) # make sure x0 is first
#
#     for(x in 2:(ncol(df) - 1)){
#         gradient <- c(gradient, gradientj(df, theta, x, lambda))
#     }
#     return(list(J = J, gradient = gradient))
# }

## example
## costFunction(ex2data1, initial_theta)



newTheta <- optim(par = initial_theta,
      fn = function(x){costFunction(ex2data1, x)$J},
      gr = function(x){costFunction(ex2data1, x)$gradient},
      method = "BFGS", control = list(maxit = 400))

ex2data2 <- read.table("ex2data2.txt", sep = ",")
ggplot(ex2data2, aes(V1, V2)) + geom_point(aes(shape = as.factor(V3), color = as.factor(V3)))
y <- ex2data2$V3

x1s <- ex2data2[, 1]
x2s <- ex2data2[, 2]

for(i in 2:6){
    x1s <- cbind(x1s, ex2data2[,1] ^ i)
    }
x1s <- cbind(1, x1s)

for(i in 2:6){
    x2s <- cbind(x2s, ex2data2[,1] ^ i)
}
x2s <- cbind(1, x2s)

allxs <- vector()

## We only want up to 6 degree polynomials, so this gives us too many
for(i in 1:ncol(x2s)){
    allxs <- cbind(allxs, x1s[, 1:(8-i)] * x2s[,i])
}

ex2data2.full <- cbind(allxs, y)

initial_theta <- rep(0, times = 28)

a <-costFunction(ex2data2.full, initial_theta)
## still need to do optional exercises
