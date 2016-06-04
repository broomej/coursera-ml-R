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
costFunction <- function(M, theta, lambda = 0){
    m <- nrow(M)
    X <- M[, 1:(ncol(M) - 1)]
    y <- M[, ncol(M)]

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



## still need to do optional exercises
