

## don't think i'll need this part





cost <- function(r, theta){ # Xs must be in order with Y at end
    x <- r[1:(length(r) - 1)]
    y <- r[length(r)]
    -y * log(h(theta, x)) - (1 - y) * log(1 - h(theta, x))
}
## Example
## cost(ex2data1[1,], initial_theta)

## SET LAMBDA TO ZERO FOR j==0
gradientj <- function(df, theta, j, lambda = 0){ # Ys are in last column
    mean(apply(df, 1, FUN = function(r){
        x <- r[1:(length(r) - 1)]
        xj <- r[j]
        y <- r[length(r)]
        (h(theta, x) - y) * xj
    })) +
        lambda * theta[j] / nrow(df)
}
## This example yields the right results
## [1,]  -0.100
## [2,] -12.009
## [3,] -11.263

## for(i in 1:3){print(gradientj(ex2data1, initial_theta, i))}
costFunction <- function(df, theta, lambda = 0){
    costs <- apply(df, 1, FUN = function(x){cost(x, theta)})
    costOverall <- mean(costs)
    reg <- lambda * sum(theta ^ 2) / (2 * nrow(df))
    J <- costOverall + reg

    gradient <- gradientj(df, theta, 1, 0) # make sure x0 is first

    for(x in 2:(ncol(df) - 1)){
        gradient <- c(gradient, gradientj(df, theta, x, lambda))
    }
    return(list(J = J, gradient = gradient))
}

## example
## costFunction(ex2data1, initial_theta)


