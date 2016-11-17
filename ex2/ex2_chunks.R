## @knitr sig
sig <- function(x){1 / (1 + exp(-x))}

## @knitr h
h <- function(theta, x){
    # matrix multiplication is pairwise multiplication, then summed
    sig(sum(theta * x))
}

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