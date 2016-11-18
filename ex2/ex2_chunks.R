## @knitr sig
sig <- function(x){1 / (1 + exp(-x))}

## @knitr h
h <- function(theta, x){
    # matrix multiplication is pairwise multiplication, then summed
    sig(sum(theta * x))
}

## @knitr cost-function
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

## @knitr pred-log-reg
predLogReg <- function(M, theta){
    apply(M[, seq_along(theta)], 1, function(x){h(theta, x)})
}

## @knitr find-decision-boundary
findDecisionBoundary <- function(pts = matrix(c(50,75,75,50), 2, 2), theta){
    newYs <- apply(pts, 1, function(x){
        optim(par = x[2],
              fn = function(y){
                    (h(c(1, x[1], y), theta) - 0.5) ^ 2
                  # Squared so min wil be at the decision boundary
                  },
              method = "BFGS", control = list(maxit = 400))
    })
    newYs <- do.call(rbind, newYs)
    newYs <- unlist(newYs[1:2])
    data.frame(x = pts[, 1], oldY = pts[, 2], newY = newYs)
}