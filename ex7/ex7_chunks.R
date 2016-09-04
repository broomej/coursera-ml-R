## @knitr min-Eu-dist
minEuDist <- function(xi, centroids){
    euDist <- apply(centroids, 1, function(j){dist(rbind(j, xi))})
    return(which.min(euDist))
}

## @knitr find-closest-centroids
findClosestCentroids <- function(X, centroids){
    apply(X, 1, function(i){minEuDist(i, centroids)})
}

## @knitr compute-centroids
computeCentroids <- function(X, idx, K){
    newCentroids <- K
    for(k in 1:nrow(K)){
        Xk <- X[k == idx,]
        newCentroids[k, ] <- apply(Xk, 2, mean)
    }
    return(newCentroids)
}

## @knitr k-means
kMeans <- function(X, K, iter){
    convergence <- matrix(nrow = iter + 1, ncol = nrow(X) + nrow(K) * ncol(K))
    for(i in 1:iter){
        idx <- findClosestCentroids(X, K)
        convergence[i, ] <- c(idx, as.vector(t(K)))
        K <- computeCentroids(X, idx, K)
    }
    idx <- findClosestCentroids(X, K)
    convergence[iter + 1, ] <- c(idx, as.vector(t(K)))
    return(convergence)
}

## @knitr random-init
randomInit <- function(X, n){X[sample(nrow(X), n), ]}

## @knitr sig
sig <- function(X){(t(X) %*% X) / nrow(X)}

## @knitr project-data
projectData <- function(X, U, K){
    X %*% U[, 1:K]
}

## @knitr recover-data
recoverData <- function(Z, U, K){
    Z %*% t(U[, 1:K])
}

## @knitr plot-faces
plotFaces <- function(X){
    # This assumes there will be at least 100 square, grayscale pictures
    k <- sqrt(ncol(X))
    X <- X[1:100,]
    Xreshaped <- matrix(nrow = k * 10, ncol = 0)
    for(i in 1:10){
        Xtall <- matrix(nrow = 0, ncol = k)
        for(j in (10 * i - 9):(10 * i)){
            ## The indexing is a little weird because we want 1:10, then 11:20,
            ## etc.
            face <- matrix(X[j,], k, k, TRUE)[, k:1]
            Xtall <- rbind(Xtall, face)
        }
        Xreshaped <- cbind(Xreshaped, Xtall)
    }
    image(Xreshaped, axes = F, col = grey(seq(0, 1, length = 256)))
}