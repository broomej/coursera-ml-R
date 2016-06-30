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
        newCentroids[k, ] <- apply(Xk, 2, sum) / nrow(Xk)
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