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
    convergence <- list(list(NA, NA))
    for(i in 1:iter){
        idx <- findClosestCentroids(X, K)
        K <- computeCentroids(X, idx, K)
        convergence[[i]] <- list(idx = idx, centroids = K)
    }
    return(convergence)
}

