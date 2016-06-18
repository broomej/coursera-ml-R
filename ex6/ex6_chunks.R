## @knitr decision-boundary
decisionBoundary <- function(svm){
    w <- colSums(coef(svm)[[1]] * ex6data1[unlist(alphaindex(svm)), 2:1])
    b <- b(svm)
    C <- param(svm)[[1]]
    return(list(slope = -w[2]/w[1], intcpt = b/w[1], C = C))
}

## @knitr sigma-transformation
sigTransform <- function(sigma){
    1 / (2 * sigma ^ 2)
}

## @knitr gaussian-kernel
gkGenerator <- function (sigma = 1) {
    gk <- function(Xi, Xj = NULL) {
        normSquared <- sum((Xi - Xj) ^ 2)
        # I'm not sure this needs to be coerced to a matrix, but rbfdot returns
        # a 1x1 matrix, so when in Rome...
        return(as.matrix(exp(-normSquared / (2 * sigma ^ 2))))
    }
    return(new("rbfkernel", .Data = gk, kpar = list(sigma = sigma)))
}
