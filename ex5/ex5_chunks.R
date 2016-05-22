## @knitr trainLinearReg
trainLinearReg <- function(X, y, lambda = 0){
    if(is.null(dim(X))){
        X <- t(data.frame(X))
    }

    theta <- rep(1, times = ncol(X))
    optim(par = theta,
          fn = function(par){computeCost(X, y, par, lambda)$J},
          gr = function(par){computeCost(X, y, par, lambda)$gradient},
          method = "BFGS", control = list(maxit = 400))
}

## @knitr learningCurve
learningCurve <- function(Xtrain, ytrain, Xval, yval, n = 12, lambda = 0){
    error <- data.frame()
    for(i in 1:n){
        Xtraini <- Xtrain[1:i, ]
        ytraini <- ytrain[1:i]
        params <- trainLinearReg(Xtraini, ytraini, lambda)

        error_val <- computeCost(Xval, yval, params$par, lambda = lambda)
        error <- rbind(error, c(i, params$value, error_val$J))
    }
    colnames(error) <- c("i", "error_train", "error_val")
    return(error)
}

#Must take a dataframe with colnames i, error_train & error_val
plotLearningCurve <- function(dat){
    ggplot(data = dat) +
        geom_line(aes(x = i, y = error_train), color = "blue") +
        geom_line(aes(x = i, y = error_val), color = "green")
}

## @knitr polyFeatures
polyFeatures <- function(X, p = 4){
    polyX <- cbind(1, X)
    for(i in 2:p){
        polyX <- cbind(polyX, X ^ i)
    }
    return(polyX)
}

## @knitr polyPlots
polyPlots <- function(Xtrain, ytrain, Xval, yval, n = 12, lambda = 0, p){
    polyX <- polyFeatures(X = Xtrain, p)
    polyX <- featureNormalize(polyX) #returns NaNs for x0
    polyX[,1] <- 1

    polyXval <- polyFeatures(Xval, p)
    polyXval <- featureNormalize(polyXval)
    polyXval[,1] <- 1

    errors <- learningCurve(polyX, ytrain, polyXval, yval, n, lambda)
    g.lc <- plotLearningCurve(errors)
    g.lm <- "eventually I'll want to show the polynomial fit, but I'll \
    revisit this"

    return(list(g.lc = g.lc, g.lm = g.lm))
}

## @knitr random-learningCurve
## just randomize train, then call learningCurve

