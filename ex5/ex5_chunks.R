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

        # Note that lambda is set to 0 here:
        error_train <- computeCost(Xtraini, ytraini, params$par, lambda = 0)
        error_val <- computeCost(Xval, yval, params$par, lambda = 0)
        error <- rbind(error, c(i, error_train$J, error_val$J))
    }
    colnames(error) <- c("tested.param", "error_train", "error_val")
    return(error)
}

#Must take a dataframe with colnames tested.param, error_train & error_val
plotLearningCurve <- function(dat){
    ggplot(data = dat) +
        geom_line(aes(x = tested.param, y = error_train), color = "blue") +
        geom_line(aes(x = tested.param, y = error_val), color = "green")
}

## @knitr polyFeatures
polyFeatures <- function(X, p = 8){
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

## @knitr validationCurve
validationCurve <- function(Xtrain, ytrain, Xval, yval, lambdas){
    # takes a vector of lambda values
    error <- data.frame()
    for(l in lambdas){
        params <- trainLinearReg(Xtrain, ytrain, l)

        # Note that lambda is set to 0 here:
        error_train <- computeCost(Xtrain, ytrain, params$par, lambda = 0)
        error_val <- computeCost(Xval, yval, params$par, lambda = 0)
        error <- rbind(error, c(l, error_train$J, error_val$J))
    }
    colnames(error) <- c("tested.param", "error_train", "error_val")
    return(error)
}



## @knitr random-learningCurve
## just randomize train, then call learningCurve

