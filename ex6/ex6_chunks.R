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

## @knitr test-params
testParams <- function(train, val, Cs, sigs){
    paramPerformance <- data.frame()
    for(C in Cs){
        for(sig in sigs){
            svm <- ksvm(V3 ~ .,
                           data = train,
                           C = C,
                           type = "C-svc",
                           kernel = gkGenerator,
                           kpar = list(sigma = sigTransform(sig)))
            pred <- predict(svm, val[, 1:ncol(val) - 1])
            valError <- sum(pred != val[, ncol(val)]) / length(pred)
            paramPerformance <- rbind(paramPerformance,
                                      c(C, sig, error(svm), valError))
        }
    }
    colnames(paramPerformance) <- c("C", "sigma", "train.error", "val.error")
    return(paramPerformance)
}

## @knitr pre-process
preProcess <- function(email){
    # More help from kaleko on github
    # https://github.com/kaleko/CourseraML/blob/master/ex6/ex6_spam.ipynb
    email <- tolower(email)
    email <- gsub('<[^<>]+>', ' ', email)
    email <- gsub('[0-9]+', 'number', email)
    email <- gsub("(http|https)://[^[:space:]]+", ' httpaddr', email)
    email <- gsub('[^[:space:]]+@[^[:space:]]+', 'emailaddr', email)
    email <- gsub('[$]+', 'dollar', email)
    email <- gsub("'", "", email) #prevent contractions from becoming 2 words
    email <- gsub("[[:punct:]]+", " ", email)
    # email <- gsub("[[:space:]]{2,}", " ", email)
    # wordStem requires a vector of words
    email <- strsplit(email, " ")[[1]]
    email <- wordStem(email)
    # # remove empty elements
    email <- email[email != ""]
    return(email)
}