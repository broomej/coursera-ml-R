## @knitr reshape-params
reshapeParams <- function(flattened_array){
    theta1 <- matrix(flattened_array[1:((input_layer_size+1)*hidden_layer_size)],
                     nrow = hidden_layer_size,
                     ncol = input_layer_size + 1,
                     byrow = FALSE)

    theta2 <- matrix(flattened_array[((input_layer_size+1)*hidden_layer_size + 1):
                                         length(flattened_array)],
                     nrow = output_layer_size,
                     ncol = hidden_layer_size + 1,
                     byrow = FALSE)

    return(list(theta1 = theta1, theta2 = theta2))
}

## @knitr reshape-x
reshapeX <- function(flattenedX){
    xReshaped <- matrix(flattenedX,
                        nrow = n_training_samples,
                        ncol = (input_layer_size+1),
                        byrow = FALSE)
    return(xReshaped)
}

## @knitr compute-cost
computeCost <- function(mythetas_flattened, myX_flattened, myy, mylambda = 0){
    # Modified to take (m X k) dimensional y matrix

    # First unroll the parameters
    mythetas <- reshapeParams(mythetas_flattened)

    # Now unroll X
    myX <- reshapeX(myX_flattened)

    #This is what will accumulate the total cost
    total_cost <- 0

    m <- n_training_samples

    # irow <- 100
    for(irow in 1:m){
        myrow <- myX[irow, ]
        myhs <- propagateForward(myrow,mythetas)[[2]][,2]
        tmpy <- myy[irow, ]
        mycost <- - crossprod(c(tmpy, 1 - tmpy), c(log(myhs), log(1 - myhs)))
        total_cost <- total_cost + mycost
    }
    total_cost <- total_cost / m

    total_reg <- 0
    for(mytheta in mythetas){
        total_reg <- total_reg + sum(mytheta * mytheta)
    }
    total_reg <- total_reg * mylambda / (2 * m)
    return(total_cost + total_reg)
}

## @knitr propagate-forward
propagateForward <- function(row, Thetas){
    features <- row
    zs_as_per_layer <- list()

    for(i in 1:length(Thetas)){
        # i <- 1
        Theta <- Thetas[[i]]

        #Theta1 is (25,401), features are (401, 1)
        #so "z" comes out to be (25, 1)
        #this is one "z" value for each unit in the hidden layer
        #not counting the bias unit
        z <- Theta %*% features
        a <- sig(z)
        zs_as_per_layer[[i]] <- cbind(z, a)
        if(i == length(Thetas)) {
            return(zs_as_per_layer)
        }
        a <- c(1, a)
        features <- a
        # i <- 2
    }
}

## @knitr sigmoid-gradient
sigmoidGradient <- function(z){
    sig(z) * (1 - sig(z))
}

## @knitr gen-rand-thetas
genRandThetas <- function(epsilon_init = 0.12){
    t1 <- matrix(runif(hidden_layer_size * (input_layer_size+1), -1),
                 hidden_layer_size,
                 input_layer_size + 1)
    t2 <- matrix(runif(output_layer_size * (hidden_layer_size+1)),
                 output_layer_size,
                 hidden_layer_size+1)
    return(list(Theta1 = t1 * epsilon_init, Theta2 = t2 * epsilon_init))
}

## @knitr back-propagate
backPropagate <- function(mythetas_flattened, myX_flattened, myy, mylambda = 0){
    # First unroll the parameters
    mythetas <- reshapeParams(mythetas_flattened)

    # Now unroll X
    myX <- reshapeX(myX_flattened)

    Delta1 = matrix(0, hidden_layer_size,input_layer_size+1)
    Delta2 = matrix(0, output_layer_size,hidden_layer_size+1)

    m = n_training_samples
    for(irow in 1:m){
        myrow <- myX[irow,]
        a1 <- myrow
        # propagateForward returns (zs, activations) for each layer excluding the input layer
        temp = propagateForward(myrow,mythetas)
        z2 = temp[[1]][,1]
        a2 = temp[[1]][,2]
        z3 = temp[[2]][,1]
        a3 = temp[[2]][,2]
        tmpy <- myy[irow, ]
        delta3 = a3 - tmpy

        delta2 <- (t(mythetas[[2]])[-1,] %*% delta3) * sigmoidGradient(z2)
        a2 <- c(1,a2)
        Delta1 <- Delta1 + delta2 %*% t(a1)
        Delta2 <- Delta2 + delta3 %*% t(a2)
    }
    D1 <- Delta1 / m
    D2 <- Delta2 / m

    #Regularization:
    D1[, -1] <- D1[, -1] + (mylambda / m) * mythetas[[1]][,-1]
    D2[, -1] <- D2[, -1] + (mylambda / m) * mythetas[[2]][,-1]

    return(unlist(list(D1, D2)))
}

## @knitr check-gradient
checkGradient <- function(mythetas,myDs,myX,myy,mylambda=0){
    myeps <- 0.0001
    flattened <- unlist(mythetas)
    flattenedDs <- unlist(myDs)
    myX_flattened <- unlist(myX)
    n_elems <- length(flattened)
    # Pick ten random elements, compute numerical gradient, compare to respective D's
    for(i in 1:10){
        x <- as.integer(runif(1) * n_elems)
        epsvec <- rep(0, times = n_elems)
        epsvec[x] <- myeps
        cost_high <- computeCost(flattened + epsvec,myX_flattened,myy,mylambda)
        cost_low <- computeCost(flattened - epsvec,myX_flattened,myy,mylambda)
        mygrad <- (cost_high - cost_low) / (2*myeps)
        return(list(element = x, num.grad = mygrad, backprop.grad = flattenedDs[x]))
    }
}

## @knitr train-nn
trainNN <- function(mylambda=0, it = 50){
    randomThetas_unrolled <- unlist(genRandThetas())
    result <- optim(par = randomThetas_unrolled,
                    fn = function(x){computeCost(x, unlist(ex3data1[, 1:401]), newy)},
                    gr = function(x)(backPropagate(x, unlist(ex3data1[, 1:401]), newy)),
                    method = "BFGS", control = list(maxit = it))
}

## @knitr nn-pred
NNpred <- function(myX,myThetas,myy){ #takes vector of ys
    apply(myX, 1, function(x){
        which.max(propagateForward(x, myThetas)[[2]][,2])
    })
}