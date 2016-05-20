require(R.matlab)
ex4data1 <- readMat("ex4data1.mat")
ex4data1$X <- cbind(1, ex4data1$X)
sig <- function(x){
  1 / (1 + exp(-x))
}

newy <- vector()
for(i in 1:10){
  newy <- cbind(newy, ex4data1$y == i)
}

ex4weights <- readMat("ex4weights.mat")

# from kaleko on Github
# https://github.com/kaleko/CourseraML/blob/master/ex4/ex4.ipynb

input_layer_size <- 400
hidden_layer_size <- 25
output_layer_size <- 10 
n_training_samples <- 5000

# unlist() does what flattenParams does in kaleko's python script

# flattened_array <- unlist(ex4weights)
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

# flattenedX <- unlist(ex4data1$X)
reshapeX <- function(flattenedX){
  xReshaped <- matrix(flattenedX,
                     nrow = n_training_samples,
                     ncol = (input_layer_size+1),
                     byrow = FALSE)
  return(xReshaped)
}

# sum(ex4data1$X!=xReshaped)

# mythetas_flattened <- unlist(ex4weights)
# myX_flattened <- unlist(cbind(ex4data1$X, 1))
# myy <- newy
# mylambda = 0
computeCost <- function(mythetas_flattened, myX_flattened, myy, mylambda = 0){
  # Modified to take (m X k) dimensional y matrix
  
  # First unroll the parameters
  mythetas <- reshapeParams(mythetas_flattened)
  
  # Now unroll X
  myX <- reshapeX(myX_flattened)
  
  #This is what will accumulate the total cost
  total_cost <- 0
  
  m <- n_training_samples
  
  irow <- 100
  irow <- 600
  irow <- 1100
  irow <- 1800
  irow <- 2100
  irow <- 2600
  irow <- 3250
  irow <- 3600
  irow <- 4100
  irow <- 4850 # works well
  irow <- 4849 # works well
  irow <- 4800 # doesn't work, predicts "8"
    
    myrow <- myX[irow, ]
    myhs <- propagateForward(myrow,mythetas)[[-1]][,2] # I think this index works now
    tmpy <- myy[irow, ]
    mycost <- - crossprod(c(tmpy, 1 - tmpy), c(log(myhs), log(1 - myhs))) 
  
  cbind(round(myhs, 3), tmpy)
  mycost
  
  
  
  total_reg <- 0
  for(mytheta in mythetas){
    total_reg <- total_reg + sum(mytheta * mytheta)
  }
  total_reg <- total_reg * mylambda / (2 * m)
  return(total_cost + total_reg)
}

# row <- c(ex4data1$X[1, ], 1)
# Thetas <- ex4weights
propagateForward <- function(row, Thetas){
  features <- myrow
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
# propagateForward(c(ex4data1$X[1, ], 1), Thetas <- ex4weights)

computeCost(unlist(ex4weights), unlist(cbind(ex4data1$X, 1)), newy)
