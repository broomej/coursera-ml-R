## Source this script from parent directory

require(R.matlab)
## ex3
ex3data1 <- readMat("data/ex3data1.mat")
write.csv(x = cbind(ex3data1$X, ex3data1$y),
          file = "data/ex3data1.csv",
          row.names = FALSE)

ex3weights <- readMat("data/ex3weights.mat")
write.csv(x = ex3weights$Theta1,
          file = "data/ex3weights_Theta1.csv",
          row.names = FALSE)
write.csv(x = ex3weights$Theta2,
          file = "data/ex3weights_Theta2.csv",
          row.names = FALSE)
## ex5
ex5data1 <- readMat("data/ex5data1.mat")
train <- data.frame(ex5data1$X, ex5data1$y)
colnames(train) <- c("X", "y")
val <- data.frame(ex5data1$Xval, ex5data1$yval)
colnames(val) <- c("X", "y")
test <- data.frame(ex5data1$Xtest, ex5data1$ytest)
colnames(test) <- c("X", "y")

write.csv(x = train, file = "data/ex5train.csv", row.names = F)
write.csv(x = val, file = "data/ex5val.csv", row.names = F)
write.csv(x = test, file = "data/ex5test.csv", row.names = F)

## ex6
ex6data1 <- readMat("data/ex6data/ex6data1.mat")
write.csv(x = cbind(ex6data1$X, ex6data1$y),
          file = "data/ex6data/ex6data1.csv",
          row.names = FALSE)

ex6data2 <- readMat("data/ex6data/ex6data2.mat")
write.csv(x = cbind(ex6data2$X, ex6data2$y),
          file = "data/ex6data/ex6data2.csv",
          row.names = FALSE)

ex6data3 <- readMat("data/ex6data/ex6data3.mat")
write.csv(x = cbind(ex6data3$X, ex6data3$y),
          file = "data/ex6data/ex6data3_train.csv",
          row.names = FALSE)
write.csv(x = cbind(ex6data3$Xval, ex6data3$yval),
          file = "data/ex6data/ex6data3_val.csv",
          row.names = FALSE)

spamTrain <- readMat("data/ex6data/spamTrain.mat")
write.csv(x = cbind(spamTrain$X, spamTrain$y),
          file = "data/ex6data/spamTrain.csv",
          row.names = FALSE)

spamTest <- readMat("data/ex6data/spamTest.mat")
write.csv(x = cbind(spamTest$X, spamTest$y),
          file = "data/ex6data/spamTest.csv",
          row.names = FALSE)

# ex7
ex7data1 <- readMat("data/ex7data/ex7data1.mat")
write.csv(x = ex7data1$X, file = "data/ex7data/ex7data1.csv", row.names = FALSE)

ex7data2 <- readMat("data/ex7data/ex7data2.mat")
write.csv(x = ex7data2$X, file = "data/ex7data/ex7data2.csv", row.names = FALSE)

ex7faces <- readMat("data/ex7data/ex7faces.mat")
write.csv(x = ex7faces$X, file = "data/ex7data/ex7faces.csv", row.names = FALSE)

bird_small <-readMat("data/ex7data/bird_small.mat")$A
## write.csv collapses the multidimensional array into a 2D array, so first I
## modify the indices so that it collapses correctly
##
bird_small <- aperm(bird_small, c(3, 1, 2))
write.csv(x = bird_small, file = "data/ex7data/bird_small.csv", row.names = F)
