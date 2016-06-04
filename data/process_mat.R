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