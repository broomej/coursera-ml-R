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