


#######################
ex7data1 <- read.csv("../data/ex7data/ex7data1.csv")
ex71normd <- scale(ex7data1)
ex71normd <- cbind()
ex71svd <- svd(ex71normd)

covM <- cov(ex71normd)
eigenValues <- eigen(covM)$values
eigenVectors <- eigen(covM)$vectors

plot(ex71normd)
lines(ex71normd[1:nrow(ex71normd)],
      eigenVectors[2,1]/eigenVectors[1,1] * ex71normd[1:nrow(ex71normd)])
lines(ex71normd[1:nrow(ex71normd)],
      eigenVectors[2,2]/eigenVectors[1,2] * ex71normd[1:nrow(ex71normd)])
