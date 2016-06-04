


# dfi <- cbind(ex3data1[, 1:401], ex3data1[, 402] == 1)
# costFunction(dfi, initial_theta)
#
# optim(par = initial_theta,
#       fn = function(x){costFunction(dfi, x)$J},
#       gr = function(x){costFunction(dfi, x)$grad},
#       method = "BFGS", control = list(maxit = 400))

thetas <- data.frame()

for(i in 1:10){
    dfi <- cbind(ex3data1[, 1:401], ex3data1[, 402] == i)
    thetai <- optim(par = initial_theta,
                       fn = function(x){costFunction(dfi, x)$J},
                       gr = function(x){costFunction(dfi, x)$grad},
                       method = "BFGS", control = list(maxit = 400))
    thetas <- rbind(thetas, thetai$par)
}


ex3pred1 <- apply(ex3data1, 1, FUN = function(x){
    which.max(as.vector(apply(thetas, 1, FUN = function(y){
        h(y, x[1:401])
    })))
})

sum(ex3data1[, 402] == ex3pred1) / nrow(ex3data1)
# This is a higher than the Matlab script

ex3weights <- readMat("ex3weights.mat")

z2 <- ex3weights$Theta1 %*% t(ex3data1[, 1:401])
a2 <- sig(z2)
a2 <- rbind(1, a2)

z3 <- ex3weights$Theta2 %*% a2
a3 <- sig(z3)

ex3pred2 <- apply(a3, 2, which.max)
sum(ex3data1[, 402] == ex3pred2) / nrow(ex3data1)
