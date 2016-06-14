## @knitr gaussian-kernel
gk <- function(Xi, Xj, sigma){
    normSquared <- sum((Xi - Xj) ^ 2) # seems to be working
    exp(-normSquared / (2 * sigma ^ 2)) # not working, but the formula looks correct
}

# norm_vec <- function(x) sqrt(sum(x^2))