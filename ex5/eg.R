# ---- test-a ----
simple_function <- function(x){
    return(x + 1)
}

# ---- test-b ----
print(simple_function(5))

# ---- test-c ----
require(R.matlab)
require(ggplot2)
train <- read.csv("../data/ex5train.csv")
val <- read.csv("../data/ex5val.csv")
test <- read.csv("../data/ex5test.csv")

g <- ggplot(data = train, aes(X, y)) +
    geom_point(shape = 4, color = "red", size = 3) +
    labs(title = "Figure 1: Training Data",
         x = "Change in water level (X)",
         y = "Water flowing out of the dam (y)")
print(g)
