# https://www.coursera.org/learn/machine-learning/discussions/all/threads/O25D0QykEeWZSyIAC5bWOg

X= c(1, 1, 1)
X <- cbind(X, matrix(c(8, 1, 6, 3, 5, 7, 4, 9, 2), 3, 3, TRUE))
y = c(7, 6, 5)
theta = c(0.1, 0.2, 0.3, 0.4)
computeCost(X, y, theta, 0)
computeCost(X, y, theta, 7)


# --------------------------
# lambda = 0  |   lambda = 7
# --------------------------
# J = 1.3533  |   J = 1.6917
# g =         |   g =
#    -1.4000  |      -1.4000
#    -8.7333  |      -8.2667
#    -4.3333  |      -3.6333
#    -7.9333  |      -7.0000



X = c(1, 2, 3, 4)
y = 5
theta = c(0.1, 0.2, 0.3, 0.4)
computeCost(X, y, theta, 7)

# % results
# J =  3.0150
# g =
#   -2.0000
#   -2.6000
#   -3.9000
#   -5.2000


X = [ones(5,1) reshape(-5:4,5,2)];
y = [-2:2]';
Xval=[X;X]/10;
yval=[y;y]/10;
[et ev] = learningCurve(X,y,Xval,yval,1)
# The solution I get is this:
#
# et =
#
# 0.000000
# 0.031250
# 0.013333
# 0.005165
# 0.002268
#
# ev =
#
# 3.0000e-002
# 5.3125e-003
# 6.0000e-004
# 9.2975e-005
# 2.2676e-005
