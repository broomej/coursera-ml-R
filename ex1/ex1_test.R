# https://www.coursera.org/learn/machine-learning/discussions/all/threads/5wftpZnyEeWKNwpBrKr_Fw

computeCost(matrix(c(1, 2, 1, 3, 1, 4, 1, 5), 4, 2, TRUE),
            c(7, 6, 5, 4),
            c(0.1, 0.2))


# 11.9450

computeCost(matrix(c(1, 2, 3, 1, 3, 4, 1, 4, 5, 1, 5, 6), 4, 3, TRUE),
            c(7,6,5,4),
            c(0.1,0.2,0.3))

# 7.0175


#     gradientDescent:
#
#     Test Case 1:
#
#     >>[theta J_hist] = gradientDescent([1 5; 1 2; 1 4; 1 5],[1 6 4 2]',[0 0]',0.01,1000);
#
# % then type in these variable names, to display the final results
# >>theta
# theta =
#     5.2148
# -0.5733
# >>J_hist(1)
# ans  =  5.9794
# >>J_hist(1000)
# ans = 0.85426
#
#
# SOME EXTRA EXAMPLES THAT I SKIPPED


computeCost(matrix(c(2, 1, 3, 7, 1, 9, 1, 8, 1, 3, 7, 4), 4, 3, TRUE),
             c(2, 5, 5, 6),
            c(0.4, 0.8, 0.8))
## 7.5500
#
# >>gradientDescentMulti([3 5 6; 1 2 3; 9 4 2],[1 6 4]',[0 0 0]',0.01,1000)
# ans =
#     1.2123
# -2.9458
# 2.3219