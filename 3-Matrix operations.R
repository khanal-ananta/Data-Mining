#####################################################################################################################
#--------------------- matrix operation-----------------------------------------------------------------------------
#####################################################################################################################
remove(list = ls())

#####################################################################################################################
#--------------------exercise 1 -------------------------------------------------------------------------------------
# Data are ususally presented in matrix form. Construct a 4*3 matrix with a row for each student and a column for 
# each shop, in which the sum of money is listed, that a student would have to pay in each shop if he buys
# verything on his shopping list for the price stated. Where should the students buy their
# fruits? What does all this have to do with matrix multiplication?

# Student Matrix,S

ShoppingList = matrix(c(6, 4, 5, 5, 3, 7, 7, 8, 3, 5, 3, 6), nrow=4,ncol=3,byrow = TRUE,
                      dimnames = list(c("Fred", "Jenny", "Lizzy", "Joe"), c("apples", "pears", "bananas"))
) 

ShoppingList


Prices = matrix( c(.5, .4, .35, .6, .8, .65, .3, .2, .25), nrow=3, ncol=3, byrow = TRUE,
                 dimnames = list(c("apples", "pears", "bananas"),
                                 c("S.1", "S.2", "S.3"))
)
Prices


Pay = ShoppingList %*% Prices #Matrix Multiplication
Pay

#      S.1 S.2  S.3
#Fred  6.9 6.6 5.95
#Jenny 6.4 5.8 5.45
#Lizzy 9.2 9.8 8.40
#Joe   6.1 5.6 5.20

# Shop 3 is the cheapest one so better to buy those fruits



#--------------------exercise 2 -----------------------------------------------------------------------------------

# Answer all the questions concerning the geometrical interpretations of 2*2 matrices in theenclosed spreadsheet.
# The following 2*2 matrices are treated: identity matrix, dilationmatrix, rotation matrix, reflection matrix,
# symmetric matrix, inverse matrix.

# area of the matrix is determine by determinant

matrix1 <- matrix(c(1,2,3,4), nrow = 2, ncol = 2, byrow = TRUE)
matrix1

# determinant 
DetMatrix <- det(matrix1)
DetMatrix # -2

# inverse
Inv <- solve(matrix1)
Inv

# identity matrix in r
Iden <- diag(2)
Iden  # [,1] [,2]
# [1,]    1    0
# [2,]    0    1


# symmetric matrix
# find transpose of any matrix and multiply to itself to find symmetric metrix
install.packages("Matrix")
library(Matrix)
sysMatrix <- forceSymmetric(matrix1)
sysMatrix

# dilation matrix i.e scaling the matrix to lower form

matrix2 <- matrix(c(.5, 0, 0, .5), ncol = 2, nrow = 2, byrow = TRUE)
matrix2

Dila <- matrix1 %*% matrix2
Dila

# rotation matrix
# this matrix rotate the points in the euclidian space as spacified.

# if a vector (2,3) is to be rotated counterclockwise 90 degree then

rotMatrix <- matrix(c(round(cos(90 *pi/180), digits=2), -sin(90 *pi/180), round(sin(90 *pi/180),digits=2), 
round(cos(90*pi/180),digits = 2)), nrow = 2, ncol = 2, byrow = TRUE)
rotMatrix

#       [,1] [,2]
#[1,]    0   -1
#[2,]    1    0


afterRotaion <- rotMatrix %*% matrix(c(2,3),nrow = 2, byrow = FALSE)
afterRotaion

# reflection matrix
# reflection matrix can be consider as the rotation in 180 degree



# The questions on eigenvalues and eigenvectors for symmetric matrices are not so easy, ifyou have not heard 
# of the concepts before. Don't spend too much time on them in this case. If you've heard of EV's and EV's 
# before, do try to do them.

A <-matrix( c(3, 1, 1, 2), 2,2, byrow = TRUE)
A

evA <- eigen(A)
evA  

#        [,1]       [,2]
# [1,] -0.8506508  0.5257311
# [2,] -0.5257311 -0.8506508

eVal <- eigen(A)$values
eVal # 3.618034 1.381966

# n * n symmetric matrix has n eigen values(not necessarily distinct)
# there exits a set of n eigenvectors one for each eigenvalues that are mutually orthogonal



#--------------------exercise 2 -----------------------------------------------------------------------------------
# The transpose, X T , of a matrix, X, is that matrix, that is obtained, when rows and columnsare interchanged. 
# So if X n*m-matrix, X T is a m*n-matrix (m and n may be greater than 2 – they ay be very large!).

# If X is NxM, then Xtranspose will be MxN, therefore multiplication always possible
# and X.Xt will be a NxN, therefore a square matrix
# Example 

X  <- matrix( c(6, 4, 5, 5, 3, 7,3,6,9,8,6,2), nrow=3, ncol=4,  byrow = TRUE) 
X

# x transpose
Xt <- t(X) 
Xt

x.Xt <- X %*% Xt
x.Xt

#X.Xt is symmetric, would mean its equal to (X.xt)t



