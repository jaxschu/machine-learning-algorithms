

## Source your Rcpp file (put in the name of your
## Rcpp file)
library(Rcpp)
sourceCpp(Sweep.cpp)

##################################
## Function 1: QR decomposition ##
##################################

myQR <- function(A){

  ## Perform QR decomposition on the matrix A
  ## Input:
  ## A, an n x m matrix

  ########################
  ## FILL IN CODE BELOW ##
  ########################
  n <- nrow(A)
  m <- ncol(A)
  R = A
  Q = diag(1,n)

  for (k in 1:(m-1)) {
    X <- matrix(0,n,1)  ### zero matrix nx1
    for (i in k:n) {
      X[i,1] <- R[i,k]
    }
    V <- X
    V[k] <- X[k] + sign(X[k]) * norm(X, type = 'f')
    S <- norm(V, type = 'f')
    u <- V/S
    R <- R - 2 * (u %*% t(u) %*% R)
    Q <- Q - 2 * (u %*% t(u) %*% Q)

  }


  ## Function should output a list with Q.transpose and R
  ## Q is an orthogonal n x n matrix
  ## R is an upper triangular n x m matrix
  ## Q and R satisfy the equation: A = Q %*% R
  return(list("Q" = t(Q), "R" = R))

}


#################################
## Function 2: Sweep operation ##
#################################

mySweep <- function(A, m){

  # Perform a SWEEP operation on A with the pivot element A[m,m].
  #
  # A: a square matrix.
  # m: the pivot element is A[m, m].
  # Returns a swept matrix B (which is m by m).

  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  n <- dim(A)[1]
  for (k in 1:m) {
    for(i in 1:n)
      for(j in 1:n)
        if (i !=k & j !=k)
          A[i,j] <- A[i,j]-A[i,k]*A[k,j]/A[k,k]
    for(i in 1:n)
      if (i != k)
          A[i,k] <-A[i,k]/A[k,k]
    for (j in 1:n)
      if (j !=k)
          A[k,j] = A[k,j]/A[k,k]

    A[k,k] <- -1/A[k,k]
  }


  ## The function outputs the matrix B
  return(A)

}


##################################
## Function 3: Ridge regression ##
##################################

myRidge <- function(X, Y, lambda){

  # Perform ridge regression of Y on X.
  #
  # X: an n x p matrix of explanatory variables.
  # Y: an n vector of dependent variables. Y can also be a
  # matrix, as long as the function works.
  # lambda: regularization parameter (lambda >= 0)
  # Returns beta, the ridge regression solution.

  ##################################
  ## FILL IN THIS SECTION OF CODE ##
  ##################################
  n = dim(X)[1]
  p = dim(X)[2]
  Z = cbind(rep(1,n),X,Y)
  A = t(Z) %*% Z
  D = diag(rep(lambda, p+2))
  D[1,1] = 0
  D[p+2,p+2] = 0
  A = A + D
  S = mySweep(A,p+1)
  beta_ridge = S[1:(p+1),p+2]


  #QR
  #R <- myQR(A)$R
  #R1 <- R[c(1:(p+1)), c(1:(p+1))]
  #Y1 <- R[c(1:(p+1)), p+2]
  #beta_ridge <- solve(R1,Y1)

  ## Function should output the vector beta_ridge, the
  ## solution to the ridge regression problem. beta_ridge
  ## should have p + 1 elements.
  return(beta_ridge)

}


####################################################
## Function 4: Piecewise linear spline regression ##
####################################################


mySpline <- function(x, Y, lambda, p = 100){

  # Perform spline regression of Y on X.
  #
  # x: An n x 1 vector or matrix of explanatory variables.
  # Y: An n x 1 vector of dependent variables. Y can also be a
  # matrix, as long as the function works.
  # lambda: regularization parameter (lambda >= 0)
  # p: Number of cuts to make to the x-axis.

  ##################################
  ## FILL IN THIS SECTION OF CODE ##
  ##################################
  #n = 20
  #p = 100
  sigma = .1
  #lambda = 1.

  #x = runif(n)
  #x = sort(x)
  #Y = x^2 + rnorm(n)*sigma
  X = matrix(x, nrow=n)
  for (k in (1:(p-1))/p)
    X = cbind(X, (x>k)*(x-k))
  beta_spline = myRidge(X,Y,lambda)
  Yhat = cbind(rep(1, n), X)%*%beta_spline



  #plot the regression with different lambda

  #plot(x, Y, ylim = c(-.2, 1.2), col = "red")
  #par(new = TRUE)

  #lambda_val =c(10,1,0.1,0.01)
  #color = c("blue", "purple", "green", "orange")
  #for (i in 1:4) {
    #par(new = TRUE)
    #beta_spline = myRidge(X,Y,lambda_val[i])
    #Yhat = cbind(rep(1, n), X)%*%beta_spline
    #plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = color[i])
  #}

  #plot(x, Y, ylim = c(-.2, 1.2), col = "red")
  #par(new = TRUE)
  #plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = "green")



  #

  ## Function should a list containing two elements:
  ## The first element of the list is the spline regression
  ## beta vector, which should be p + 1 dimensional (here,
  ## p is the number of cuts we made to the x-axis).
  ## The second element is y.hat, the predicted Y values
  ## using the spline regression beta vector. This
  ## can be a numeric vector or matrix.
  output <- list(beta_spline = beta_spline, predicted_y = Yhat)
  return(output)

}


# #benchmark for R vs CPP
# ###############################################################################
# myRidgeC <- function(X, Y, lambda){
#
#   # Perform ridge regression of Y on X.
#   #
#   # X: an n x p matrix of explanatory variables.
#   # Y: an n vector of dependent variables. Y can also be a
#   # matrix, as long as the function works.
#   # lambda: regularization parameter (lambda >= 0)
#   # Returns beta, the ridge regression solution.
#
#   ##################################
#   ## FILL IN THIS SECTION OF CODE ##
#   ##################################
#   n = dim(X)[1]
#   p = dim(X)[2]
#   Z = cbind(rep(1,n),X,Y)
#   A = t(Z) %*% Z
#   D = diag(rep(lambda, p+2))
#   D[1,1] = 0
#   D[p+2,p+2] = 0
#   A = A + D
#   S = mySweepC(A,p+1)
#   beta_ridge = S[1:(p+1),p+2]
#
#   ## Function should output the vector beta_ridge, the
#   ## solution to the ridge regression problem. beta_ridge
#   ## should have p + 1 elements.
#   return(beta_ridge)
#
# }
#
# mySplineC <- function(x, Y, lambda, p = 100){
#
#   # Perform spline regression of Y on X.
#   #
#   # x: An n x 1 vector or matrix of explanatory variables.
#   # Y: An n x 1 vector of dependent variables. Y can also be a
#   # matrix, as long as the function works.
#   # lambda: regularization parameter (lambda >= 0)
#   # p: Number of cuts to make to the x-axis.
#
#   ##################################
#   ## FILL IN THIS SECTION OF CODE ##
#   ##################################
#   #n = 20
#   #p = 100
#   sigma = .1
#   #lambda = 1.
#
#   #x = runif(n)
#   #x = sort(x)
#   #Y = x^2 + rnorm(n)*sigma
#   X = matrix(x, nrow=n)
#   for (k in (1:(p-1))/p)
#     X = cbind(X, (x>k)*(x-k))
#   beta_spline = myRidgeC(X,Y,lambda)
#   Yhat = cbind(rep(1, n), X)%*%beta_spline
#
#
#
#   #plot the regression with different lambda
#
#   #plot(x, Y, ylim = c(-.2, 1.2), col = "red")
#   #par(new = TRUE)
#
#   #lambda_val =c(10,1,0.1,0.01)
#   #color = c("blue", "purple", "green", "orange")
#   #for (i in 1:4) {
#   #par(new = TRUE)
#   #beta_spline = myRidge(X,Y,lambda_val[i])
#   #Yhat = cbind(rep(1, n), X)%*%beta_spline
#   #plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = color[i])
#   #}
#
#   #plot(x, Y, ylim = c(-.2, 1.2), col = "red")
#   #par(new = TRUE)
#   #plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = "green")
#
#
#
#   #
#
#   ## Function should a list containing two elements:
#   ## The first element of the list is the spline regression
#   ## beta vector, which should be p + 1 dimensional (here,
#   ## p is the number of cuts we made to the x-axis).
#   ## The second element is y.hat, the predicted Y values
#   ## using the spline regression beta vector. This
#   ## can be a numeric vector or matrix.
#   output <- list(beta_spline = beta_spline, predicted_y = Yhat)
#   return(output)
#
# }
#
# ####QR VS SWEEP, QR benchmark:
# ##########################################################################################################
# myRidge_QR <- function(X, Y, lambda){
#
#   # Perform ridge regression of Y on X.
#   #
#   # X: an n x p matrix of explanatory variables.
#   # Y: an n vector of dependent variables. Y can also be a
#   # matrix, as long as the function works.
#   # lambda: regularization parameter (lambda >= 0)
#   # Returns beta, the ridge regression solution.
#
#   ##################################
#   ## FILL IN THIS SECTION OF CODE ##
#   ##################################
#   n = dim(X)[1]
#   p = dim(X)[2]
#   Z = cbind(rep(1,n),X,Y)
#   A = t(Z) %*% Z
#   D = diag(rep(lambda, p+2))
#   D[1,1] = 0
#   D[p+2,p+2] = 0
#   A = A + D
#   #S = mySweep(A,p+1)
#   #beta_ridge = S[1:(p+1),p+2]
#
#
#   #QR
#   R <- myQR(A)$R
#   R1 <- R[c(1:(p+1)), c(1:(p+1))]
#   Y1 <- R[c(1:(p+1)), p+2]
#   beta_ridge <- solve(R1,Y1)
#
#   ## Function should output the vector beta_ridge, the
#   ## solution to the ridge regression problem. beta_ridge
#   ## should have p + 1 elements.
#   return(beta_ridge)
#
# }
#
#
# ####################################################
# ## Function 4: Piecewise linear spline regression ##
# ####################################################
#
#
# mySpline_QR <- function(x, Y, lambda, p = 100){
#
#   # Perform spline regression of Y on X.
#   #
#   # x: An n x 1 vector or matrix of explanatory variables.
#   # Y: An n x 1 vector of dependent variables. Y can also be a
#   # matrix, as long as the function works.
#   # lambda: regularization parameter (lambda >= 0)
#   # p: Number of cuts to make to the x-axis.
#
#   ##################################
#   ## FILL IN THIS SECTION OF CODE ##
#   ##################################
#   #n = 20
#   #p = 100
#   sigma = .1
#   #lambda = 1.
#
#   #x = runif(n)
#   #x = sort(x)
#   #Y = x^2 + rnorm(n)*sigma
#   X = matrix(x, nrow=n)
#   for (k in (1:(p-1))/p)
#     X = cbind(X, (x>k)*(x-k))
#   beta_spline = myRidge_QR(X,Y,lambda)
#   Yhat = cbind(rep(1, n), X)%*%beta_spline
#
#
#
#   #plot the regression with different lambda
#
#   #plot(x, Y, ylim = c(-.2, 1.2), col = "red")
#   #par(new = TRUE)
#
#   #lambda_val =c(10,1,0.1,0.01)
#   #color = c("blue", "purple", "green", "orange")
#   #for (i in 1:4) {
#   #par(new = TRUE)
#   #beta_spline = myRidge(X,Y,lambda_val[i])
#   #Yhat = cbind(rep(1, n), X)%*%beta_spline
#   #plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = color[i])
#   #}
#
#   #plot(x, Y, ylim = c(-.2, 1.2), col = "red")
#   #par(new = TRUE)
#   #plot(x, Yhat, ylim = c(-.2, 1.2), type = 'l', col = "green")
#
#
#
#   #
#
#   ## Function should a list containing two elements:
#   ## The first element of the list is the spline regression
#   ## beta vector, which should be p + 1 dimensional (here,
#   ## p is the number of cuts we made to the x-axis).
#   ## The second element is y.hat, the predicted Y values
#   ## using the spline regression beta vector. This
#   ## can be a numeric vector or matrix.
#   output <- list(beta_spline = beta_spline, predicted_y = Yhat)
#   return(output)
#
# }
#
#
