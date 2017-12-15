

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

###############################################
## Function 2: Linear regression based on QR ##
###############################################

myLM <- function(X, Y){
  
  ## Perform the linear regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of responses
  ## Do NOT simulate data in this function. n and p
  ## should be determined by X.
  ## Use myQR inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  n <- nrow(X)
  p <- ncol(X)
  z <- cbind(rep(1,n),X,Y)
  R <- myQR(z)[2]
  R_mat <- matrix(unlist(R), ncol = n, byrow = TRUE) ##change the list to matrix
  R_mat <- t(R_mat)                                     ##keep the same data 
  R1 <- R_mat[c(1:(p+1)), c(1:(p+1))]
  Y1 <- R_mat[c(1:(p+1)), p+2]
  beta_ls <- solve(R1,Y1)
  
  
  ## Function returns the 1 x (p + 1) vector beta_ls, 
  ## the least squares solution vector
  return(beta_ls)
  
}

##################################
## Function 3: PCA based on QR  ##
##################################

myEigen_QR <- function(A, numIter = 1000){
  
  ## Perform PCA on matrix A using your QR function, myQRC.
  ## Input:
  ## A: Square matrix
  ## numIter: Number of iterations
  
  ########################
  ## FILL IN CODE BELOW ##
  ######################## 
  T <- numIter
  B <- A
  r <- dim(B)[1] ## ROW
  c <- dim(B)[2] ## COL
  v <- matrix(rnorm(r * r), nrow = r)
  for (i in (1:T)) {
    Q <- myQR(v)[1] #get Q
    R <- myQR(v)[2] #get R
    Q <- matrix(unlist(Q), ncol = r, byrow = TRUE)  #make the list to matrix
    R <- matrix(unlist(R), ncol = r, byrow = TRUE)  #make the list to matrix
    Q <- t(Q)
    R <- t(R)
    ##Q <- qr.Q(qr(B))
    ##R <- qr.R(qr(B))
    v <- B%*%Q
    
  }
  
  
  ## Function should output a list with D and V
  ## D is a vector of eigenvalues of A
  ## V is the matrix of eigenvectors of A (in the 
  ## same order as the eigenvalues in D.)
  return(list("D" = diag(R), "V" = Q))
  
}


