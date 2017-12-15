

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
  ## Use myQR (or myQRC) inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################  
  n <- nrow(X)
  p <- ncol(X)
  z <- cbind(X,Y)
  R <- myQR(z)[2]
  R_mat <- matrix(unlist(R), ncol = n, byrow = TRUE) ##change the list to matrix
  R_mat <- t(R_mat)                                     ##keep the same data 
  R1 <- R_mat[1:(p), 1:(p)]
  Y1 <- R_mat[1:(p), p+1]
  beta_ls <- solve(R1,Y1)
  
  
  ## Function returns the least squares solution vector
  return(beta_ls)
  
}

######################################
## Function 3: Logistic regression  ##
######################################

## Expit/sigmoid function
expit <- function(x){
  1 / (1 + exp(-x))
}

myLogistic <- function(X, Y){

  ## Perform the logistic regression of Y on X
  ## Input: 
  ## X is an n x p matrix of explanatory variables
  ## Y is an n dimensional vector of binary responses
  ## Use myLM (or myLMC) inside of this function
  
  ########################
  ## FILL IN CODE BELOW ##
  ########################
  r <- nrow(X)
  c <- ncol(X)
  
  beta <- matrix(rep(0, c), nrow = c)
  epsilon = 1e-6
  
  while(1) {
    eta <- X %*% beta
    pr <- expit(eta)
    w <- pr * (1 - pr)
    z <- eta + (Y - pr) / w
    sw <- sqrt(w)
    mw <- matrix(sw, r, c)
    X_work <- mw * X
    Y_work <- sw * z
    beta_new <- myLM(X_work,Y_work)
    err <- sum(abs(beta_new - beta))
    beta <- beta_new
    if (err < epsilon)
      break
  }
  
  
  ## Function returns the logistic regression solution vector
  beta  
    
}


## Optional testing (comment out!)
 #n <- 100
 #p <- 4
 
 #X    <- matrix(rnorm(n * p), nrow = n)
 #beta <- rnorm(p)
 #Y    <- 1 * (runif(n) < expit(X %*% beta))
 
 #logistic_beta <- myLogistic(X, Y)
 #logistic_beta    

