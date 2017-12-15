

#####################################
## Function 1: Lasso solution path ##
#####################################

myLasso <- function(X, Y, lambda_all){
  
  # Find the lasso solution path for various values of 
  # the regularization parameter lambda.
  # 
  # X: n x p matrix of explanatory variables.
  # Y: n dimensional response vector
  # lambda_all: Vector of regularization parameters. Make sure 
  # to sort lambda_all in decreasing order for efficiency.
  #
  # Returns a matrix containing the lasso solution vector 
  # beta for each regularization parameter.
 
  #######################
  ## FILL IN CODE HERE ##
  #######################
  p = ncol(X)
  L = length(lambda_all)
  
  beta = matrix(rep(0, p), nrow = p)
  beta_all = matrix(rep(0, p*L), nrow = p)
  
  sum_sqr = rep(0, p)
  for (j in 1:p) {
    sum_sqr[j] = sum(X[, j]^2)
  }
  
  for (l in 1:L) {
    for (i in 1:10) {
      for (j in 1:p) {
        db = sum(Y*X[,j])/sum_sqr[j]
        b = beta[j] + db
        b = sign(b) * max(0, abs(b) - lambda_all[l] / sum_sqr[j])
        db = b - beta[j]
        Y= Y - X[,j] * db
        beta[j] = b
      }
    }
    beta_all[, l] = beta
  }
  #matplot(t(matrix(rep(1, p), nrow = 1)%*%abs(beta_all)), t(beta_all), xlab = "Lasso Solution Path", ylab = "Beta Value", type = 'l')
  
  ## Function should output the matrix beta_all, the 
  ## solution to the lasso regression problem for all
  ## the regularization parameters. 
  ## beta_all is p x length(lambda_all)
  return(beta_all)
  
}



