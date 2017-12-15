


# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;



/* ~~~~~~~~~~~~~~~~~~~~~~~~~ 
Problem 1: Sweep operator 
~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
mat mySweepC(const mat A, int m){
  
  /*
  Perform a SWEEP operation on A with the pivot element A[m,m].
  
  A: a square matrix (mat).
  m: the pivot element is A[m, m]. 
  Returns a swept matrix B (which is m by m).
  
  Note the "const" in front of mat A; this is so you
  don't accidentally change A inside your code.
  
#############################################
## FILL IN THE BODY OF THIS FUNCTION BELOW ##
#############################################
  */
  
  mat B = A;
  int n = B.n_rows;
  
  for (int k = 0; k < m; k++) {
    for (int i = 0; i < n; i++) {
      for (int j = 0; j < n; j++) {
        if ((i != k) && (j != k)) {
          B(i,j) = B(i,j) - B(i,k)*B(k,j)/B(k,k);
        }
      }
    }
    for (int i = 0; i < n; i++) {
      if (i != k)
        B(i,k) = B(i,k)/B(k,k);
    }
    for (int j = 0; j < n; j++) {
      if (j != k) 
        B(k,j) = B(k,j)/B(k,k);
    }
    B(k,k) = -1/B(k,k);
  }
  
  
  
  
  // Return swept matrix B
  return(B);
  
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
Problem 2: Linear regression using the sweep operator 
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */


// [[Rcpp::export()]]
mat myLinearRegressionC(const mat X, const mat Y){
  
  /*  
  Find the regression coefficient estimates beta_hat
  corresponding to the model Y = X * beta + epsilon
  Your code must use the sweep operator you coded above.
  Note: we do not know what beta is. We are only 
  given a matrix X and a matrix Y and we must come 
  up with an estimate beta_hat.
  
  X: an 'n row' by 'p column' matrix of input variables.
  Y: an 'n row' by '1 column' matrix of responses
  
#############################################
## FILL IN THE BODY OF THIS FUNCTION BELOW ##
#############################################
  */  
  
  // Let me start things off for you...
  int n = X.n_rows;
  int p = X.n_cols;
  
  mat I(n,1);
  //create an nx1 matrix
  for (int i = 0; i < n; i++) {
    I(i,0) = 1; //make it all be 1
  }
  mat temp = join_rows(I,X); //cbind
  mat Z = join_rows(temp, Y);
  //return Z;
  mat A = Z.t() * Z; //matrix multiplication
  mat S = mySweepC(A, p+1);
  //myreturn S;
  mat beta_hat = S.submat(0,p+1,p,p+1); //row_from, col_from, row_to,col_to
  
  // Function returns the 'p+1' by '1' matrix 
  // beta_hat of regression coefficient estimates
  return(beta_hat);
  
}



