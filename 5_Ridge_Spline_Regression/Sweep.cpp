

# include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;


/* ~~~~~~~~~~~~~~~~~~~~~~~~~
 Sign function for later use
 ~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
double signC(double d){
  return d<0?-1:d>0? 1:0;
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 Problem 1: QR decomposition
~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */

// [[Rcpp::export()]]
List myQRC(const mat A){

  /*
  Perform QR decomposition on the matrix A
  Input:
  A, an n x m matrix (mat)

  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################

  */
  int n = A.n_rows;
  int m = A.n_cols;

  mat Q = mat(n,n);
  Q.eye();
  mat R = mat(A);

  for (int k = 0; k < m-1; k++) {
    mat X = mat(n,1,fill::zeros);

    for (int i = k; i < n; i++) {
      X(i,0) = R(i,k);
    }

    mat V = mat(X);
    V(k) = X(k) + signC(X(k)) * norm(X);
    double S = norm(V);
    if (S != 0) {
      mat u = V/S;
      R = R - (2 * (u * u.t() * R));
      Q = Q - (2 * (u * u.t() * Q));
    }
  }

  Rcpp::List output = Rcpp::List::create(
    Rcpp::Named("Q"),
    Rcpp::Named("R")
  );
  // Function should output a List 'output', with
  // Q.transpose and R
  // Q is an orthogonal n x n matrix
  // R is an upper triangular n x m matrix
  // Q and R satisfy the equation: A = Q %*% R
  output["Q"] = Q.t();
  output["R"] = R;
  return(output);

}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~
   Problem 2: Sweep operator
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

