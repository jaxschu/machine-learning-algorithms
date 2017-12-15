

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
  
  
  
  // Function should output a List 'output', with 
  // Q.transpose and R
  // Q is an orthogonal n x n matrix
  // R is an upper triangular n x m matrix
  // Q and R satisfy the equation: A = Q %*% R
  Rcpp::List output = Rcpp::List::create(
    Rcpp::Named("Q"),
    Rcpp::Named("R")
  );
  
  
  output["Q"] = Q.t();
  output["R"] = R;
  return(output);
  

}
  
/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 
   Problem 2: Linear regression using QR 
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ */
  
  
// [[Rcpp::export()]]
mat myLinearRegressionC(const mat X, const mat Y){
    
  /*  
  Perform the linear regression of Y on X
  Input: 
  X is an n x p matrix of explanatory variables
  Y is an n dimensional vector of responses
  Do NOT simulate data in this function. n and p
  should be determined by X.
  Use myQRC inside of this function
  
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
  
  */  
  int n = X.n_rows;
  int p = X.n_cols;
  mat I(n,1);  //create an nx1 matrix
  for (int i = 0; i < n; i++) {
    I(i,0) = 1; //make it all be 1
  }
  mat temp = join_rows(I,X); //cbind
  mat Z = join_rows(temp, Y);
  List QR = myQRC(Z);
  mat R = QR["R"];
  //myreturn R;
  mat R1 = R.submat(0,0,p,p);
  mat Y1 = R.submat(0,p+1,p,p+1);
  
  mat beta_ls = solve(R1,Y1);
  // Function returns the 'p+1' by '1' matrix 
  // beta_ls of regression coefficient estimates
  return(beta_ls.t());
  
}  

/* ~~~~~~~~~~~~~~~~~~~~~~~~ 
 Problem 3: PCA based on QR 
 ~~~~~~~~~~~~~~~~~~~~~~~~~~ */


// [[Rcpp::export()]]
List myEigen_QRC(const mat A, const int numIter = 1000){
  
  /*  
  
  Perform PCA on matrix A using your QR function, myQRC.
  Input:
  A: Square matrix
  numIter: Number of iterations
   
  #############################################
  ## FILL IN THE BODY OF THIS FUNCTION BELOW ##
  #############################################
   
   */  
  int T = numIter;
  mat B = A;
  int r = B.n_rows;
  int c = B.n_cols;
  mat V = mat(r,r);
  for (int i = 0 ; i < r; i++) {
    for (int j = 0 ; j < r; j++) {
      V(i,j) = rand();  //fill the matrix v (rxr) with random value;
    }
  }
  //return v;
  //mat Q = myQRC(V)["Q"];
  //mat R = myQRC(V)["R"];
  mat RR;
  mat QQ;
  for (int k = 0; k < T; k++) {
    mat Q = myQRC(V)["Q"];
    mat R = myQRC(V)["R"];
    RR = R;   //this is used to avoid operator "=" ambiguity
    QQ = Q;   //this is used to avoid operator "=" ambiguity
    V = B * Q;
  }
  NumericVector D;
  for (int i = 0; i < r; i++) {
    for (int j = 0; j < r; j++) {
      if (i == j) {
        D.push_back(RR(i,j));
      }
    }
  }
  // Function should output a list with D and V
  // D is a vector of eigenvalues of A
  // V is the matrix of eigenvectors of A (in the 
  // same order as the eigenvalues in D.)
  Rcpp::List output = Rcpp::List::create(
    Rcpp::Named("D"),
    Rcpp::Named("V")
  );
  
  output["D"] = D;
  output["V"] = QQ;
  return(output);

}
  
