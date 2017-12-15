
import numpy as np


################################
## Function 1: Sweep operator ##
################################

def mySweep(B, m):
    
    """
    Perform a SWEEP operation on A with the pivot element A[m,m].
    
    :param A: a square matrix (np.array).
    :param m: the pivot element is A[m, m].
    :returns a swept matrix (np.array). Original matrix is unchanged.
    
    FILL IN THE BODY OF THIS FUNCTION BELOW
    """
    
    A = np.copy(B)   
    n = A.shape[0]
    for k in range(m):
        for i in range(n):
            for j in range(n):
                if (i != k and j != k):
                    A[i,j] = A[i,j] - A[i,k] * A[k,j] / A[k,k]
    
        for i in range(n):
            if i != k:
                A[i,k] = A[i,k] / A[k,k]

        for j in range(n):
            if j != k:
                A[k,j] = A[k,j] / A[k,k]

        A[k,k] = -1/A[k,k]


    
    ## The function outputs the matrix (np.array) B
    return(A)



########################################################
## Function 2: Linear regression using Sweep operator ##
########################################################

def myLinearRegression(X, Y):
  
  """
  Find the regression coefficient estimates beta_hat
  corresponding to the model Y = X * beta + epsilon
  Your code must use the sweep operator you coded above.
  Note: we do not know what beta is. We are only 
  given a matrix X and a vector Y and we must come 
  up with an estimate beta_hat.
  
  X: an 'n row' by 'p column' matrix (np.array) of input variables.
  Y: an n-dimensional vector (np.array) of responses

  FILL IN THE BODY OF THIS FUNCTION BELOW
  """
  
  ## Let me start things off for you...
  n = X.shape[0]
  p = X.shape[1]
  
  Z = np.hstack((np.ones((n,1)), X, Y.reshape(n,1)))
  A = np.dot(np.transpose(Z),Z)
  S = mySweep(A, p+1)
  beta_hat = S[range(p+1),p+1]
  
  ## Function returns the (p+1)-dimensional vector (np.array) 
  ## beta_hat of regression coefficient estimates
  return beta_hat
  

