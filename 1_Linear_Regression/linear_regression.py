import numpy as np

#Function 1: Plain version of Gauss Jordan
def myGaussJordan(A, m):
    """
    Perform Gauss Jordan elimination on A.
    
    A: a square matrix.
    m: the pivot element is A[m, m].
    Returns a matrix with the identity matrix 
    on the left and the inverse of A on the right. 
    """
    n = A.shape[0]
    B = np.hstack((A, np.identity(n)))
    for k in range(n):  # 修改为n，以确保处理所有行
        a = B[k, k]
        for j in range(n * 2):
            B[k, j] = B[k, j] / a
        for i in range(n):
            if i != k:
                b = B[i, k]
                for j in range(n * 2):
                    B[i, j] = B[i, j] - B[k, j] * b

    return B

  


####################################################
## Function 2: Vectorized version of Gauss Jordan ##
####################################################

def myGaussJordanVec(A, m):
  
  """
  Perform Gauss Jordan elimination on A.
  
  A: a square matrix.
  m: the pivot element is A[m, m].
  Returns a matrix with the identity matrix 
  on the left and the inverse of A on the right.
  
  FILL IN THE BODY OF THIS FUNCTION BELOW
  """
  n = A.shape[0]
  B = np.hstack((A, np.identity(n)))
  for k in range(m):
    B[k,] = B[k,] / B[k,k]
    for i in range(n):
        if (i != k):
            B[i,] = B[i,] - B[k,] * B[i,k]
  
  
  ## Function returns the np.array B
  return B


######################################################
## Function 3: Linear regression using Gauss Jordan ##
######################################################

def myLinearRegression(X, Y):
  
  """
  Find the regression coefficient estimates beta_hat
  corresponding to the model Y = X * beta + epsilon
  Your code must use one of the 2 Gauss Jordan 
  functions you wrote above (either one is fine).
  Note: we do not know what beta is. We are only 
  given a matrix X and a vector Y and we must come 
  up with an estimate beta_hat.
  
  X: an 'n row' by 'p column' matrix (np.array) of input variables.
  Y: an n-dimensional vector (np.array) of responses

  FILL IN THE BODY OF THIS FUNCTION BELOW
  """

  n = X.shape[0]
  p = X.shape[1]
    
  temp1 = np.ones((n,1))
  temp2 = np.hstack((temp1,X))
  Z = np.hstack((temp2,Y))

  A = np.dot(Z.transpose(),Z)
  
  S = myGaussJordan(A, p + 1)
  
  last_row = S[p+1]
  index = np.size(last_row) - 1
  beta_hat = -last_row[p+2: index]
  ## Function returns the (p+1)-dimensional vector (np.array) 
  ## beta_hat of regression coefficient estimates
  #print beta_hat
  return beta_hat
  


########################################################
## Optional examples (comment out before submitting!) ##
########################################################

#def testing_Linear_Regression():
  
  ## This function is not graded; you can use it to 
  ## test out the 'myLinearRegression' function 
  #n = 100
  #p = 3
  #X = np.random.standard_normal((n*p)).reshape((n,p))
  
  #beta = np.arange(1,p+1,1).reshape((p,1))
    
  #Y = np.dot(X,beta) + np.random.standard_normal(n).reshape(n,1)

  ## You can set up a similar test function as was 
  ## provided to you in the R file.



