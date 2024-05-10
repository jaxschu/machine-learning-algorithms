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

def myGaussJordanVec(A, m):
    """
    Perform Gauss Jordan elimination on A using a vectorized approach.
    
    A: a square matrix.
    m: the pivot element is A[m, m].
    Returns a matrix with the identity matrix 
    on the left and the inverse of A on the right.
    """
    n = A.shape[0]
    B = np.hstack((A, np.identity(n)))
    for k in range(n):  # 修改为 n，确保处理所有行
        B[k, :] = B[k, :] / B[k, k]  # 缩放行使主对角线元素为 1
        for i in range(n):
            if i != k:
                B[i, :] = B[i, :] - B[k, :] * B[i, k]  # 消除第i行的第k列

    return B

def myLinearRegression(X, Y):
    """
    Find the regression coefficient estimates beta_hat
    corresponding to the model Y = X * beta + epsilon
    using Gauss Jordan elimination method.
    
    X: an 'n row' by 'p column' matrix (np.array) of input variables.
    Y: an n-dimensional vector (np.array) of responses
    """
    n, p = X.shape
    
    # Adding intercept term by appending a column of ones
    X = np.hstack([np.ones((n, 1)), X])
    
    # Building the normal equation components
    XTX = np.dot(X.T, X)
    XTY = np.dot(X.T, Y)
    
    # Augmenting XTX with XTY for Gauss-Jordan
    A = np.hstack([XTX, XTY.reshape(-1, 1)])
    
    # Applying Gauss-Jordan elimination
    S = myGaussJordanVec(A, p + 1)  # Using the vectorized Gauss Jordan function
    
    # The last column of S (after Gauss-Jordan) will be the solution beta_hat
    beta_hat = S[:, -1]  # Extracting beta_hat
    
    return beta_hat

# Note: Ensure to adjust the Gauss Jordan function to handle non-square matrices if needed.

  


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



