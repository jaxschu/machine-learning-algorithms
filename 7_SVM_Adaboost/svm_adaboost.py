
import numpy as np
import sklearn.datasets as ds
from sklearn.model_selection import train_test_split
# import matplotlib.pyplot as plt

def prepare_data(valid_digits=np.array((6, 5))):
    ## valid_digits is a vector containing the digits
    ## we wish to classify.
    ## Do not change anything inside of this function
    if len(valid_digits) != 2:
        raise Exception("Error: you must specify exactly 2 digits for classification!")

    data = ds.load_digits()
    labels = data['target']
    features = data['data']

    X = features[(labels == valid_digits[0]) | (labels == valid_digits[1]), :]
    Y = labels[(labels == valid_digits[0]) | (labels == valid_digits[1]),]

    X = np.asarray(map(lambda k: X[k, :] / X[k, :].max(), range(0, len(X))))

    Y[Y == valid_digits[0]] = 0
    Y[Y == valid_digits[1]] = 1

    X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size=0.25, random_state=10)
    Y_train = Y_train.reshape((len(Y_train), 1))
    Y_test = Y_test.reshape((len(Y_test), 1))

    return X_train, Y_train, X_test, Y_test


####################################################
## Function 1: Support vector machine  ##
####################################################

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Train an SVM to classify the digits data ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

def my_SVM(X_train, Y_train, X_test, Y_test, lamb=0.01, num_iterations=200, learning_rate=0.1):
    ## X_train: Training set of features
    ## Y_train: Training set of labels corresponding to X_train
    ## X_test: Testing set of features
    ## Y_test: Testing set of labels correspdonding to X_test
    ## lamb: Regularization parameter
    ## num_iterations: Number of iterations.
    ## learning_rate: Learning rate.

    ## Function should learn the parameters of an SVM.


    n = X_train.shape[0]
    p = X_train.shape[1] + 1
    X_train1 = np.concatenate((np.repeat(1, n, axis=0).reshape((n, 1)), X_train), axis=1)
    Y_train = 2 * Y_train - 1
    beta = np.repeat(0., p, axis=0).reshape((p, 1))

    ntest = X_test.shape[0]
    X_test1 = np.concatenate((np.repeat(1, ntest, axis=0).reshape((ntest, 1)), X_test), axis=1)
    Y_test = 2 * Y_test - 1

    acc_train = np.repeat(0., num_iterations, axis=0)
    acc_test = np.repeat(0., num_iterations, axis=0)

    #######################
    ## FILL IN CODE HERE ##
    #######################
    for i in range(num_iterations):
        s = np.dot(X_train1, beta)
        db = s * Y_train < 1
        dbeta = np.dot(np.ones((1,n)), (np.tile(db*Y_train,p)*X_train1)/n)
        beta = beta - learning_rate * np.transpose(dbeta)
        beta[1:p] = beta[1:p] - lamb * beta[1:p]

        acc_train[i] = np.mean(np.sign(s * Y_train))
        acc_test[i] = np.mean(np.sign(np.dot(X_test1, beta) * Y_test))



    ## Function should output 3 things:
    ## 1. The learned parameters of the SVM, beta
    ## 2. The accuracy over the training set, acc_train (a "num_iterations" dimensional vector).
    ## 3. The accuracy over the testing set, acc_test (a "num_iterations" dimensional vector).

    return beta, acc_train, acc_test


######################################
## Function 2: Adaboost ##
######################################

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##
## Use Adaboost to classify the digits data ##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ##

def my_Adaboost(X_train, Y_train, X_test, Y_test, num_iterations=200):
    ## X_train: Training set of features
    ## Y_train: Training set of labels corresponding to X_train
    ## X_test: Testing set of features
    ## Y_test: Testing set of labels correspdonding to X_test
    ## num_iterations: Number of iterations.

    ## Function should learn the parameters of an Adaboost classifier.

    n = X_train.shape[0]
    p = X_train.shape[1]
    threshold = 0.8

    X_train1 = 2 * (X_train > threshold) - 1
    Y_train = 2 * Y_train - 1

    X_test1 = 2 * (X_test > threshold) - 1
    Y_test = 2 * Y_test - 1

    beta = np.repeat(0., p).reshape((p, 1))
    w = np.repeat(1. / n, n).reshape((n, 1))

    weak_results = np.multiply(Y_train, X_train1) > 0

    acc_train = np.repeat(0., num_iterations, axis=0)
    acc_test = np.repeat(0., num_iterations, axis=0)

    #######################
    ## FILL IN CODE HERE ##
    #######################
    for i in range(num_iterations):
        w = w / np.sum(w)
        weighted_weak_results = w[:,0].reshape((n,1)) * weak_results
        weighted_accuracy = weighted_weak_results.sum(axis = 0)

        e = 1 - weighted_accuracy
        j = np.argmin(e)

        dbeta = np.log((1-e[j]) / e[j]) /2
        beta[j] = beta[j] + dbeta
        w = w * np.exp(-Y_train * X_train1[:,j].reshape((n,1)) * dbeta)
        
        acc_train[i] = np.mean(np.sign(np.dot(X_train1, beta)) == Y_train)
        acc_test[i] = np.mean(np.sign(np.dot(X_test1, beta)) == Y_test)



    ## Function should output 3 things:
    ## 1. The learned parameters of the adaboost classifier, beta
    ## 2. The accuracy over the training set, acc_train (a "num_iterations" dimensional vector).
    ## 3. The accuracy over the testing set, acc_test (a "num_iterations" dimensional vector).
    return beta, acc_train, acc_test


############################################################################
## Testing your functions and visualize the results here##
############################################################################

X_train, Y_train, X_test, Y_test = prepare_data()






