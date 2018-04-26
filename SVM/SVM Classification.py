import pandas as pd
import pylab as pl
import numpy as np
import gc
# Quadratic constraint solver
import cvxopt.solvers
from sklearn.model_selection import train_test_split

print('Reading data')
df = pd.DataFrame()
df = pd.read_csv("../res/datasets/bitcoin-data/bitflyerJPY_1-min_data_2017-07-04_to_2018-01-08.csv")

# Split dataframe into training and test sets
train, test = train_test_split(df[:2000], test_size=0.2)

# Training class data
train_c = list()
test_c = list()

for index, row in train.iterrows():
    if index > 0 and index < (len(train) - 1):
        # Checks current price vs previous price
        if row['Close'] > train.iloc[index-1,4]:
            train_c.append(1)
        else:
            train_c.append(-1)
    else:
        train_c.append(-1)

for index, row in test.iterrows():
    if index > 0 and index < (len(test) - 1):
        # Checks current price vs previous price
        if row['Close'] > test.iloc[index-1,4]:
            test_c.append(1)
        else:
            test_c.append(-1)
    else:
        test_c.append(-1)

print('Done reading data')
print('Increase Points -- Class 1 length: ' + str(train_c.count(1)))
print('Decrease Points -- Class -1 length: ' + str(train_c.count(-1)))

print('Train Class length: ' + str(len(train_c)))
print('Train Set length: ' + str(len(train)))

class SVM():
    """
    Fit the model to the data. Requires a 2D array of data X
    and a 1D array of corresponding class. Must all be floats!
    Returns nothing, but calculates internal variables in object.
    """
    def fit(self, X, y):

        # Scrape data size
        rows, columns = X.shape

        print('Rows: ' + str(rows))
        # Create the Gram matrix
        # P, q A, b, G, n from textbook, http://cvxopt.org/
        K = np.zeros((rows, rows))

        for i in range(rows):
            for j in range(rows):
                K[i,j] = np.dot(X[i], X[j])

        P = cvxopt.matrix(K * np.outer(y,y))
        q = cvxopt.matrix(-1 * np.ones(rows))
        A = cvxopt.matrix(y, (1,rows))
        b = cvxopt.matrix(0.0)
        G = cvxopt.matrix(-1 * np.diag(np.ones(rows)))
        h = cvxopt.matrix(np.zeros(rows))

        # print(P,q,A,b,G,h)
        # Plug the Gram matrix into CVXOPT package
        solution = cvxopt.solvers.qp(P, q, G, h, A, b, kktsolver='ldl')

        # Store lagrange multipliers from solvers.qp
        # Ravel returns a contiguous flattened array.
        lag_mult = np.ravel(solution['x'])

        # Avoid machine precision errors.
        # Lagrange multipliers must be greater than zero.
        sup_vec = lag_mult > 1e-6

        # Arange returns evenly spaced values within a given interval.
        lagrange_indexes = np.arange(len(lag_mult))[sup_vec]

        # Build an array of our lagrange multipliers that were greater than zero.
        self.lag_mult = lag_mult[sup_vec]

        # Store data points at associated support vector data points.
        self.sup_vec= X[sup_vec]
        self.sv_y = y[sup_vec]

        print("Found " + str(len(self.lag_mult)) + " support vectors.")

        # Find the intercept of our hyperplane
        self.b = 0
        for n in range(len(self.lag_mult)):
            self.b += self.sv_y[n]
            self.b -= np.sum(self.lag_mult * self.sv_y * K[lagrange_indexes[n], sup_vec])
        self.b /= len(self.lag_mult)

        # Calculate the weight W
        self.w = np.zeros(columns)
        for n in range(len(self.lag_mult)):
            self.w += self.lag_mult[n] * self.sv_y[n] * self.sup_vec[n]


    """
    Attempt to classify a new point. X must be in the same format
    as above, a 2D array of floats.
    """
    def predict(self, X):
        return np.sign(np.dot(X, self.w) + self.b)

def plot_margin(X1_train, X2_train, clf):
    def f(x, w, b, c=0):
        # given x, return y such that [x,y] in on the line
        # w.x + b = c
        return (-w[0] * x - b + c) / w[1]

    pl.plot(X1_train[:,0], X1_train[:,1], "ro")
    pl.plot(X2_train[:,0], X2_train[:,1], "bo")
    pl.scatter(clf.sv[:,0], clf.sv[:,1], s=100, c="g")

    # w.x + b = 0
    a0 = -4; a1 = f(a0, clf.w, clf.b)
    b0 = 4; b1 = f(b0, clf.w, clf.b)
    pl.plot([a0,b0], [a1,b1], "k")

    # w.x + b = 1
    a0 = -4; a1 = f(a0, clf.w, clf.b, 1)
    b0 = 4; b1 = f(b0, clf.w, clf.b, 1)
    pl.plot([a0,b0], [a1,b1], "k--")

    # w.x + b = -1
    a0 = -4; a1 = f(a0, clf.w, clf.b, -1)
    b0 = 4; b1 = f(b0, clf.w, clf.b, -1)
    pl.plot([a0,b0], [a1,b1], "k--")

    pl.axis("tight")
    pl.show()

# Training data
df_X = train[['Timestamp', 'Close']].copy()
df_Y = train_c

# Test data
df_test_X = test[['Timestamp', 'Close']].copy()
df_test_Y = test_c

# Garbage Collector for memory
gc.collect()

# Training data format for SVM class
df_X = df_X.values
df_Y = np.hstack(df_Y)


# Test data format for SVM class
df_test_X = df_test_X.values
df_test_Y = np.hstack(df_test_Y)

# Data must be a float
df_Y = df_Y.astype(np.float)
df_test_Y = df_test_Y.astype(np.float)

# Fit SVM model and predict values
svm = SVM()
svm.fit(df_X, df_Y)
y_predict = svm.predict(df_test_X)
correct = np.sum(y_predict == df_test_Y)

print( "Predicted " + str(correct) + " of " + str(len(y_predict)) + " correctly.")
