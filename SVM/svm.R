

# Support Vector Model
# Parameters: train (Training data - 80%), test(Testing data - 20%)
SVM = function(train, test) {
	# Features -- BTC Volume in Currency
	train.X = train[6]
	test.X = test[6]

	# Labels -- BTC Open Value
	train.y = train[1]
	test.y = test[1]

}

# Computes Lagrange multipliers
# Returns Lagrange multipliers
lagrange_multipliers = function(X, y) {
	K = gram_matrix(X)

}

# Gram Matrix
# Returns the Gram Matrix
gram_matrix = function(X) {


}


# Reads data from .csv file and splits it into
# training/testing sets
read_data = function() {
	print('Reading data...')

	# Bitcoin dataset at 1-minute interval Jan 2012 - Jan 2018
	df <- read.csv('res/datasets/bitcoin-data/bitflyerJPY_1-min_data_2017-07-04_to_2018-01-08.csv')
	# Creates bound of 80% Train, 20% Test
	bound <- floor(0.8*nrow(df))
	df <- df[sample(nrow(df)), ]

	# Training set
	df.train <- df[1:bound, ]

	# Testing set
	df.test <- df[(bound+1):nrow(df), ]

	# Calls SVM Classifier with the training and testing sets
	SVM(df.train, df.test)
}

# Calls ReadData
read_data()
