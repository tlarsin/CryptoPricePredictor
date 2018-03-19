

# Support Vector Model
# Parameters: train (Training data - 80%), test(Testing data - 20%)
SVM = function(train, test) {

}

ReadData = function() {
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
ReadData()
