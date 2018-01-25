# CryptoPricePredictor

## About

### Project Proposal

**Overall Objective:** Predict future prices of various cryptocurrency
* Implement two machine learning algorithms
* Find datasets containing detailed history of various coins to train our algorithm
* Compare performance of different algorithms

**Justification:** Being able to accurately predict prices of cryptocurrency could enable someone to make smarter investments.

### Algorithms of Choice

**Time Series Analysis - Regression**
* Support Vector Machine (SVM)
  * Widely applied and well surveyed
  * Non-linear and non-stationary process

* Generalized Additive Model (GAM)
  * Application: Facebook's Prophet, a forecasting tool
  * Decomposition: Trend + Cyclical + Seasonal + Irregular
  
### Datasets

**Kaggle Datasets**

* Bitcoin's data at 1-minute inverals from Jan 2012 - Jan 2018
  * Attributes
    * Timestamp
    * Price (Open/High/Low/Close)
    * Volume in BTC & USD (Value of amount transacted in 24 hours)

* Several top coin's data at daily intervals over several years
  * Attributes
    * Timestamp
    * Price (Open/High/Low/Close)
    * Market Cap (Coin Price * Circulating Supply)


### Authors
* Benjamin Carpenter
* Jacob Pauly
* Shuning Jin
* Tristan Larsin
