###Set working directory
setwd("C:/Users/smcdo/OneDrive/Documents/Model_Framework/Benchmarks")

###Read x and y data
data = read.csv("x_data.csv", row.names=1)
y = read.csv("y_data.csv", row.names=1)
data = as.matrix(data)
y = as.matrix(y)

######SVM
library(e1071)
svm_mod = svm(data, y, kernel = 'radial', cost = 1, epsilon = .1, gamma=.1, tolerance=.0001, scale=FALSE, shrinking=TRUE)
summary(svm_mod)
fitted(svm_mod)

#SVM CV
tune.svm(x=data, y=y, gamma = 2^(-1:1), cost=2^(2:4), kernel = 'radial', epsilon = .1, tune.control(sampling = 'cross', cross = 5))

