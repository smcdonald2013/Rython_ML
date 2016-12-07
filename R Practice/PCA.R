###Set working directory
setwd("C:/Users/smcdo/OneDrive/Documents/Model_Framework/Benchmarks")

###Read x and y data
data = read.csv("x_data.csv", row.names=1)
y = read.csv("y_data.csv", row.names=1)
data = as.matrix(data)
y = as.matrix(y)

######PCA
####Princomp
##Method: Covariance (Default), correlation also available
##Solver: Eigendecomposition
##Bias: N rather than N-1 
pc = princomp(data, cor=FALSE)
pc$loadings

####Prcomp 
##Method: Covariance (Default), correlation also available
##Solver: SVD
##Bias: N
pc = prcomp(data, center=TRUE, scale=FALSE)
pc$rotation