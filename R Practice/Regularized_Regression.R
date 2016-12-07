###Set working directory
setwd("C:/Users/smcdo/OneDrive/Documents/Model_Framework/Benchmarks")

###Read x and y data
data = read.csv("x_data.csv", row.names=1)
y = read.csv("y_data.csv", row.names=1)
data = as.matrix(data)
y = as.matrix(y)

###Setting python parameters
py_lambda = 10

##########Ridge Regression
#GLMNET and Penalized do not equal because of glmnet's standardization of y
library(glmnet)
model_ridge = glmnet(data, y, lambda=py_lambda/dim(data)[1], alpha=0, intercept=FALSE, standardize = FALSE, family='gaussian', thresh=1e-16)
model_ridge$beta 

library(penalized)
pen_mod = penalized(y, data, lambda2=py_lambda, unpenalized = ~0)
coefficients(pen_mod)

library(microbenchmark)
penalized_bench <- microbenchmark(penalized(y, data, lambda2=py_lambda, unpenalized = ~0), times=100)
glmnet_bench <- microbenchmark(glmnet(data, y, lambda=py_lambda/dim(data)[1], alpha=0, intercept=FALSE, standardize = FALSE, family='gaussian', thresh=1e-16), times=100)

#With Standardization of Y to make them equal, using N as bias rather than N-1
y_center = scale(y, scale=FALSE)
y_var = sqrt(sum(y_center^2)/dim(y_center)[1])
y_stand = y_center/y_var

model_ridge = glmnet(data, y_stand, lambda=py_lambda/dim(data)[1], alpha=0, intercept=FALSE, standardize = FALSE, family='gaussian', thresh=1e-16)
model_ridge$beta 

pen_mod = penalized(y_stand, data, lambda2=py_lambda, unpenalized = ~0)
coefficients(pen_mod)

#CV Ridge
model_cv = cv.glmnet(data, y, family="gaussian", alpha=0, intercept=FALSE)
lam.index = which(model_cv$lambda == model_cv$lambda.1se)
model_cv$glmnet.fit$beta[,lam.index ]
model_cv$lambda.1se
model_cv$lambda.min
py_lam = model_cv$lambda.1se*dim(data)[1]
plot(model_cv)
predict(model_cv, newx = data, s = "lambda.min")

library(glmnet)
model_ridge = glmnet(data, y, lambda=model_cv$lambda.1se, alpha=0, intercept=FALSE, standardize = FALSE, family='gaussian', thresh=1e-16)
model_ridge$beta 


##########Lasso Regression
#GLMNET and Penalized do not equal because of glmnet's standardization of y
#Note that for Lasso and Elastic Net, python's alpha parameter is glmnet's lambda. 
#Python's l1_ratio is lambda
library(glmnet)
model_ridge = glmnet(data, y, lambda=1, alpha=1, intercept=FALSE, standardize = FALSE, family='gaussian', thresh=1e-16)
model_ridge$beta 

library(penalized)
pen_mod = penalized(y, data, lambda1=1*dim(data)[1], unpenalized = ~0)
coefficients(pen_mod)

#With Standardization of Y to make them equal
y_center = scale(y, scale=FALSE)
y_var = sqrt(sum(y_center^2)/dim(y_center)[1])
y_stand = y_center/y_var
model_ridge = glmnet(data, y_stand, lambda=1, alpha=1, intercept=FALSE, standardize = FALSE, family='gaussian', thresh=1e-16)
model_ridge$beta 

pen_mod = penalized(y_stand, data, lambda1=1*dim(data)[1], unpenalized = ~0)
coefficients(pen_mod)

#CV Lasso
model_cv = cv.glmnet(data, y, family="gaussian", alpha=1, intercept=FALSE)
lam.index = which(model_cv$lambda == model_cv$lambda.1se)
model_cv$glmnet.fit$beta[,lam.index ]
model_cv$lambda.1se
model_cv$lambda.min
py_lam = model_cv$lambda.1se*dim(data)[1]
plot(model_cv)
predict(model_cv, newx = data, s = "lambda.min")

#####Elastic Net
model_en = glmnet(data, y, lambda=1, alpha=.5, intercept=FALSE, standardize = FALSE, family='gaussian', thresh=1e-16)
model_en$beta

pen_mod = penalized(y, data, lambda1=.5*dim(data)[1], lambda2 = .5*dim(data)[1], unpenalized = ~0)
coefficients(pen_mod)


#With Standardization of Y to make them equal
y_center = scale(y, scale=FALSE)
y_var = sqrt(sum(y_center^2)/dim(y_center)[1])
y_stand = y_center/y_var
model_ridge = glmnet(data, y_stand, lambda=1, alpha=.5, intercept=FALSE, standardize = FALSE, family='gaussian', thresh=1e-16)
model_ridge$beta 

pen_mod = penalized(y_stand, data, lambda1=.5*dim(data)[1], lambda2 = .5*dim(data)[1], unpenalized = ~0)
coefficients(pen_mod)

#CV Lasso
model_cv = cv.glmnet(data, y, family="gaussian", alpha=1, intercept=FALSE)
lam.index = which(model_cv$lambda == model_cv$lambda.1se)
model_cv$glmnet.fit$beta[,lam.index ]
model_cv$lambda.1se
model_cv$lambda.min
py_lam = model_cv$lambda.1se*dim(data)[1]
plot(model_cv)
predict(model_cv, newx = data, s = "lambda.min")
