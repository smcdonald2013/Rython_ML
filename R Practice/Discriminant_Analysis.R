###Set working directory
setwd("C:/Users/smcdo/OneDrive/Documents/Model_Framework/Benchmarks")

###Read x and y data
data = read.csv("x_data.csv", row.names=1)
y = read.csv("y_data.csv", row.names=1)
data = as.matrix(data)
y = as.matrix(y)

####Converting y to binary response
y_cat = y
y_cat[y_cat < 0] <- 0
y_cat[y_cat > 0] <- 1

library(MASS)
###LDA, no CV
fit <- lda(y_cat ~ data, CV=FALSE)
summary(fit)

###LDA, with CV
fit <- lda(y_cat ~ data, CV=TRUE)
summary(fit)

###QDA, no CV 
fit <- qda(y_cat ~ data, CV=FALSE)
summary(fit)

###QDA, with CV
fit <- qda(y_cat ~ data, CV=TRUE)
summary(fit)
