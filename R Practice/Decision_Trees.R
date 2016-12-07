###Set working directory
setwd("C:/Users/smcdo/OneDrive/Documents/Model_Framework/Benchmarks")

###Read x and y data
data = read.csv("x_data.csv", row.names=1)
y = read.csv("y_data.csv", row.names=1)
data = as.matrix(data)
y = as.matrix(y)

######Decision Trees
library(rpart)
reg_tree = rpart(y~data, control=c(maxdepth=3, minsplit=20))
plot(reg_tree)
text(reg_tree, use.n=TRUE, all=TRUE)
