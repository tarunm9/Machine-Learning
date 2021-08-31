###
### Task 1
###
library(MASS)
set.seed(0329)
n<-nrow(Boston)
train<-sample(1:n,n/2)
training<-Boston[train,c("lstat","rm","medv")]
test<-Boston[-train,c("medv")]
r<-training$medv

#decision.stump(training,r,test,"lstat","rm")
decision.stump(Boston[-train,c("lstat","rm","medv")],r,test,"lstat","rm")