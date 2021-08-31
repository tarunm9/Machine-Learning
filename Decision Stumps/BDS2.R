###
### Task 2
###
library(MASS)
set.seed(0329)
n<-nrow(Boston)
train<-sample(1:n,n/2)
training<-Boston[train,c("lstat","rm","medv")]
test<-Boston[-train,c("medv")]
r<-training$medv
eta<-0.01

boosted.decision.stump(training,r,test,eta,1000,"lstat","rm")