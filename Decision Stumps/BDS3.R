###
### Task 3
###
### remove '#' in line 158 of BDS.R
###
library(MASS)
set.seed(0329)
n<-nrow(Boston)
train<-sample(1:n,n/2)
training<-Boston[train,c("lstat","rm","medv")]
test<-Boston[-train,c("medv")]
r<-training$medv
eta<-0.5
itr<-1:10
mse<-c()
for (i in 1:10)
{
  mse<-c(mse,boosted.decision.stump(training,r,test,eta,i,"lstat","rm"))
}

plot(itr, mse, type = "b", pch = 16, xlab="Number of trees (B)", ylab="Mean Squared Error", main="MSE for eta=0.5 and range of B", col="red")