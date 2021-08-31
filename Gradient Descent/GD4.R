####
#### Task 4
####

train.X<-training[c("horsepower","weight","year","origin.1","origin.2","origin.3")]
train.Y<-training$high
test.X<-test[c("horsepower","weight","year","origin.1","origin.2","origin.3")]
test.Y<-test$high
weights <- runif(6,-0.7,0.7)
# training using the training set : train.X and train.Y
print(gradient.descent(train.X,train.Y,weights,b=0,eta=0.3,iterations=1500))
# finding Negative Log Likelihood on test test
# test.X and test.Y
print(gradient.descent(test.X,test.Y,weights,b=0,eta=0.3,iterations=1500))