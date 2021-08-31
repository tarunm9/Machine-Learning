####
#### Task 7
####
set.seed(0329)
nll <- c()
best.nll <- 100
best.wts <- c()
for (i in 1:4) {
  weights <- rnorm(6)
  nll<-c(nll,gradient.descent(train.X,train.Y,weights,b=0,eta=0.2,iterations=2000))
  if (nll<best.nll){
    best.nll <- nll
    best.wts <- weights
  }
}
print("The Best NLL ")
print(best.nll)
print("is achieved by the weights: ")
print(best.wts)