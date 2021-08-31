####
#### Task 6
####
### Modified gradient.descent for task 6

sigmoid <- function(x) {
  
  return (1/(1+exp(-x)))
}

NLL <- function(x,y,w,b) {
  
  sig <- sigmoid(w*(x)+b)
  nll <- (-1/nrow(x))*(sum(((y)*log(sig))+((1-(y))*log(1-sig))))
  return(nll)
}

gradient.descent1<-function(x,y,w,b=0,eta=0.1,iterations=100) {
  
  y <- matrix(y)
  negative.log.likelihood <- c()
  
  for (i in 1:iterations) {
    
    negative.log.likelihood <- c(negative.log.likelihood,NLL(x,y,w,b))
    
    #gradient
    sig <- sigmoid(w*(x)+b)
    delta.w <- (1/nrow(x))*((x)*(sig-(y)))
    delta.b <- (1/nrow(x))*(sum(sig-(y)))
    
    w <- w-(eta*delta.w)
    b <- b-(eta*delta.b)
  }
  return(negative.log.likelihood)
}
set.seed(0329)
weights <- runif(6,-0.7,0.7)
Y.axis<-gradient.descent1(test.X,test.Y,weights,b=0,eta=0.5,iterations=100)
boxplot(Y.axis,ylab="Negative log-likelihood",main="Boxplot of NLL")
print(Y.axis)
print(mean(Y.axis))