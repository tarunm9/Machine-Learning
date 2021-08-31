####
#### Task 1
####
### Logistic Regression using Gradient Descent with
### Negative Log Likelihood as the objective function.

sigmoid <- function(x) {
  
  return (1/(1+exp(-x)))
}

NLL <- function(x,y,w,b) {
  
  sig <- sigmoid(w*(x)+b)
  nll <- (-1/nrow(x))*(sum(((y)*log(sig))+((1-(y))*log(1-sig))))
  return(nll)
}

gradient.descent<-function(x,y,w,b=0,eta=0.1,iterations=1000) {
  
  y <- matrix(y)
  
  for (i in 1:iterations) {
    
    negative.log.likelihood <- NLL(x,y,w,b)
    
    #gradient
    sig <- sigmoid(w*(x)+b)
    delta.w <- (1/nrow(x))*((x)*(sig-(y)))
    delta.b <- (1/nrow(x))*(sum(sig-(y)))

    w <- w-(eta*delta.w)
    b <- b-(eta*delta.b)
    
  }
  #print(w[nrow(w),]) #commented this for task 7
  #print(b)           #commented this for task 7
  return(negative.log.likelihood)
}