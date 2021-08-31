####
#### Task 5
####

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
  nll <- c()
  
  for (i in 1:iterations) {
    
    nll <- c(nll,NLL(x,y,w,b))
    
    if ((i>10)&&(abs(((nll[i]-nll[i-10])/nll[i])*100)<1.0)) {
      break
    } else {}
    
    #gradient
    sig <- sigmoid(w*(x)+b)
    delta.w <- (1/nrow(x))*((x)*(sig-(y)))
    delta.b <- (1/nrow(x))*(sum(sig-(y)))
    
    w <- w-(eta*delta.w)
    b <- b-(eta*delta.b)
  }
  return(tail(nll))
}
set.seed(0329)
weights <- runif(6,-0.7,0.7)
a<-gradient.descent(train.X,train.Y,weights,eta=0.1,iterations=1000)
print(a)