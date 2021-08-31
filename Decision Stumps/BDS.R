###
### Boosted Decision Stump for two predictor attributes
###
decision.stump <- function(train,r,test,a1,a2) {
  
  split.region <- function(s,train,r,a) {
    
    sum.lt<-sum.gt<-0
    n.lt<-n.gt<-0
    mean.Y.lt<-mean.Y.gt<-0
    
    for (i in 1:nrow(train))
    {
      if (train[i,c(a)]<s)
      {
        sum.lt<-sum.lt+r[i]
        n.lt<-n.lt+1
      }
      if (train[i,c(a)]>s)
      {
        sum.gt<-sum.gt+r[i]
        n.gt<-n.gt+1
      }
    }
    
    mean.Y.lt<-sum.lt/n.lt
    mean.Y.gt<-sum.gt/n.gt
    
    sum.lt.sqr<-0
    sum.gt.sqr<-0
    
    for(i in 1:nrow(train))
    {
      if (train[i,c(a)]<s)
      {
        sum.lt.sqr<-sum.lt.sqr+(r[i]-mean.Y.lt)^2
      }
      if (train[i,c(a)]>s)
      {
        sum.gt.sqr<-sum.gt.sqr+(r[i]-mean.Y.gt)^2
      }
    }
    
    RSS<-sum.lt.sqr+sum.gt.sqr
    return(RSS)
  }
  
  RSS.a1<-matrix(nrow=nrow(train),ncol=1,byrow=TRUE)
  RSS.a2<-matrix(nrow=nrow(train),ncol=1,byrow=TRUE)
  
  for (i in 1:nrow(train))
  {
    S.a1<-train[i,c(a1)]
    RSS.a1[i]<-split.region(S.a1,train,r,a1)
    
    S.a2<-train[i,c(a2)]
    RSS.a2[i]<-split.region(S.a2,train,r,a2)
  }
  
  to.boost<-c()
  
  if (min(RSS.a1)>min(RSS.a2))
  {
    S.i<-which.min(RSS.a2)
    S<-train[S.i,c(a2)]
    
    sum.lt<-sum.gt<-0
    n.lt<-n.gt<-0
    mean.Y.lt<-mean.Y.gt<-0
    
    for (i in 1:nrow(train))
    {
      if (train[i,c(a2)]<S)
      {
        sum.lt<-sum.lt+r[i]
        n.lt<-n.lt+1
      }
      if (train[i,c(a2)]>S)
      {
        sum.gt<-sum.gt+r[i]
        n.gt<-n.gt+1
      }
      
    }
    mean.Y.lt<-sum.lt/n.lt
    mean.Y.gt<-sum.gt/n.gt
    
    to.boost<-c(a2,S,mean.Y.lt,mean.Y.gt)
    return(to.boost)
  }  else  {
    S.i<-which.min(RSS.a1)
    S<-train[S.i,c(a1)]
    
    sum.lt<-sum.gt<-0
    n.lt<-n.gt<-0
    mean.Y.lt<-mean.Y.gt<-0
    
    for (i in 1:nrow(train))
    {
      if (train[i,c(a1)]<S)
      {
        sum.lt<-sum.lt+r[i]
        n.lt<-n.lt+1
      }
      if (train[i,c(a1)]>S)
      {
        sum.gt<-sum.gt+r[i]
        n.gt<-n.gt+1
      }
    }
    mean.Y.lt<-sum.lt/n.lt
    mean.Y.gt<-sum.gt/n.gt
    
    to.boost<-c(a1,S,mean.Y.lt,mean.Y.gt)
    return(to.boost)
  }
  
}

boosted.decision.stump <- function(train,r,test,eta,B,a1,a2) {
  
  d.stump<-matrix(nrow=B,ncol=4,byrow=TRUE)
  f.hat<-matrix(nrow=B,ncol=nrow(train),byrow=TRUE)
  
  for (i in 1:B)
  {
    d.stump[i,]<-decision.stump(train,r,test,a1,a2)
    
    for (j in 1:nrow(train))
    {
      a<-d.stump[i,1]
      S<-as.numeric(d.stump[i,2])
      mean.lt<-as.numeric(d.stump[i,3])
      mean.gt<-as.numeric(d.stump[i,4])
      
      if (train[j,c(a)]<S)
      {
        f.hat[i,j]<-mean.lt
      }
      else
      {
        f.hat[i,j]<-mean.gt
      }
      r[j]<-r[j]-eta*f.hat[i,j]
    }
  }
  
  sqr.diff<-0
  
  for (i in 1:length(test))
  {
    predict.Rule<-sum(eta*f.hat[,i])
    sqr.diff<-sqr.diff+(test[i]-(predict.Rule))^2
  }
  mean.squared.error<-sqr.diff/length(test)
  
  print(c("Test MSE for Boosted Decision Stump:",mean.squared.error))
  #return(mean.squared.error) #used for Task 3
}