###
### Decision Stump for two predictor attributes
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
        sum.lt=sum.lt+r[i]
        n.lt=n.lt+1
      }
      if (train[i,c(a)]>s)
      {
        sum.gt=sum.gt+r[i]
        n.gt=n.gt+1
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
  
  if (min(RSS.a1)>min(RSS.a2))
  {
    a<-a2
    mse<-min(RSS.a2)/length(test)
  }
  else{
    a<-a1
    mse<-min(RSS.a1)/length(test)
  }
  print(c("The attribute with min RSS is:",a))
  print(c("Test MSE for Decision Stump:",mse))
}
