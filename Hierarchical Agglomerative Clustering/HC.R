# TASK 1
# Hierarchical Agglomerative Clustering
#
# euclidean distance function
distance <- function(ds) {
  
  euclidean.distance<-matrix(0,nrow(ds),nrow(ds))
  for (i in 1:nrow(ds)) {
    x<-ds[i,]
    for (j in 1:nrow(ds)) {
      y<-ds[j,]
      res<-sqrt(sum((x-y)^2))
      euclidean.distance[j,i]<-res
    }
  }
  return(data.frame(euclidean.distance))
}
# hierarchical agglomerative clustering function
hierarchical.agglomerative <- function(x,method="single",lab=rownames(x)) {
  
  x<-as.matrix(x)
  n<-nrow(x)
  diag(x)<-Inf
  level<-matrix(0,n-1,2)
  heights<-rep(0,n-1)
  leaf<- -1:-n
  for (i in 1:(n-1)) {
    heights[i]<-min(x)
    d<-which(x-heights[i]==0,arr.ind=TRUE)
    d<-d[1,,drop=FALSE]
    temp<-leaf[d]
    temp<-temp[order(temp)]
    level[i,]<-temp
    clusters<-c(d,which(leaf%in%leaf[d[1,leaf[d]>0]]))
    leaf[clusters]<-i
  
    if (method=="complete") {
      linkage<-apply(x[d,],2,max)
    } else if (method=="average") {
      linkage<-apply(x[d,],2,mean)
    } else {
      linkage<-apply(x[d,],2,min)
    }
    x[min(d),]<-linkage
    x[,min(d)]<-linkage
    x[min(d),min(d)]<-Inf
    x[max(d),]<-Inf
    x[,max(d)]<-Inf
  }
  
  order.levels <- function(m) {
    n<-nrow(m)+1
    ord<-rep(0,n)
    ord[1]<-m[n-1,1]
    b<-2
    ord[2]<-m[n-1,2]
    for(i in seq(n-2,1)) {
      for(j in seq(1,b)) {
        if(ord[j]==i) {
          ord[j]<-m[i,1]
          if(j==b) {
            b<-b+1
            ord[b]<-m[i,2]
          } else {
            b<-b+1
            for(k in seq(b, j+2)) {
              ord[k] = ord[k-1]
            }
            ord[j+1] = m[i,2]
          }
        }
      }
    }
    return(-ord)
  }
  
  structure(
    list(
      merge=level,height=heights,order=order.levels(level),
      labels=lab,method=method,call=match.call(),
      dist.method="euclidean"
      ),
    class="hclust"
    )
}