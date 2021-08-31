# loading NCI microarray dataset from ISLR
library(ISLR)
NCI<-NCI60$data
Label<-NCI60$labs

# TASK 2 and TASK 3 (HAC on NCI microarray dataset)
for (i in 1:length(Label)) {
  q<-toString(i)
  Label[i]<-paste(Label[i],q)
}
dist.NCI<-distance(NCI) # dist()
single.link<-hierarchical.agglomerative(distance(NCI),"single",lab=Label)
plot(single.link,cex=0.7)
complete.link<-hierarchical.agglomerative(distance(NCI),"complete",lab=Label)
plot(complete.link,cex=0.7)
average.link<-hierarchical.agglomerative(distance(NCI),"average",lab=Label)
plot(average.link,cex=0.7)

# TASK 4 (k-means on NCI microarray dataset)
disp_k<-c(3,7,14) # to display the clusters for k=(3,7,14)
for (i in disp_k)
{
  km.nci<-kmeans(NCI,i)
  print("For K =")
  print(i)
  print(km.nci$cluster)
  
}
y_axis<-c() # to plot the % fit for k=1:63
for (i in 1:(nrow(NCI)-1))
{
  km.nci<-kmeans(NCI,i)
  within_cluster_sos<-(km.nci$betweenss/km.nci$totss)*100
  y_axis<-c(y_axis,within_cluster_sos)
  
}
plot(1:(nrow(NCI)-1),y_axis,type="l",col="red",lwd=5,
     xlab="k value",
     ylab="Within Cluster Sum of Squares (% fit)",
     main="% for differnt k values")

# TASK 6 (finding optimal k using Silhouette method)
library(factoextra)
library(NbClust)
library(ggplot2)
fviz_nbclust(NCI, kmeans, method = "silhouette",
             k.max = 63) + theme_minimal() +
  ggtitle("Silhouette Method to find optimal k value")

