####
#### Task 2
####

library(ISLR)
attach(Auto)
high <- ifelse(mpg>=23,1,0) #label high
origin.1 <- ifelse(origin==1,1,0) #dummy variable 1
origin.2 <- ifelse(origin==2,1,0) #dummy variable 2
origin.3 <- ifelse(origin==3,1,0) #dummy variable 3
Auto<-data.frame(Auto,origin.1,origin.2,origin.3)
Auto <- scale(Auto[,c("horsepower","weight","year","origin.1","origin.2","origin.3")])
Auto <- data.frame(Auto,high)
### dataframe Auto is normalized with mean 0 and variance 1