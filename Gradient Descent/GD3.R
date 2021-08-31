####
#### Task 3
####

set.seed(0329)
train <- sample(1:392,196)
training <- Auto[train,]
test <- Auto[-train,]
### training and test split of
### Auto dataset in 2 equal parts