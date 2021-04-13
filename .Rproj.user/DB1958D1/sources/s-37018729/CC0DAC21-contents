library(arrow)
library(dplyr)

# __________________________ Original ________________________
train <- read_parquet("data/processed/train.parquet")
nfeature <- dim(train)[[2]]

varn <- names(train)
intCols <- match(names(Filter(is.numeric,train)), varn)
facCols <- match(names(Filter(is.factor,train)), varn)

# Plot  integer features
par(mfrow=c(6,6))
for(ind in intCols) { 
  par(mar = c(2,2,2,2))
  plot(train[,ind],train$SalePrice, 
       ylab="Sale Price",
       xlab="letter", 
       main =varn[ind]) 
}

# Plot factor features
par(mfrow=c(6,7))
for(ind in facCols) { 
  par(mar = c(2,2,2,2))
  plot(train[,ind],train$SalePrice, 
       ylab="Sale Price",
       xlab="letter", 
       main =varn[ind]) 
}

# _________________________ Engineered ________________________

tTrain <- read_parquet("data/processed/transformed_train.parquet")
nfeature <- dim(tTrain)[[2]]

varn <- names(tTrain)
intCols <- match(names(Filter(is.numeric,tTrain)), varn)
facCols <- match(names(Filter(is.factor,tTrain)), varn)

# Plot  integer features
par(mfrow=c(6,7))
for(ind in intCols) { 
  par(mar = c(2,2,2,2))
  plot(tTrain[,ind],tTrain$SalePrice, 
       ylab="Sale Price",
       xlab="letter", 
       main =varn[ind]) 
}

# Plot factor features
par(mfrow=c(7,8))
for(ind in facCols) { 
  par(mar = c(2,2,2,2))
  plot(tTrain[,ind],tTrain$SalePrice, 
       ylab="Sale Price",
       xlab="letter", 
       main =varn[ind]) 
}
