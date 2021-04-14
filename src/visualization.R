library(arrow)
library(dplyr)

# __________________________ Original ________________________
vTrain <- read_parquet("data/processed/train.parquet")
nfeature <- dim(vTrain)[[2]]

varn <- names(vTrain)
intCols <- match(names(Filter(is.numeric,vTrain)), varn)
facCols <- match(names(Filter(is.factor,vTrain)), varn)

# Plot  integer features
par(mfrow=c(6,6))
for(ind in intCols) { 
  par(mar = c(2,2,2,2))
  plot(vTrain[,ind],vTrain$SalePrice, 
       ylab ="Sale Price",
       main =varn[ind]) 
}

# Plot factor features
par(mfrow=c(6,7))
for(ind in facCols) { 
  par(mar = c(2,2,2,2))
  plot(vTrain[,ind],vTrain$SalePrice, 
       ylab ="Sale Price",
       main =varn[ind]) 
}

# _________________________ Engineered ________________________

tTrain <- read_parquet("data/processed/transformed_train.parquet")
nfeature <- dim(tTrain)[[2]]

varn <- names(tTrain)
intCols <- match(names(Filter(is.numeric,tTrain)), varn)
facCols <- match(names(Filter(is.factor,tTrain)), varn)

# Plot  integer features
par(mfrow=c(7,7))
for(ind in intCols) { 
  par(mar = c(2,2,2,2))
  plot(tTrain[,ind],tTrain$SalePrice, 
       ylab ="Sale Price",
       main =varn[ind]) 
}

# Plot factor features
par(mfrow=c(7,8))
for(ind in facCols) { 
  par(mar = c(2,2,2,2))
  plot(tTrain[,ind],tTrain$SalePrice, 
       ylab ="Sale Price",
       main =varn[ind]) 
}

# ________________ Visualize Log Transformations ______________________
toLog <- c("LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF",
           "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", 
           "GrLivArea", "GarageArea", "WoodDeckSF", "OpenPorchSF", 
           "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "MiscVal")
# Before
par(mfrow=c(5,4))
for(name in toLog) { 
  par(mar = c(2,2,2,2))
  plot(vTrain[[name]],vTrain$SalePrice, 
       ylab ="Sale Price",
       main =name) 
}

# after
par(mfrow=c(5,4))
for(name in toLog) { 
  par(mar = c(2,2,2,2))
  plot(tTrain[[name]],tTrain$SalePrice, 
       ylab ="Sale Price",
       main =name) 
}