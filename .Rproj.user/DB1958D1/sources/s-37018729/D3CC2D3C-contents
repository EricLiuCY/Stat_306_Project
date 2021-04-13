library(arrow)
library(dplyr)

# __________________________ FUNCTIONS ______________________________
addRatios <- function(df) {
  newDf <- df
  # New features
  # UnfinishedRatio: BsmtUnfSF / TotalBsmtSF
  # GaragePerCar : GarageCars / GarageArea
  # X2ndFlrRatio : X2ndFlrSF / GrLivArea
  # GrLivAreaRatio : GrLivArea / LotArea
  newDf$UnfinishedRatio <- newDf$BsmtUnfSF / newDf$TotalBsmtSF
  newDf$GaragePerCar <- newDf$GarageArea / newDf$GarageCars
  newDf$X2ndFlrRatio <- newDf$X2ndFlrSF / newDf$GrLivArea
  newDf$GrLivAreaRatio <- newDf$GrLivArea / newDf$LotArea
  
  # bedrooms per bath
  newDf$BedPerBath <- newDf$BedroomAbvGr / newDf$FullBath
  newDf$BedPerBath[newDf$BedPerBath == Inf] <- 0
  
  newDf$BedPerBathAbv <- newDf$BedroomAbvGr / (newDf$FullBath + newDf$HalfBath)
  newDf$BedPerBathAbv[newDf$BedPerBathAbv == Inf] <- 0
  
  # Total Baths and Beds Per Bath
  newDf$totalBath <- newDf$FullBath + newDf$HalfBath + newDf$BsmtHalfBath + newDf$BsmtFullBath
  
  newDf$BedPerTotalBath <- newDf$BedroomAbvGr / newDf$totalBath
  newDf$BedPerTotalBath[newDf$BedPerTotalBath == Inf] <- 0
  
  # Sqft Per Bath
  newDf$BathsPerLivAbv <- newDf$GrLivArea / newDf$BedPerTotalBath
  newDf$BathsPerLivAbv[newDf$BathsPerLivAbv == Inf] <- 0
  
  return(newDf)
}

# Add logVal
addlogVal <- function(df, varn) {
  newDf <- df
  for (name in varn) {
    newDf[name] <- log(newDf[name] + 1)
  }
  return(newDf)
}

# Add Existence Binary of factor True False
addHasFeat <- function(df, varn) {
  newDf <- df
  for (name in varn) {
    newName <- paste("has", name, sep="")
    newDf[newName] <- as.factor(newDf[name] > 0)
  }
  return(newDf)
}

createTransformedTrain <- function() {
  train <- read_parquet("data/processed/train.parquet")
  
  # Add new ratio covariates
  train <- addRatios(train)
  
  # Log SF and MiscVal covariates
  toLog <- c("LotArea", "MasVnrArea", "BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF",
             "TotalBsmtSF", "X1stFlrSF", "X2ndFlrSF", "LowQualFinSF", 
             "GrLivArea", "GarageArea", "WoodDeckSF", "OpenPorchSF", 
             "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "MiscVal")
  train <- addlogVal(train, toLog)
  
  # Add Has Feature covariates
  toExist <- c("X2ndFlrSF", "LowQualFinSF", "WoodDeckSF", "OpenPorchSF", 
               "EnclosedPorch", "X3SsnPorch", "ScreenPorch", "MiscVal", 
               "PoolArea", "BedroomAbvGr")
  train <- addHasFeat(train, toExist)
  
  write_parquet(train, sink = "data/processed/transformed_train.parquet")
  
  return(train)
}

# __________________________ SCRIPTS ______________________________

createTransformedTrain()
